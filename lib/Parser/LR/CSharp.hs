{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, QuasiQuotes
  , RecordWildCards
  , MultiParamTypeClasses
  , FlexibleContexts
  #-}
module Parser.LR.CSharp () where

import Parser.LR.Build
import Lang

import Grammar hiding (first)
import qualified Data.Map as M
import Data.Maybe
import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as T
import Utils
import Parser.LR.Point
import Parser.Types
import Control.Arrow

instance LRPoint p => ParserWriter (LRParser p) CSharp where
  --writeParser :: Proxy lang -> Text -> ParserOptions a -> parser -> [(FilePath,Text)]
  writeParser _ gtop ParserOptions{..} LRParser{..} = [
      (base <> ".cs", [interp|
using lexer;
using System;
using System.Collections.Generic;
#{topTop gtop}
namespace parser {
public class #{className}#{topInh gtop} {
  private readonly Lexer lex;
  private readonly bool debug;
  private Stack<(uint state, dynamic value)> stack = new Stack<(uint state, dynamic value)>();
  private static uint[,] Action = new uint[,] {
    #{indent 2 actionTable}
  };
  private static uint[,] GOTO = new uint[,] {
    #{indent 2 gotoTable}
  };
  private uint top() {
    return stack.Count == 0 ? 0 : stack.Peek().state;
  }
  static string[] stateNames = new string[] {#{stateToString}};
  static string[] expectedSyms = new string[] {#{expectedSym}};

  public #{className}(Lexer lex, bool debug = false) {
    this.lex = lex;
    this.debug = debug;
  }
  public dynamic parse() {
    var a = lex.getNextToken();
    while (true) {
      var action = Action[top(), (int)a.type];
      switch (action) {
      #{indent 3 actionCases}
      default:
        if(debug) Console.Error.WriteLine($"Shift to {action}");
        stack.Push((action, a));
        a=lex.getNextToken();
        break;
      }
    }
  }
}
}
|])]
    where
    indent = indentLang 2
    base = parserOptionsBaseFileName
    className = parserOptionsName
    tokens = lrTerminals
    states = lrStates
    nonTerminals = lrNonTerminals
    braces :: Text -> Text
    braces x = "{"<>x<>"}"
    actionTable = T.intercalate ",\n" $ map (braces . T.intercalate "," . map tshow) actionTableRaw
    gotoTable = T.intercalate ",\n" $ map (braces . T.intercalate "," . map tshow) gotoTableRaw
    (actionTableRaw, (actionsMap, _)) = runState (mapM (forM tokens . actionCell) states) (M.empty, fromIntegral $ length states)
    gotoTableRaw = map (\st -> map (\nt -> fromJust $ M.lookup (st, nt) lrGoto) nonTerminals) states
    actionCell st tok = do
      let Just act = M.lookup (st, tok) lrAction
      mi <- gets (M.lookup act . fst)
      case mi of
        Just i -> return i
        Nothing -> case act of
          Shift i -> do
            modify (first $ M.insert act i)
            return i
          _ -> do
            i <- gets snd
            modify (M.insert act i *** (+1))
            return i
    stateToString = T.intercalate "," $ "\"·\"" : map (quote . showSymbol) (M.elems lrStateSym)
    expectedSym = T.intercalate "," $ map (quote . T.intercalate "/") $ M.elems lrExpected
    actionCases = T.intercalate "\n" $ mapMaybe writeAction $ M.toList actionsMap
    writeAction (Shift _, _) = Nothing
    writeAction (a, n) = Just [interp|
    case #{n}: {
        #{indent 2 $ actionBody a}
      }
    |] :: Maybe Text
    actionBody Reject = [interp|
      string parsed=stateNames[top()];
      var lastSt = top();
      while(stack.Count > 0) { stack.Pop(); parsed = stateNames[top()] + " " + parsed; }
      throw new ApplicationException(
        $"Rejection state reached after parsing \\"{parsed}\\", when encoutered symbol \\""
        + $"\\"{a.type}\\" in state {lastSt}. Expected \\"{expectedSyms[lastSt]}\\"");
      |] :: Text
    actionBody (Shift _) = error "does not happen"
    actionBody (Reduce ((ExtendedStartRule, _), _)) = [interp|
      stack.Pop();
      return stack.Pop().value;
      |]
    actionBody (Reduce ((h, body), mcode)) = [interp|
      if(debug) Console.Error.WriteLine("Reduce using #{h} -> #{showBody body}");
      #{T.intercalate "\n" (reverse $ zipWith showArg body [1::Word ..])}
      var gt = GOTO[top(), #{tshow (nonTermIdx (NonTerm h))} /*#{h}*/];
      if(gt==0) throw new ApplicationException("No goto");
      if(debug) {
        Console.Error.WriteLine($"{top()} is now on top of the stack;");
        Console.Error.WriteLine($"{gt} will be placed on the stack");
      }
      stack.Push((gt,(#{result})));
      break;
      |]
      where
        result :: Text
        result
          | Just code <- mcode
          = T.strip code
          | otherwise
          = "null"
        showArg (NonTerm _) i =
          [interp|dynamic _#{i}=stack.Pop().value;|]
        showArg _ i =
          [interp|(TokenType name, dynamic attr) _#{i}=stack.Pop().value;|]
    nonTermIdx nt = fromJust $ M.lookup nt nonTerminalsMap
    nonTerminalsMap = M.fromList $ zip nonTerminals [0::Word ..]
    quote x = "\"" <> x <> "\""
