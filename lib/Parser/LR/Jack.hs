{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, QuasiQuotes
  , RecordWildCards
  , MultiParamTypeClasses
  , FlexibleContexts
  #-}
module Parser.LR.Jack () where

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

instance LRPoint p => ParserWriter (LRParser p) Jack where
  --writeParser :: Proxy lang -> Text -> ParserOptions a -> parser -> [(FilePath,Text)]
  writeParser _ _gtop ParserOptions{..} LRParser{..} = [
    ("Parser.jack", [interp|
/** LR-based parser */
class #{name} {
  /** Action and GOTO tables as static arrays */
  static Array action, goto;
  /** Widths of corresponding tables */
  static int action_w, goto_w;
  /** Lexer */
  field Lexer lex;
  /** State stack */
  field Stack stack;

  /** Constructs new parser using lexer i_lex */
  constructor #{name} new(Lexer i_lex) {
    let lex = i_lex;
    let stack = Stack.new();
    return this;
  }

  /** Dispose */
  method void dispose() {
    do cleanup();
    do stack.dispose();
    do Memory.deAlloc(this);
    return;
  }

  /** Helper function to clean-up the contents of the stack */
  method void cleanup() {
    var Token t;
    while(~(stack.top() = null)) {
      let t = stack.pop();
      do t.dispose();
    }
    return;
  }

  /** Helper function to get item identifier on top of the stack */
  method int top() {
    var Token t;
    let t = stack.top();
    if(t = null) {
      return 0;
    } else {
      return t.id();
    }
  }

  /** Main parsing method
    * Returns arbitrary value defined by the grammar's semantic rules.
    * Return type represented by int, in C that would be void*.
    * On error, returns null.
    */
  method int parse() {
    var Token a, t, _1, _2, _3, _4, _5, _6, _7, _8, _9;
    var int act, gt, ti;
    let a = lex.getNextToken();
    if(a=null) { do cleanup(); return null; }
    while(true) {
      let act = action[(top()*action_w)+a.id()];
      #{indent 3 $ elsify (actionCases ++ [shiftAction])}
    }
    do Sys.error("Internal parser error: unreachable code reached");
    return null; // never reached
  }


  /** This function initializes Action and GOTO tables.
    * It must be called before method parse() is used
    * It should be called exactly once.
    */
  function void init() {
    let action = Array.new(#{actionTableSize});
    let action_w = #{length tokens};
    let goto = Array.new(#{gotoTableSize});
    let goto_w = #{length nonTerminals};
    #{indent 2 actionTable}
    #{indent 2 gotoTable}
    return;
  }
}
|])]
    where
    shiftAction = [interp|
      // shift next token
      // note: we're reusing Token class for pushing states on stack
      // this is only because Token is basically a generic Pair class
      do stack.push(Token.new(act, a.dataPtr()));
      do a.dispose();
      let a=lex.getNextToken();
      if(a=null) { do cleanup(); return null; }
      |] :: Text
    elsify :: [Text] -> Text
    elsify [x,y] =  [interp|
      #{x} else { #{y} }
      |]
    elsify (x:xs) = [interp|
      #{x} else { #{elsify xs} }
      |]
    elsify [] = ""
    actionTableSize = length states * length tokens
    gotoTableSize = length states * length nonTerminals
    -- base = parserOptionsBaseFileName
    name = parserOptionsName
    tokens = lrTerminals
    states = lrStates
    nonTerminals = lrNonTerminals
    actionTable = T.intercalate "\n" . zipWith (set "action") [(0::Word)..] $ concat actionTableRaw
    gotoTable = T.intercalate "\n" . zipWith (set "goto") [(0::Word)..] $ concat gotoTableRaw
    set :: Text -> Word -> Word -> Text
    set what n a = [interp|let #{what}[#{n}] = #{a};|]
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
    actionCases = mapMaybe writeAction $ M.toList actionsMap
    writeAction (Shift _, _) = Nothing
    writeAction (a, n) = Just [interp|
    if (act = #{tshow n}) {
      #{indent 1 $ actionBody a}
    }|] :: Maybe Text
    actionBody Reject = [interp|
      /* Rejection */
      do Output.printString("Invalid input");
      do Util.newline();
      do a.dispose();
      do cleanup();
      return null;
      |] :: Text
    actionBody (Shift _) = error "does not happen"
    actionBody (Reduce ((ExtendedStartRule, _), _)) = [interp|
      /* Acceptance */
      let t = stack.pop();
      do t.dispose();
      let t = stack.pop();
      let ti = t.dataPtr();
      do t.dispose();
      do a.dispose();
      do cleanup();
      return ti;
      |]
    actionBody (Reduce ((h, body), mcode)) = [interp|
      /* Reduction: #{showBody body} => #{h} */
      #{T.intercalate "\n" (reverse $ zipWith showArg body [1::Word ..])}
      let gt = goto[(top()*goto_w) + #{tshow (nonTermIdx (NonTerm h))}]; // #{h}
      if (gt=0) { do Sys.error("No goto"); }
      do stack.push(Token.new(gt,(#{result})));
      |]
      where
        result :: Text
        result
          | Just code <- mcode
          = T.strip code
          | otherwise
          = "null"
        showArg _ i = [interp|
          let t = stack.pop();
          let _#{i} = t.dataPtr();
          do t.dispose();
          |]
    nonTermIdx nt = fromJust $ M.lookup nt nonTerminalsMap
    nonTerminalsMap = M.fromList $ zip nonTerminals [0::Word ..]
    indent = indentLang 2
