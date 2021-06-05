{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, QuasiQuotes
  , RecordWildCards
  , MultiParamTypeClasses
  , FlexibleContexts
  #-}
module Parser.LL.Langs.CSharp () where

import Parser.Types
import Utils
import Data.Maybe
import Data.List
import Parser.LL.Build
import Data.Text (Text)
import Grammar
import qualified Data.Text as T
import qualified Data.Map as M

instance ParserWriter LLParser CSharp where
  -- writeParser :: Proxy lang -> Text -> ParserOptions a -> parser -> [(FilePath,Text)]
  writeParser _ gtop ParserOptions{..} p@LLParser{..} = [
      (basename <> ".cs", sourceFile)
    ]
    where
    basename = parserOptionsBaseFileName
    sourceFile = [interp|
using lexer;
using System;
using System.Collections.Generic;
#{topTop gtop}
namespace parser {
public class #{className}#{topInh gtop} {
  public enum NonTerminal : uint { #{T.intercalate ", " (map ("NT_" <>) nonTerms)} };
  private readonly Lexer lex;
  private readonly bool debug;
  private Stack<dynamic> stack = new Stack<dynamic>();
  private Stack<dynamic> resultStack = new Stack<dynamic>();
  private static uint[,] M = new uint[,] {
    #{indent 2 $ T.intercalate ",\n" $ map (braces . T.intercalate "," . map showIdx') transTable}
  };
  public #{className}(Lexer lex, bool debug = false) {
    this.lex = lex;
    this.debug = debug;
  }
  public dynamic parse() {
    stack.Push(#{encodeSymbol llStartSymbol});
    var a = lex.getNextToken();
    while (stack.Count > 0) {
      var x = stack.Pop();
      switch (x) {
      case TokenType X:
        if (a.type == X) {
          resultStack.Push(a);
          a = lex.getNextToken();
        } else {
          throw new ApplicationException($"Found terminal {a.type} but expected {X}.");
        }
        break;
      case NonTerminal X:
        var trans = M[(int)X, (int)a.type];
        stack.Push(trans);
        switch(trans) {
        case 0: throw new ApplicationException($"No transition for {X}, {a.type}");
        #{indent 4 bodies}
        }
        break;
      case uint X:
        switch(X) {
        #{indent 4 actions}
        }
        break;
      }
    }
    return resultStack.Pop();
  }
}
}
|]
    indent = indentLang 2
    className = parserOptionsName
    (transTable, actionMap) = indexTable p
    nonTerms = mapMaybe getNt llNonTerminals
    getNt (NonTerm x) = Just x
    getNt _ = Nothing
    showIdx' Nothing = "0"
    showIdx' (Just x) = showIdx x
    showIdx x = tshow (x+1)
    braces :: Text -> Text
    braces x = "{"<>x<>"}"
    actionList = sortOn snd $ M.toList actionMap
    bodies = T.intercalate "\n" $ map (uncurry makeBody) actionList
    actions = T.intercalate "\n" $ map (uncurry makeAction) actionList
    makeBody ((NonTerm nt, b), _) n = [interp|
    case #{showIdx n}:
      if(debug) Console.Error.WriteLine("#{nt} -> #{showBody b}");
      #{indent 1 . T.intercalate "\n" $ map pushSymbol (reverse b)}
      break;
    |] :: Text
    makeBody _ _ = error "Should never happen"
    pushSymbol s = [interp|stack.Push(#{encodeSymbol s});|] :: Text
    makeAction ((_, body), mcode) n = [interp|
      case #{showIdx n}: {
        #{indent 1 $ T.intercalate "\n" (reverse $ zipWith showArg body [1::Word ..])}
        resultStack.Push(#{act});
        break;
      }|] :: Text
      where
        act :: Text
        act | Just code <- mcode
            = [interp|(#{code})|]
            | otherwise
            = "null"
        showArg (NonTerm _) i = [interp|
          dynamic _#{i}=resultStack.Pop();
          |]
        showArg _ i = [interp|
          var _#{i}=resultStack.Pop().Item2;
          |]

encodeSymbol :: Symbol -> Text
encodeSymbol (NonTerm nt) = "NonTerminal.NT_" <> nt
encodeSymbol (Term t) = "TokenType.Tok_" <> t
encodeSymbol TermEof = "TokenType.eof"
