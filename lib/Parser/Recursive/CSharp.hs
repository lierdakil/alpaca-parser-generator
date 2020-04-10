{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, QuasiQuotes
  , RecordWildCards
  , MultiParamTypeClasses
  #-}
module Parser.Recursive.CSharp () where

import Grammar

import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as S
import Data.List
import Utils
import Parser.Types
import qualified Control.Arrow as A
import Parser.Recursive.Build
import Lang

instance ParserWriter RecursiveParser CSharp where
  --writeParser :: forall a. Proxy lang -> ParserOptions a -> parser -> [(FilePath,Text)]
  writeParser _ gtop ParserOptions{..} RecursiveParser{..} =
    [(basename <> ".cs", [interp|
using lexer;
using System;
#{topTop gtop}
namespace parser {
class #{parserOptionsName}#{topInh gtop} {
  private readonly Lexer lex;
  private (TokenType type, dynamic attr) curTok;
  private readonly bool debug;
  #{indent 1 $ T.intercalate "\n" parsers}
  public #{parserOptionsName}(Lexer lex, bool debug = false) {
    this.lex = lex;
    this.debug = debug;
    curTok = lex.getNextToken();
  }
  public dynamic parse() {
    return parse_#{recursiveParserStartRule}();
  }
}
}
|])]
    where
    RecursiveParserItem recursiveParserStartRule _ _ :| _ = recursiveParserParsers
    basename = parserOptionsBaseFileName
    indent = indentLang 2
    parsers :: [Text]
    parsers = map makeOneParser $ NE.toList recursiveParserParsers

    makeOneParser :: RecursiveParserItem -> Text
    makeOneParser RecursiveParserItem{..} = [interp|
      private dynamic parse_#{recursiveParserItemHead}() {
        #{indent 1 $ makeAlternatives recursiveParserItemHead recursiveParserItemAlternatives }
      }
      |]

    makeAlternatives :: Text -> RecursiveParserItemAlternatives -> Text
    makeAlternatives _ (SingleBody b act) = makeBody b act
    makeAlternatives h (MultiBody alts) = [interp|
      #{T.intercalate " else " $ map (uncurry makeAlt) alts'} else {
        #{indent 1 alt}
      }
      |] where alts' = map (A.first $ S.map fromJust) $ filter (all isJust . fst) alts
               alt | Just res <- find (S.member Nothing . fst) alts = uncurry makeBody (snd res)
                   | otherwise = [interp|
                        throw new ApplicationException($"No alternative matched while parsing nonterminal #{h}: {curTok.type}");
                      |]

    makeBody :: Body -> Maybe Text -> Text
    makeBody (Body debug syms) act = [interp|
      if (debug) Console.Error.WriteLine("#{debug}");
      #{T.intercalate "\n" $ map (uncurry makeBodySym) $ zip [1::Word ..] syms}
      #{writeAction act}
      |]

    writeAction :: Maybe Text -> Text
    writeAction Nothing = ""
    writeAction (Just a) = [interp|return #{T.strip a};|]

    makeAlt :: S.Set Symbol -> (Body, Maybe Text) -> Text
    makeAlt s (b, act) = [interp|
      if(#{checkLookahead s}){
        #{indent 1 $ makeBody b act}
      }
      |]

    checkLookahead :: S.Set Symbol -> Text
    checkLookahead la = T.intercalate " || " $ map cond $ S.toList la
      where
        cond :: Symbol -> Text
        cond s = [interp|curTok.type == TokenType.#{tok s}|]

    tok :: Symbol -> Text
    tok TermEof = "eof"
    tok (Term x) = "Tok_" <> x
    tok _ = error "Not a token"

    makeBodySym :: Word -> (Symbol, RecursiveParserItemDoesReturn) -> Text
    makeBodySym n s = case s of
      (NonTerm nt, DoesReturnValue)
        -> [interp|dynamic _#{n} = parse_#{nt}();|]
      (NonTerm nt, DoesNotReturnValue)
        -> [interp|parse_#{nt}();|]
      (s', _) -> [interp|
        if(curTok.type != TokenType.#{tok s'})
          throw new ApplicationException($"Expected token #{tok s'}, but got {curTok.type}");
        var _#{n} = curTok;
        curTok = lex.getNextToken();
        |]
