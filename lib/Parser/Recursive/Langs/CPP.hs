{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, QuasiQuotes
  , RecordWildCards
  , MultiParamTypeClasses
  #-}
module Parser.Recursive.Langs.CPP () where

import Grammar

import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Utils
import Parser.Types
import Data.Char
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List
import qualified Control.Arrow as A
import Parser.Recursive.Build

instance ParserWriter RecursiveParser 'CPP where
  --writeParser :: forall a. Proxy lang -> ParserOptions a -> parser -> [(FilePath,Text)]
  writeParser _ gtop ParserOptions{..} RecursiveParser{..} =
    [(basename <> ".h", [interp|
\#ifndef #{headerName}_H
\#define #{headerName}_H
\#include "lexer.h"
#{topTop gtop}
class #{parserOptionsName}#{topInh gtop} {
  Lexer *lex;
  Token curTok;
  bool debug;
  #{indent 1 $ T.intercalate "\n" $ map fst parsers}
public:
  #{parserOptionsName}(Lexer *lex, bool debug = false);
  #{startRuleType} parse();
};
\#endif
|]) ,(basename <> ".cpp", [interp|
\#include "#{basename}.h"
\#include <stdexcept>
\#include <iostream>
#{parserOptionsName}::#{parserOptionsName}(Lexer *lex, bool debug):lex(lex),debug(debug){
  curTok = lex->getNextToken();
}
#{startRuleType} Parser::parse() { return parse_#{recursiveParserStartRule}(); }
#{T.intercalate "\n" $ map snd parsers}
|])]
    where
    RecursiveParserItem recursiveParserStartRule startRuleDoesReturn _ :| _ = recursiveParserParsers
    basename = parserOptionsBaseFileName
    headerName = map toUpper basename
    indent = indentLang 2
    parsers :: [(Text, Text)]
    parsers = map makeOneParser $ NE.toList recursiveParserParsers

    startRuleType = returnType (NonTerm recursiveParserStartRule) startRuleDoesReturn

    makeOneParser :: RecursiveParserItem -> (Text, Text)
    makeOneParser RecursiveParserItem{..} = ([interp|
      #{returnType (NonTerm recursiveParserItemHead) recursiveParserItemDoesReturn} parse_#{recursiveParserItemHead}();
      |], [interp|
      #{returnType (NonTerm recursiveParserItemHead) recursiveParserItemDoesReturn} #{parserOptionsName}::parse_#{recursiveParserItemHead}() {
        #{indent 1 $ makeAlternatives recursiveParserItemHead recursiveParserItemAlternatives }
      }
      |])

    returnType :: Symbol -> RecursiveParserItemDoesReturn -> Text
    returnType _ DoesNotReturnValue = "void"
    returnType x DoesReturnValue = case M.lookup x recTypes of
      Just (Type t) -> t
      _ -> "std::any"

    makeAlternatives :: Text -> RecursiveParserItemAlternatives -> Text
    makeAlternatives _ (SingleBody b act) = makeBody b act
    makeAlternatives h (MultiBody alts) = [interp|
      #{T.intercalate " else " $ map (uncurry makeAlt) alts'} else {
        #{indent 1 alt}
      }
      |] where alts' = map (A.first $ S.map fromJust) $ filter (all isJust . fst) alts
               alt | Just res <- find (S.member Nothing . fst) alts = uncurry makeBody (snd res)
                   | otherwise = [interp|
                        throw std::runtime_error("No alternative matched while parsing nonterminal #{h}:" + to_string(curTok.first));
                      |]

    makeBody :: Body -> Maybe Text -> Text
    makeBody (Body debug syms) act = [interp|
      if (debug) std::cerr << "#{debug}" << std::endl;
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
        cond s = [interp|curTok.first == TokenType::#{tok s}|]

    tok :: Symbol -> Text
    tok TermEof = "eof"
    tok (Term x) = "Tok_" <> x
    tok _ = error "Not a token"

    makeBodySym :: Word -> (Symbol, RecursiveParserItemDoesReturn) -> Text
    makeBodySym n s@(x,_) = case s of
      (NonTerm nt, DoesReturnValue) -> [interp|auto _#{n} = parse_#{nt}();|]
      (NonTerm nt, DoesNotReturnValue) -> [interp|parse_#{nt}();|]
      (s', _) -> [interp|
        if(curTok.first != TokenType::#{tok s'})
          throw std::runtime_error("Expected token #{tok s'}, but got " + to_string(curTok.first));
        auto _#{n} = #{cast "std::move(curTok.second)"};
        curTok = lex->getNextToken();
        |]
      where cast :: Text -> Text
            cast y = case M.lookup x recTypes of
              Just (Type t) -> [interp|std::any_cast<#{t}&&>(#{y})|]
              _ -> y
