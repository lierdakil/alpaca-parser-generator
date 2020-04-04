{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, QuasiQuotes
  , RecordWildCards
  , MultiParamTypeClasses
  #-}
module Parser.Recursive.CPP where

import Grammar

import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Utils
import Parser.Types
import Data.Char
import qualified Control.Arrow as A
import Parser.Recursive.Build
import Lang

instance ParserWriter RecursiveParser CPP where
  --writeParser :: forall a. Proxy lang -> ParserOptions a -> parser -> [(FilePath,Text)]
  writeParser _ gtop ParserOptions{..} RecursiveParser{..} =
    [(basename <> ".h", [interp|
#ifndef #{headerName}_H
#define #{headerName}_H
#include "lexer.h"
#{gtop}
class #{parserOptionsName} {
  Lexer *lex;
  Token curTok;
  bool debug;
  #{indent 1 $ T.intercalate "\n" $ map fst parsers}
public:
  #{parserOptionsName}(Lexer *lex, bool debug);
  #{returnType startRuleDoesReturn} parse();
};
#endif
|]) ,(basename <> ".cpp", [interp|
#include "#{basename}.h"
#include <stdexcept>
#include <iostream>
#{parserOptionsName}::#{parserOptionsName}(Lexer *lex, bool debug = false):lex(lex),debug(debug){
  curTok = lex->getNextToken();
}
#{returnType startRuleDoesReturn} Parser::parse() { return parse_#{recursiveParserStartRule}(); }
#{T.intercalate "\n" $ map snd parsers}
|])]
    where
    RecursiveParserItem recursiveParserStartRule startRuleDoesReturn _ :| _ = recursiveParserParsers
    basename = parserOptionsBaseFileName
    headerName = map toUpper basename
    indent = indentLang 2
    parsers :: [(Text, Text)]
    parsers = map makeOneParser $ NE.toList recursiveParserParsers

    makeOneParser :: RecursiveParserItem -> (Text, Text)
    makeOneParser RecursiveParserItem{..} = ([interp|
      #{returnType recursiveParserItemDoesReturn} parse_#{recursiveParserItemHead}();
      |], [interp|
      #{returnType recursiveParserItemDoesReturn} #{parserOptionsName}::parse_#{recursiveParserItemHead}() {
        #{indent 1 $ makeAlternatives recursiveParserItemHead recursiveParserItemAlternatives }
      }
      |])

    returnType :: RecursiveParserItemDoesReturn -> Text
    returnType DoesReturnValue = "ResultType"
    returnType DoesNotReturnValue = "void"

    makeAlternatives :: Text -> RecursiveParserItemAlternatives -> Text
    makeAlternatives _ (SingleBody b act) = makeBody b act
    makeAlternatives h (MultiBody alts) = [interp|
      #{T.intercalate " else " $ map (uncurry makeAlt) alts'} else {
        #{indent 1 alt}
      }
      |] where alts' = map (A.first fromJust) $ filter (isJust . fst) alts
               alt | Just x <- lookup Nothing alts = uncurry makeBody x
                   | otherwise = [interp|
                        throw std::runtime_error("No alternative matched while parsing nonterminal #{h}:" + to_string(curTok.type));
                      |]

    makeBody :: Body -> Maybe Text -> Text
    makeBody (Body debug syms) act = [interp|
      if (debug) std::cerr << "#{debug}" << std::endl;
      #{T.intercalate "\n" $ map (uncurry makeBodySym) $ zip [1::Word ..] syms}
      #{T.strip $ fromMaybe "" act}
      |]

    makeAlt :: Lookahead -> (Body, Maybe Text) -> Text
    makeAlt s (b, act) = [interp|
      if(curTok.type == TokenType::#{tok s}){
        #{indent 1 $ makeBody b act}
      }
      |]

    tok :: Symbol -> Text
    tok TermEof = "eof"
    tok (Term x) = "Tok_" <> x
    tok _ = error "Not a token"

    makeBodySym :: Word -> (Symbol, RecursiveParserItemDoesReturn) -> Text
    makeBodySym n s = case s of
      (NonTerm nt, DoesReturnValue)
        -> [interp|auto _#{n} = parse_#{nt}();|]
      (NonTerm nt, DoesNotReturnValue)
        -> [interp|parse_#{nt}();|]
      (s', _) -> [interp|
        if(curTok.type != TokenType::#{tok s'})
          throw std::runtime_error("Expected token #{tok s'}, but got " + to_string(curTok.type));
        auto _#{n} = std::move(curTok);
        curTok = lex->getNextToken();
        |]
