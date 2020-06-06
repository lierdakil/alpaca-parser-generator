{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, QuasiQuotes
  , RecordWildCards
  , MultiParamTypeClasses
  #-}
module Parser.Recursive.JS () where

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

instance ParserWriter RecursiveParser JS where
  writeParser _ gtop ParserOptions{..} RecursiveParser{..} =
    [(basename <> ".js", [interp|
const {tokToStr, TokenType} = require('./lexer.js')
#{topTop gtop}
class #{parserOptionsName}#{topInh gtop} {
  constructor(lexer, debug = false) {
    this.lex = lexer
    this.debug = debug
    this.curTok = this.lex.getNextToken()
  }

  parse() {
    return this._parse_#{recursiveParserStartRule}()
  }

  #{indent 1 $ T.intercalate "\n" parsers}
}

module.exports = {#{parserOptionsName}}
|])]
    where
    RecursiveParserItem recursiveParserStartRule _ _ :| _ = recursiveParserParsers
    basename = parserOptionsBaseFileName
    indent = indentLang 2
    parsers :: [Text]
    parsers = map makeOneParser $ NE.toList recursiveParserParsers

    printDebug :: Text -> Text
    printDebug t = [interp|if (this.debug) console.log("#{t}")|]

    makeOneParser :: RecursiveParserItem -> Text
    makeOneParser RecursiveParserItem{..} = [interp|
      _parse_#{recursiveParserItemHead}() {
        #{indent 1 $ makeAlternatives recursiveParserItemHead recursiveParserItemAlternatives}
      }
      |]

    makeAlternatives :: Text -> RecursiveParserItemAlternatives -> Text
    makeAlternatives _ (SingleBody b act) = makeBody b act
    makeAlternatives h (MultiBody alts) = [interp|
      #{T.intercalate "\nelse " $ map (uncurry makeAlt) alts'}
      else {
        #{indent 1 alt}
      }
      |] where alts' = map (A.first $ S.map fromJust) $ filter (all isJust . fst) alts
               alt | Just res <- find (S.member Nothing . fst) alts = uncurry makeBody (snd res)
                   | otherwise = [interp|
                      throw new Error("No alternative matched while parsing nonterminal #{h}:" + tokToStr(this.curTok[0]));
                      |]

    makeBody :: Body -> Maybe Text -> Text
    makeBody (Body debug syms) act = [interp|
      #{printDebug debug}
      #{T.intercalate "\n" $ map (uncurry makeBodySym) $ zip [1::Word ..] syms}
      #{writeAction act}
      |]

    makeAlt :: S.Set Symbol -> (Body, Maybe Text) -> Text
    makeAlt s (b, act) = [interp|
        if (#{checkLookahead s}) {
          #{indent 1 $ makeBody b act}
        }
        |]

    checkLookahead :: S.Set Symbol -> Text
    checkLookahead la = T.intercalate " || " $ map cond $ S.toList la
      where
        cond :: Symbol -> Text
        cond s = [interp|this.curTok[0] === TokenType.#{tok s}|]

    writeAction :: Maybe Text -> Text
    writeAction Nothing = ""
    writeAction (Just a) = [interp|return #{T.strip a}|]

    tok :: Symbol -> Text
    tok TermEof = "eof"
    tok (Term x) = "Tok_" <> x
    tok _ = error "Not a token"

    makeBodySym :: Word -> (Symbol, RecursiveParserItemDoesReturn) -> Text
    makeBodySym n s = case s of
      (NonTerm nt, DoesReturnValue)
        -> [interp|const _#{n} = this._parse_#{nt}()|]
      (NonTerm nt, DoesNotReturnValue)
        -> [interp|this._parse_#{nt}()|]
      (s', _) -> [interp|
        if (this.curTok[0] != TokenType.#{tok s'}) {
          throw new Error("Expected token #{tok s'}, but got " + tokToStr(this.curTok[0]))
        }
        const _#{n} = this.curTok[1]
        this.curTok = this.lex.getNextToken()
        |]
