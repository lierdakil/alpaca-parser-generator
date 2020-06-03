{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, QuasiQuotes
  , RecordWildCards
  , MultiParamTypeClasses
  #-}
module Parser.Recursive.Python () where

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

instance ParserWriter RecursiveParser Python where
  --writeParser :: forall a. Proxy lang -> ParserOptions a -> parser -> [(FilePath,Text)]
  writeParser _ gtop ParserOptions{..} RecursiveParser{..} =
    [(basename <> ".py", [interp|
from lexer import TokenType
#{topTop gtop}
class #{parserOptionsName}#{topInh gtop}:
    def __init__(self, lexer, debug = False):
        self.lex = lexer
        self.debug = debug
        self.curTok = self.lex.getNextToken()

    #{indent 1 $ T.intercalate "\n" parsers}

    def parse(self):
        return self.parse_#{recursiveParserStartRule}()
|])]
    where
    RecursiveParserItem recursiveParserStartRule _ _ :| _ = recursiveParserParsers
    basename = parserOptionsBaseFileName
    indent = indentLang 4
    parsers :: [Text]
    parsers = map makeOneParser $ NE.toList recursiveParserParsers

    printDebug :: Text -> Text
    printDebug t = [interp|if self.debug: print("#{t}")|]

    makeOneParser :: RecursiveParserItem -> Text
    makeOneParser RecursiveParserItem{..} = [interp|
      def parse_#{recursiveParserItemHead}(self):
          #{indent 1 $ makeAlternatives recursiveParserItemHead recursiveParserItemAlternatives}
      |]

    makeAlternatives :: Text -> RecursiveParserItemAlternatives -> Text
    makeAlternatives _ (SingleBody b act) = makeBody b act
    makeAlternatives h (MultiBody alts) = [interp|
      #{T.intercalate "\nel" $ map (uncurry makeAlt) alts'}
      else:
          #{indent 1 alt}
      |] where alts' = map (A.first $ S.map fromJust) $ filter (all isJust . fst) alts
               alt | Just res <- find (S.member Nothing . fst) alts = uncurry makeBody (snd res)
                   | otherwise = [interp|
                      raise Exception("No alternative matched while parsing nonterminal #{h}:" + str(self.curTok[0]));
                      |]

    makeBody :: Body -> Maybe Text -> Text
    makeBody (Body debug syms) act = [interp|
      #{printDebug debug}
      #{T.intercalate "\n" $ map (uncurry makeBodySym) $ zip [1::Word ..] syms}
      #{writeAction act}
      |]

    makeAlt :: S.Set Symbol -> (Body, Maybe Text) -> Text
    makeAlt s (b, act) = [interp|
        if #{checkLookahead s}:
            #{indent 1 $ makeBody b act}
        |]

    checkLookahead :: S.Set Symbol -> Text
    checkLookahead la = T.intercalate " or " $ map cond $ S.toList la
      where
        cond :: Symbol -> Text
        cond s = [interp|self.curTok[0] == TokenType.#{tok s}|]

    writeAction :: Maybe Text -> Text
    writeAction Nothing = ""
    writeAction (Just a) = [interp|return #{T.strip a};|]

    tok :: Symbol -> Text
    tok TermEof = "eof"
    tok (Term x) = "Tok_" <> x
    tok _ = error "Not a token"

    makeBodySym :: Word -> (Symbol, RecursiveParserItemDoesReturn) -> Text
    makeBodySym n s = case s of
      (NonTerm nt, DoesReturnValue)
        -> [interp|_#{n} = self.parse_#{nt}();|]
      (NonTerm nt, DoesNotReturnValue)
        -> [interp|self.parse_#{nt}();|]
      (s', _) -> [interp|
        if self.curTok[0] != TokenType.#{tok s'}:
            raise Exception("Expected token #{tok s'}, but got " + str(self.curTok[0]))
        _#{n} = self.curTok[1]
        self.curTok = self.lex.getNextToken()
        |]
