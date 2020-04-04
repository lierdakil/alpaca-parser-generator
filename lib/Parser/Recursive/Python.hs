{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, QuasiQuotes
  , RecordWildCards
  , MultiParamTypeClasses
  #-}
module Parser.Recursive.Python where

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

instance ParserWriter RecursiveParser Python where
  --writeParser :: forall a. Proxy lang -> ParserOptions a -> parser -> [(FilePath,Text)]
  writeParser _ gtop ParserOptions{..} RecursiveParser{..} =
    [(basename <> ".py", [interp|
from lexer import TokenType
#{gtop}
class #{parserOptionsName}:
    def __init__(self, lexer, debug = False):
        self.lex = lexer
        self.debug = debug
        self.curTok = self.lex.getNextToken()

    #{indent 1 $ T.intercalate "\n" parsers}

    def parse(self):
        return self.parse_#{recursiveParserStartRule}()
|])]
    where
    RecursiveParserItem recursiveParserStartRule startRuleDoesReturn _ :| _ = recursiveParserParsers
    basename = parserOptionsBaseFileName
    headerName = map toUpper basename
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
      |] where alts' = map (A.first fromJust) $ filter (isJust . fst) alts
               alt | Just res <- lookup Nothing alts = uncurry makeBody res
                   | otherwise = [interp|
                      raise Exception("No alternative matched while parsing nonterminal #{h}:" + str(self.curTok.type));
                      |]

    makeBody :: Body -> Maybe Text -> Text
    makeBody (Body debug syms) act = [interp|
      #{printDebug debug}
      #{T.intercalate "\n" $ map (uncurry makeBodySym) $ zip [1::Word ..] syms}
      #{T.strip $ fromMaybe "" act}
      |]

    makeAlt :: Lookahead -> (Body, Maybe Text) -> Text
    makeAlt s (b, act) = [interp|
        if self.curTok.type == TokenType.#{tok s}:
            #{indent 1 $ makeBody b act}
        |]


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
        if self.curTok.type != TokenType.#{tok s'}:
            raise Exception("Expected token #{tok s'}, but got " + str(curTok.type))
        _#{n} = self.curTok
        self.curTok = self.lex.getNextToken()
        |]
