{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, QuasiQuotes
  , RecordWildCards
  , MultiParamTypeClasses
  , FlexibleContexts
  #-}
module Parser.LL.Python where

import Parser.Types
import Lang
import Utils
import Data.Maybe
import Data.List
import Parser.LL.Build
import Data.Text (Text)
import Grammar
import qualified Data.Text as T
import qualified Data.Map as M

instance ParserWriter LLParser Python where
  -- writeParser :: Proxy lang -> Text -> ParserOptions a -> parser -> [(FilePath,Text)]
  writeParser _ gtop ParserOptions{..} p@LLParser{..} = [ (basename <> ".py", sourceFile) ]
    where
    basename = parserOptionsBaseFileName
    sourceFile = [interp|
from lexer import *
from enum import IntEnum
from collections import deque
#{gtop}
class NonTerminal(IntEnum):
    #{indent 1 $ T.intercalate "\n" nonTermDefs}

class #{className}:
    M = [
        #{indent 2 $ T.intercalate ",\n" $ map (braces . T.intercalate "," . map showIdx') transTable}
    ]
    def __init__(self, lex, debug):
        self.lex = lex
        self.debug = debug
        self.stack = deque()
        self.resultStack = deque()

    def parse(self):
        self.stack.append(#{encodeSymbol llStartSymbol})
        a = self.lex.getNextToken()
        while len(self.stack) > 0:
            X = self.stack.pop()
            if isinstance(X, TokenType):
                if a.type == X:
                    self.resultStack.append(a)
                    a = self.lex.getNextToken()
                else:
                    raise Exception(f"Found terminal {a.type} but expected {X}.")
            elif isinstance(X,NonTerminal):
                trans = self.M[int(X)][int(a.type)]
                self.stack.append(trans)
                if trans == 0:
                    raise Exception(f"No transition for {X}, {a.type}")
                #{indent 4 $ T.intercalate "\n" bodies}
            elif isinstance(X, int):
                #{indent 4 $ T.intercalate "\nel" actions}

        return self.resultStack.pop()
|]
    indent = indentLang 4
    nonTermDefs = zipWith nonTermDef [(0::Word) ..] nonTerms
    nonTermDef n s = [interp|NT_#{s} = #{n}|]
    className = parserOptionsName
    (transTable, actionMap) = indexTable p
    nonTerms = mapMaybe getNt llNonTerminals
    getNt (NonTerm x) = Just x
    getNt _ = Nothing
    showIdx' Nothing = "0"
    showIdx' (Just x) = showIdx x
    showIdx x = tshow (x+1)
    braces :: Text -> Text
    braces x = "["<>x<>"]"
    actionList = sortOn snd $ M.toList actionMap
    bodies = map (uncurry makeBody) actionList
    actions = map (uncurry makeAction) actionList
    makeBody (b, _) n = [interp|
      elif trans == #{showIdx n}:
          if self.debug: print(f"{X} -> #{showBody b}")
          #{indent 1 . T.intercalate "\n" $ map pushSymbol (reverse b)}
      |] :: Text
    pushSymbol s = [interp|self.stack.append(#{encodeSymbol s})|] :: Text
    makeAction (body, mcode) n = [interp|
      if X == #{showIdx n}:
          #{indent 1 $ T.intercalate "\n" (reverse $ zipWith showArg body [1::Word ..])}
          #{indent 1 act}
      |] :: Text
      where
        act :: Text
        act | Just code <- mcode
            = [interp|
                def doAction():
                    #{indent 1 $ T.strip $ code}
                self.resultStack.append(doAction())
                |]
            | otherwise
            = [interp|self.resultStack.append(None)|]
        showArg _ i = [interp|_#{tshow i}=self.resultStack.pop()|]

encodeSymbol :: Symbol -> Text
encodeSymbol (NonTerm nt) = "NonTerminal.NT_" <> nt
encodeSymbol (Term t) = "TokenType.Tok_" <> t
encodeSymbol TermEof = "TokenType.eof"
