{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, QuasiQuotes
  , RecordWildCards
  , MultiParamTypeClasses
  , FlexibleContexts
  #-}
module Parser.LR.Python () where

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

instance LRPoint p => ParserWriter (LRParser p) Python where
  --writeParser :: Proxy lang -> Text -> ParserOptions a -> parser -> [(FilePath,Text)]
  writeParser _ gtop ParserOptions{..} LRParser{..} = [
      (base <> ".py", [interp|
from lexer import *
from enum import IntEnum
from collections import deque

#{topTop gtop}

def stateToString(state):
  return [ #{stateToString} ][state]

def expectedSym(state):
  return [ #{expectedSym} ][state]

class #{name}#{topInh gtop}:
    Action = [
        #{indent 2 actionTable}
        ]
    GOTO = [
        #{indent 2 gotoTable}
        ]
    def __init__(self, lex, debug=False):
        self.lex = lex
        self.debug = debug
        self.stack = deque()
    def top(self):
        if len(self.stack)>0:
            return self.stack[-1][0]
        else:
            return 0
    def parse(self):
        a = self.lex.getNextToken()
        while True:
            action = self.Action[self.top()][int(a[0])]
            #{indent 3 $ T.intercalate "\nel" actionCases}
            else:
                if self.debug: print(f"Shift to {action}")
                self.stack.append((action, a))
                a=self.lex.getNextToken()
|])]
    where
    base = parserOptionsBaseFileName
    name = parserOptionsName
    tokens = lrTerminals
    states = lrStates
    nonTerminals = lrNonTerminals
    braces :: Text -> Text
    braces x = "["<>x<>"]"
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
    stateToString = T.intercalate "," $ "\".\"" : map (quote . showSymbol) (M.elems lrStateSym)
    expectedSym = T.intercalate "," $ map (quote . T.intercalate "/") $ M.elems lrExpected
    actionCases = mapMaybe writeAction $ M.toList actionsMap
    writeAction (Shift _, _) = Nothing
    writeAction (a, n) = Just [interp|
    if action == #{tshow n}:
        #{indent 1 $ actionBody a}
    |] :: Maybe Text
    actionBody Reject = [interp|
      lastSt = self.top()
      parsed=stateToString(lastSt)
      while len(self.stack) > 0:
          self.stack.pop()
          parsed = stateToString(self.top()) + " " + parsed
      raise Exception(
        f'Rejection state reached after parsing "{parsed}", when encoutered symbol "{a[0].name}" in state {lastSt}. Expected "{expectedSym(lastSt)}"')
      |] :: Text
    actionBody (Shift _) = error "does not happen"
    actionBody (Reduce ((ExtendedStartRule, _), _)) = [interp|
      self.stack.pop()
      return self.stack.pop()[1]
      |]
    actionBody (Reduce ((h, body), mcode)) = [interp|
      if self.debug: print("Reduce using #{h} -> #{showBody body}")
      #{T.intercalate "\n" (reverse $ zipWith showArg body [1::Word ..])}
      gt = self.GOTO[self.top()][#{tshow (nonTermIdx (NonTerm h))}] \# #{h}
      if gt==0: raise Exception("No goto")
      if self.debug:
          print(f'{self.top()} is now on top of the stack;')
          print(f'{gt} will be placed on the stack')
      self.stack.append((gt,(#{result})))
      |]
      where
        result :: Text
        result
          | Just code <- mcode
          = T.strip code
          | otherwise
          = "None"
        showArg (NonTerm _) i = [interp|_#{i} = self.stack.pop()[1]|]
        showArg _ i = [interp|_#{i} = self.stack.pop()[1][1]|]
    nonTermIdx nt = fromJust $ M.lookup nt nonTerminalsMap
    nonTerminalsMap = M.fromList $ zip nonTerminals [0::Word ..]
    quote x = "\"" <> x <> "\""
    indent = indentLang 4
