{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Lexer.Python() where

import qualified Data.List.NonEmpty as NE
import Regex.Parse
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Lexer.Types
import Lang
import Utils

instance LexerWriter Python where
  writeLexer _ accSt tokNames stList =
    [ ("lexer.py", [interp|
from enum import IntEnum
class TokenType(IntEnum):
    eof = 0
    #{indent 1 tokDefns}

class Lexer:
    def __init__(self, input, debug = False):
        self.input = input
        self.curChIx = 0
        self.debug = debug

    def getNextToken(self):
        lastAccChIx = self.curChIx
        startChIx = self.curChIx
        curCh = '\\0'
        accSt = -1
        curSt = 0
        while curSt >= 0:
            if curSt in [#{T.intercalate "," $ map (tshow . fst) accSt}]:
                lastAccChIx = self.curChIx
                accSt = curSt
            if curSt in [#{T.intercalate "," nonGreedyStates}]:
                break
            if self.curChIx >= len(self.input): break
            curCh = self.input[self.curChIx]
            self.curChIx+=1
            #{indent 3 transTable}
            break

        lastReadChIx = self.curChIx
        self.curChIx = lastAccChIx
        text = self.input[startChIx:lastAccChIx]
        #{indent 2 returnResult}
        if self.curChIx >= len(self.input):
            if self.debug: print("Got EOF while lexing \\"" + text + "\\"")
            return (TokenType.eof, None)
        raise Exception("Unexpected input: " + self.input[startChIx:lastReadChIx])
|])]
    where
    indent = indentLang 4
    nonGreedyStates = map (tshow . fst) $ filter (nonGreedy . snd) accSt
      where nonGreedy StateData{saGreed=NonGreedy} = True
            nonGreedy _ = False
    returnResult = T.intercalate "\nel" (map returnResult1 accSt)
    returnResult1 :: (Int, StateData) -> Text
    returnResult1 (st, StateData{saName=Just name, saAct=act}) = [interp|
      if accSt == #{tshow st}:
          if self.debug: print("Lexed token #{name}: \\"" + text + "\\"")
          return (TokenType.Tok_#{name}, #{mkAct act})
      |]
    returnResult1 (st, StateData{saName=Nothing}) = [interp|
      if accSt == #{tshow st}:
          if self.debug: print("Skipping state #{tshow st}: \\"" + text + "\\"")
          return self.getNextToken()
      |]
    checkState :: (Int, (a, [(NE.NonEmpty CharPattern, Int)])) -> Maybe Text
    checkState (_, (_, [])) = Nothing
    checkState (curSt, (_, charTrans)) = Just [interp|
      if curSt == #{tshow curSt}:
          #{indent 1 $ T.intercalate "\nel" (map checkChars charTrans)}
          break
      |]
    transTable = T.intercalate "\nel" $ mapMaybe checkState stList
    tokDefns = T.intercalate "\n" $ zipWith (\x n -> [interp|Tok_#{fst x} = #{n}|] :: Text) tokNames [1::Word ..]
    mkAct NoAction = "None"
    mkAct (Action act) = act
    checkChars :: (NE.NonEmpty CharPattern, Int) -> Text
    checkChars (charGroup, newSt) = [interp|
      if #{charCond charGroup}:
          curSt = #{tshow newSt}
          continue
      |]
    charCond = T.intercalate " or " . map charCond1 . NE.toList
    charCond1 (CChar c) = "curCh == " <> tshow c
    charCond1 (CRange c1 c2) = "(curCh >= " <> tshow c1 <> " and curCh <= " <> tshow c2 <> ")"
    charCond1 CAny = "True"
    charCond1 (CNot c) = "not (" <> charCond1 c <> ")"
