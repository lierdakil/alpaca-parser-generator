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

class Buf:
    def __init__(self, it):
        self.current = iter(it)
        self.stack = []

    def __iter__(self):
        return self

    def __next__(self):
        if self.current is None:
            raise StopIteration
        try:
            return next(self.current)
        except StopIteration:
            if self.stack:
                self.current = self.stack.pop()
                return next(self)
            else:
                self.current = None
                raise StopIteration

    def __bool__(self):
        return self.current is not None

    def unshift(self, it):
        self.stack.append(self.current)
        self.current = iter(it)



def lex(input, debug = False):
    inputBuf = Buf(input)

    while True:
        curCh = None
        accSt = -1
        curSt = 0
        buf = ""
        tmp = ""
        while curSt >= 0:
            if curSt in [#{T.intercalate "," $ map (tshow . fst) accSt}]:
                buf += tmp
                tmp = ""
                accSt = curSt
            if curSt in [#{T.intercalate "," nonGreedyStates}]:
                break
            try:
                curCh = next(inputBuf)
            except StopIteration:
                break
            tmp += curCh
            #{indent 3 transTable}
            break

        if tmp:
            inputBuf.unshift(tmp)
        text = buf
        #{indent 2 returnResult}
        if not inputBuf:
            if debug: print("Got EOF while lexing \\"" + text + "\\"")
            yield (TokenType.eof, None)
            continue
        raise Exception("Unexpected input: " + buf + tmp)
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
          if debug: print("Lexed token #{name}: \\"" + text + "\\"")
          yield (TokenType.Tok_#{name}, #{mkAct act})
          continue
      |]
    returnResult1 (st, StateData{saName=Nothing}) = [interp|
      if accSt == #{tshow st}:
          if debug: print("Skipping state #{tshow st}: \\"" + text + "\\"")
          continue
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
