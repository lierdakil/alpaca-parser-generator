{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Lexer.JS() where

import qualified Data.List.NonEmpty as NE
import Regex.Parse
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Lexer.Types
import Lang
import Utils

instance LexerWriter JS where
  writeLexer _ accSt tokNames stList =
    [ ("lexer.js", [interp|
'use strict'

const TokenType = {
  eof: 0,
  #{indent 1 tokDefns}
}

function tokToStr(x) {
  switch(x) {
    case 0: return '%eof'
    #{indent 2 tokToStr}
  }
}

class Lexer {
  constructor(input, debug = false) {
    this.input = input
    this.curChIx = 0
    this.debug = debug
  }

  getNextToken() {
    let lastAccChIx = this.curChIx
    const startChIx = this.curChIx
    let curCh = '\\0'
    let accSt = -1
    let curSt = 0
    while (curSt >= 0) {
      if ([#{T.intercalate "," $ map (tshow . fst) accSt}].includes(curSt)) {
        lastAccChIx = this.curChIx
        accSt = curSt
      }
      if ([#{T.intercalate "," nonGreedyStates}].includes(curSt)) {
        break
      }
      if (this.curChIx >= this.input.length) break
      curCh = this.input[this.curChIx]
      this.curChIx+=1
      switch(curSt) {
      #{indent 3 transTable}
      }
      break
    }

    const lastReadChIx = this.curChIx
    this.curChIx = lastAccChIx
    const text = this.input.substring(startChIx, lastAccChIx)
    switch(accSt) {
      #{indent 3 returnResult}
    }
    if (this.curChIx >= this.input.length) {
      if (this.debug) console.log(`Got EOF while lexing "${text}"`)
      return [TokenType.eof, null]
    }
    throw new Error("Unexpected input: " + this.input.substring(startChIx, lastReadChIx))
  }
}

module.exports = {TokenType, tokToStr, Lexer}
|])]
    where
    indent = indentLang 2
    nonGreedyStates = map (tshow . fst) $ filter (nonGreedy . snd) accSt
      where nonGreedy StateData{saGreed=NonGreedy} = True
            nonGreedy _ = False
    returnResult = T.intercalate "\n" (map returnResult1 accSt)
    returnResult1 :: (Int, StateData) -> Text
    returnResult1 (st, StateData{saName=Just name, saAct=act}) = [interp|
      case #{st}:
        if (this.debug) console.log("Lexed token #{name}: \\"" + text + "\\"")
        return [TokenType.Tok_#{name}, #{mkAct act}]
      |]
    returnResult1 (st, StateData{saName=Nothing}) = [interp|
      case #{st}:
        if (this.debug) console.log("Skipping state #{tshow st}: \\"" + text + "\\"")
        return this.getNextToken()
      |]
    checkState :: (Int, (a, [(NE.NonEmpty CharPattern, Int)])) -> Maybe Text
    checkState (_, (_, [])) = Nothing
    checkState (curSt, (_, charTrans)) = Just [interp|
      case #{tshow curSt}:
        #{indent 1 $ T.intercalate " else " (map checkChars charTrans)}
        break
      |]
    transTable = T.intercalate "\n" $ mapMaybe checkState stList
    tokDefns = T.intercalate ",\n" $ zipWith (\x n -> [interp|Tok_#{fst x}: #{n}|] :: Text) tokNames [1::Word ..]
    tokToStr = T.intercalate "\n" $ zipWith (\x n -> [interp|case #{n}: return '#{fst x}'|] :: Text) tokNames [1::Word ..]
    mkAct NoAction = "null"
    mkAct (Action act) = act
    checkChars :: (NE.NonEmpty CharPattern, Int) -> Text
    checkChars (charGroup, newSt) = [interp|
      if (#{charCond charGroup}) {
        curSt = #{newSt}
        continue
      }
      |]
    charCond = T.intercalate " || " . map charCond1 . NE.toList
    charCond1 :: CharPattern -> Text
    charCond1 (CChar c) = [interp|curCh === '#{showChar' JS c}'|]
    charCond1 (CRange c1 c2) = [interp|(curCh >= '#{showChar' JS c1}' && curCh <= '#{showChar' JS c2}')|]
    charCond1 CAny = "true"
    charCond1 (CNot c) = "!(" <> charCond1 c <> ")"
