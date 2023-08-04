{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Lexer.Langs.JS() where

import qualified Data.List.NonEmpty as NE
import Regex.Parse
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Lexer.Types
import Lang
import Utils

instance LexerWriter 'JS where
  writeLexer _ accSt tokNames stList =
    [ ("lexer.js", [interp|
'use strict'

export const TokenType = {
  eof: 0,
  #{indent 1 tokDefns}
}

export function tokToStr(x) {
  switch(x) {
    case 0: return '%eof'
    #{indent 2 tokToStr}
  }
}

class Buf {
  constructor(it) {
    this.current = it[Symbol.iterator]()
    this.stack = []
  }

  [Symbol.iterator]() {
    return this
  }

  next() {
    if (this.current === null) {
      return {done: true}
    }
    const res = this.current.next()
    if (res.done) {
      if (this.stack.length) {
        this.current = this.stack.pop()
        return this.next()
      } else {
        this.current = null
      }
    }
    return res
  }

  empty() {
    return this.current === null
  }

  unshift(it) {
    this.stack.push(this.current)
    this.current = it[Symbol.iterator]()
  }
}

export function *lex(input, debug = false) {
  const inputBuf = new Buf(input)

  while(true) {
    let curCh = '\\0'
    let accSt = -1
    let curSt = 0
    let buf = ""
    let tmp = ""
    while (curSt >= 0) {
      if ([#{T.intercalate "," $ map (tshow . fst) accSt}].includes(curSt)) {
        buf += tmp
        tmp = ""
        accSt = curSt
      }
      if ([#{T.intercalate "," nonGreedyStates}].includes(curSt)) {
        break
      }
      const t = inputBuf.next()
      if (t.done) break
      curCh = t.value
      tmp += curCh
      switch(curSt) {
      #{indent 3 transTable}
      }
      break
    }

    if (tmp.length > 0) {
      inputBuf.unshift(tmp)
    }
    const text = buf
    switch(accSt) {
      #{indent 3 returnResult}
    }
    if (inputBuf.empty()) {
      if (debug) console.log(`Got EOF while lexing "${text}"`)
      yield [TokenType.eof, null]
      continue
    }
    throw new Error("Unexpected input: " + buf + tmp)
  }
}
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
        if (debug) console.log("Lexed token #{name}: \\"" + text + "\\"")
        yield [TokenType.Tok_#{name}, #{mkAct act}]
        continue
      |]
    returnResult1 (st, StateData{saName=Nothing}) = [interp|
      case #{st}:
        if (debug) console.log("Skipping state #{tshow st}: \\"" + text + "\\"")
        continue
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
    charCond1 (CNot c) = "!(" <> charCond c <> ")"
