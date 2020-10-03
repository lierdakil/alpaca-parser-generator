{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Lexer.Jack() where

import qualified Data.List.NonEmpty as NE
import Regex.Parse
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Char (ord)
import Lexer.Types
import Lang
import Utils

instance LexerWriter Jack where
  writeLexer _ accSt tokNames stList =
    [ ("Token.jack", [interp|
/** Very basic Token class */
class Token {
  field int _id;
  field int _dataPtr;
  static Array _tokStrMap;

  /** constructs a new token with id and arbitrary data pointer (represented by int) */
  constructor Token new(int id, int dataPtr) {
    let _id = id;
    let _dataPtr = dataPtr;
    return this;
  }

  /** free the memory; dataPtr is not deallocated */
  method void dispose() {
    // this purposefuly ignores dataPtr; extra care should be taken
    do Memory.deAlloc(this);
    return;
  }

  /** return token id */
  method int id() {
    return _id;
  }

  /** return pointer to data as int */
  method int dataPtr() {
    return _dataPtr;
  }

  /** convert token id to string. can be used for debugging */
  function String toStr(int tokenId) {
    if(_tokStrMap = null) {
      let _tokStrMap = Array.new(#{M.size tokIdMap});
      let _tokStrMap[0] = "%eof";
      #{indent 3 tokToStr}
    }
    return _tokStrMap[tokenId];
  }
}
|]), ("Lexer.jack", [interp|
/** DFA-based lexer */
class Lexer {
  /** input string */
  field String input;
  /** current position (character index) in input string */
  field int curChIx;

  /** creates a lexer with no input string */
  constructor Lexer new() {
    let input = null;
    let curChIx = 0;
    return this;
  }

  /** sets input string and resets position to 0 */
  method void setInput(String i) {
    let input = i;
    let curChIx = 0;
    return;
  }

  /** free memory; it's the responsibility of the caller to dealloc input string */
  method void dispose() {
    do Memory.deAlloc(this);
    return;
  }

  /** Tries to parse one token at the current position of the input string.
    * Returns Token. On lexing error returns null.
    */
  method Token getNextToken() {
    var int lastAccChIx, startChIx, accSt, curSt, i;
    var char curCh;
    var boolean break;
    var String text;
    var Token t;
    let lastAccChIx = curChIx;
    let startChIx = curChIx;
    let curCh = 0;
    let accSt = -1;
    let curSt = 0;
    let break = false;
    while (~break) {
      // accepting states
      if (#{T.intercalate " | " $ map (\n -> "(curSt = " <> tshow (fst n) <> ")") accSt}) {
        let lastAccChIx = curChIx;
        let accSt = curSt;
      }
      if (curChIx < input.length()) {
        let curCh = input.charAt(curChIx);
        let curChIx = curChIx+1;
        let curSt = Lexer.transTable(curSt, curCh);
        if (curSt < 0) { let break = true; }
      } else {
        let break = true;
      }
    }

    let curChIx = lastAccChIx;

    // handle rejection
    if(accSt < 0) {
      if (curChIx > (input.length() - 1)) {
        return Token.new(0 /* %eof */, null);
      }
      do Output.printString("Unexpected input");
      do Util.newline();
      return null;
    }

    // handle acceptance
    // text = input.substring(startChIx, lastAccChIx);
    let text = String.new(lastAccChIx - startChIx);
    let i = startChIx;
    while(i < lastAccChIx) {
      do text.appendChar(input.charAt(i));
      let i = i+1;
    }
    #{indent 2 returnResult}
    do text.dispose();
    do Sys.error("Internal lexer error: unreachable code reached");
    return null; // never reached
  }

  /** DFA transition table implemented via nested conditions.
    * The rationale behind using nested conditions is that
    * a table will take up at least 127*(number of states)*16 bits
    * which is almost 4K of RAM.
    */
  function int transTable(int curSt, char curCh) {
    #{indent 2 transTable}
    return -1;
  }
}
|])]
    where
    indent = indentLang 2
    returnResult = T.intercalate "\n" $ map returnResult1 accSt
    returnResult1 :: (Int, (Maybe Text, Action)) -> Text
    returnResult1 (st, (Just name, act)) = [interp|
      if (accSt = #{st}) {
        let t = Token.new(#{tokId name} /* #{name} */, #{mkAct act});
        do text.dispose();
        return t;
      }|]
    returnResult1 (st, (Nothing, _)) = [interp|
      if (accSt = #{st}) { /* skip */ return getNextToken(); }|]
    checkState :: (Int, (a, [(NE.NonEmpty CharPattern, Int)])) -> Maybe Text
    checkState (_, (_, [])) = Nothing
    checkState (curSt, (_, charTrans)) = Just [interp|
      if (curSt = #{tshow curSt}) {
        #{indent 1 $ T.intercalate "\n" (map checkChars charTrans)}
        return -1;
      }|]
    tokId name = fromJust $ M.lookup name tokIdMap
    tokIdMap = M.fromList $ ("%eof", 0) : zip tokNames [1 :: Word ..]
    transTable = T.intercalate "\n" $ mapMaybe checkState stList
    tokToStr = T.intercalate "\n" $ zipWith (\x n -> [interp|
      let _tokStrMap[#{n}] = "#{x}";
      |]) tokNames [1::Word ..]
    mkAct NoAction = "null"
    mkAct (Action act) = act
    checkChars :: (NE.NonEmpty CharPattern, Int) -> Text
    checkChars (charGroup, newSt) = [interp|
      if (#{charCond charGroup}) { return #{newSt}; }
      |]
    charCond = T.intercalate " | " . map charCond1 . NE.toList
    charCond1 :: CharPattern -> Text
    charCond1 (CChar c) = [interp|(curCh = #{tshow $ ord c} /* #{c} */)|]
    charCond1 (CRange c1 c2) = [interp|((curCh > #{tshow $ ord c1 - 1} /* >= #{c1} */) & (curCh < #{tshow $ ord c2 + 1} /* <= #{c2} */))|]
    charCond1 CAny = "true"
