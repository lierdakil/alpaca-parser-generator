{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE QuasiQuotes, OverloadedStrings, FlexibleContexts #-}
module Lexer.CSharp() where

import qualified Data.IntSet as IS
import qualified Data.List.NonEmpty as NE
import Regex.Parse
import Data.Text (Text)
import qualified Data.Text as T
import Lexer.Types
import Lang
import Utils

instance LexerWriter CSharp where
  writeLexer _ accSt tokNames stList =
    [ ("lexer.cs", [interp|
using System;

namespace lexer {
public enum TokenType : uint {
  eof, #{T.intercalate "," (map ("Tok_"<>) tokNames)}
}
public class Lexer {
  private readonly string input;
  private readonly bool debug;
  int curChIx;
  public Lexer(string input, bool debug = false) {
    this.input = input;
    this.debug = debug;
    curChIx = 0;
  }
  public (TokenType type, dynamic attr) getNextToken() {
    start:
    var lastAccChIx = curChIx;
    var startChIx = curChIx;
    char curCh;
    int accSt = -1;
    #{indent 2 transTable}
    end:
    var lastReadChIx = curChIx;
    curChIx = lastAccChIx;
    var text = input.Substring(startChIx, curChIx - startChIx);
    switch(accSt){
      #{indent 3 returnResult}
    }
    if (curChIx >= input.Length) {
      if (debug) Console.Error.WriteLine($"Got EOF while lexing \\"{text}\\"");
      return (TokenType.eof, null);
    }
    throw new ApplicationException("Unexpected input: " + input.Substring(startChIx, lastReadChIx-startChIx));
  }
}
}
|])]
    where
    indent = indentLang 2
    accStS = IS.fromList $ map fst accSt
    checkAccepting st
      | st `IS.member` accStS
      = "lastAccChIx = curChIx; accSt = "<>tshow st <>";"
      | otherwise = ""
    returnResult = T.intercalate "\n" (map returnResult1 accSt)
    checkState (curSt, (_, charTrans)) = [interp|
      state_#{curSt}:
        #{indent 1 $ checkAccepting curSt}
        if(curChIx >= input.Length) goto end;
        curCh = input[curChIx];
        ++curChIx;
        #{indent 1 $ T.intercalate " else " $ map checkChars charTrans}
        goto end;
      |] :: Text
    transTable = T.intercalate "\n" $ map checkState stList
    returnResult1 (st, (Just name, act)) = [interp|
      case #{st}:
        if (debug) Console.Error.WriteLine($"Lexed token #{name}: \\"{text}\\"");
        return (TokenType.Tok_#{name}, #{mkAct act});
      |] :: Text
    returnResult1 (st, (Nothing, _)) = [interp|
      case #{st}:
        if (debug) Console.Error.WriteLine($"Skipping state #{st}: \\"{text}\\"");
        goto start;
      |]
    mkAct NoAction = "null"
    mkAct (Action act) = act
    checkChars (charGroup, newSt) = [interp|
      if(#{charCond charGroup}) goto state_#{newSt};
      |]
    charCond = T.intercalate "||" . map charCond1 . NE.toList
    charCond1 (CChar c) = [interp|curCh == '#{c}'|]
    charCond1 (CRange c1 c2) =
      [interp|(curCh >= '#{c1}' && curCh <= '#{c2}')|]
    charCond1 CAny = "true"
