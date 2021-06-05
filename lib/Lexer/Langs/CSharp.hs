{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE QuasiQuotes, OverloadedStrings, FlexibleContexts #-}
module Lexer.Langs.CSharp() where

import qualified Data.IntMap as IM
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
using System.Collections;
using System.Collections.Generic;

namespace lexer {
public enum TokenType : uint {
  eof, #{T.intercalate "," (map (("Tok_"<>) . fst) tokNames)}
}

class Buf<T> : IEnumerator<T> {
  IEnumerator<T> current;
  Stack<IEnumerator<T>> stack;

  Object IEnumerator.Current => Current;
  public T Current => Empty ? default(T) : current.Current;
  public bool Empty => current is null;

  public Buf(IEnumerable<T> it) {
    current = it.GetEnumerator();
    stack = new Stack<IEnumerator<T>>();
  }

  public bool MoveNext() {
    if (Empty) return false;
    var res = current.MoveNext();
    if (!res) {
      if (stack.Count > 0) {
        current = stack.Pop();
        return MoveNext();
      } else {
        current = null;
      }
    }
    return res;
  }

  public void Unshift(IEnumerable<T> it) {
    stack.Push(current);
    current = it.GetEnumerator();
  }

  public void Reset() {
    throw new NotSupportedException();
  }

  public void Dispose() {
    /* no-op */
  }
}

public class Lexer {
  public static IEnumerable<(TokenType type, dynamic attr)> lex(IEnumerable<char> input, bool debug = false) {
    var inputBuf = new Buf<char>(input);
    start:
    char curCh;
    int accSt = -1;
    string buf = "";
    string tmp = "";
    #{indent 2 transTable}
    end:
    if (tmp.Length > 0) {
      inputBuf.Unshift(tmp);
    }
    var text = buf;
    switch(accSt){
      #{indent 3 returnResult}
    }
    if (inputBuf.Empty) {
      if (debug) Console.Error.WriteLine($"Got EOF while lexing \\"{text}\\"");
      yield return (TokenType.eof, null);
      goto start;
    }
    throw new ApplicationException("Unexpected input: " + buf + tmp);
  }
}
}
|])]
    where
    indent = indentLang 2
    accStS = IM.fromList accSt
    checkAccepting st
      | Just StateData{saGreed=greed} <- st `IM.lookup` accStS = [interp|
        buf += tmp;
        tmp = "";
        accSt = #{st};
        #{if greed == NonGreedy then "goto end;" else "" :: T.Text}
        |]
      | otherwise = ""
    returnResult = T.intercalate "\n" (map returnResult1 accSt)
    checkState (curSt, (_, charTrans)) = [interp|
      state_#{curSt}:
        #{indent 1 $ checkAccepting curSt}
        if(!inputBuf.MoveNext()) goto end;
        curCh = inputBuf.Current;
        tmp += curCh;
        #{indent 1 $ T.intercalate " else " $ map checkChars charTrans}
        goto end;
      |] :: Text
    transTable = T.intercalate "\n" $ map checkState stList
    returnResult1 (st, StateData{saName=Just name, saAct=act}) = [interp|
      case #{st}:
        if (debug) Console.Error.WriteLine($"Lexed token #{name}: \\"{text}\\"");
        yield return (TokenType.Tok_#{name}, #{mkAct act});
        goto start;
      |] :: Text
    returnResult1 (st, StateData{saName=Nothing}) = [interp|
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
    charCond1 (CChar c) = [interp|curCh == '#{showChar' CSharp c}'|]
    charCond1 (CRange c1 c2) = [interp|(curCh >= '#{showChar' CSharp c1}' && curCh <= '#{showChar' CSharp c2}')|]
    charCond1 CAny = "true"
    charCond1 (CNot c) = "!(" <> charCond1 c <> ")"
