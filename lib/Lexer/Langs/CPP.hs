{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE QuasiQuotes, OverloadedStrings, FlexibleContexts #-}
module Lexer.Langs.CPP() where

import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE
import Regex.Parse
import qualified Data.Text as T
import Lexer.Types
import Lang
import Utils

instance LexerWriter 'CPP where
  writeLexer _ accSt tokNames stList =
    [ ("lexer.h", [interp|
\#ifndef LEXER_H
\#define LEXER_H
\#include <cstddef>
\#include <string>
\#include <utility>
\#include <any>
enum class TokenType : std::size_t {
  eof, #{T.intercalate "," (map (("Tok_"<>) . fst) tokNames)}
};
const std::string to_string(TokenType tt);
using Token = std::pair<TokenType, std::any>;
class Lexer {
  const std::string _input;
  const std::string_view input;
  std::string::const_iterator curChIx;
  std::string::const_iterator endIx;
  const bool debug;
public:
  Lexer(const std::string &input, bool debug = false);
  Token getNextToken();
};
\#endif
|]), ("lexer.cpp", [interp|
\#include "lexer.h"
\#include <stdexcept>
\#include <iostream>
const std::string to_string(TokenType tt){
  static constexpr const char *names[] = { #{tokReflect} };
  return names[static_cast<std::size_t>(tt)];
}
Lexer::Lexer(const std::string &input, bool debug)
: _input(input), input(_input), curChIx(input.cbegin()), endIx(input.cend()), debug(debug) {}
Token Lexer::getNextToken() {
  start:
  auto lastAccChIx = curChIx;
  auto startChIx = curChIx;
  char curCh;
  int accSt = -1;
  #{indent 1 transTable}
  end:
  auto lastReadChIx = curChIx;
  curChIx = lastAccChIx;
  std::string_view text_(&*startChIx, std::distance(startChIx, curChIx));
\#define text (std::string(text_))
  switch(accSt){
    #{indent 2 returnResult}
  }
\#undef text
  if (curChIx == endIx) {
  if (debug) std::cerr << "Got EOF while lexing \\"" << text_ << "\\"" << std::endl;
  return {TokenType::eof, ""}; }
  throw std::runtime_error("Unexpected input: " + std::string(startChIx, lastReadChIx));
}
|])]
    where
    indent = indentLang 2
    accStS = IM.fromList accSt
    checkAccepting st
      | Just StateData{saGreed=greed} <- st `IM.lookup` accStS = [interp|
        lastAccChIx = curChIx;
        accSt = #{st};
        #{if greed == NonGreedy then "goto end;" else "" :: T.Text}
        |]
      | otherwise = ""
    returnResult = T.intercalate "\n" (map returnResult1 accSt)
    tokReflect = T.intercalate "," . map (\x -> "\"" <> x <> "\"") $ "%eof": map fst tokNames
    checkState (curSt, (_, charTrans)) = [interp|
      state_#{curSt}:
        #{indent 1 $ checkAccepting curSt}
        if(curChIx == endIx) goto end;
        curCh = *curChIx;
        ++curChIx;
        #{T.intercalate " else " (map checkChars charTrans)}
        goto end;
      |]
    transTable = T.intercalate "\n" $ map checkState stList
    returnResult1 (st, StateData{saName=Just name, saAct=act}) = [interp|
      case #{st}:
        if (debug) std::cerr << "Lexed token #{name}: \\"" << text << "\\"" << std::endl;
        return {TokenType::Tok_#{name}#{mkAct name act}};
      |]
    returnResult1 (st, StateData{saName=Nothing}) = [interp|
      case #{st}:
        if (debug) std::cerr << "Skipping state #{st}: \\"" << text << "\\"" << std::endl;
        goto start;
      |]
    tokTypeMap = M.fromList tokNames
    mkAct _ NoAction = ", std::any()"
    mkAct tn (Action act) = case M.lookup tn tokTypeMap of
      Just (Type t) -> [interp|,static_cast<#{t}&&>(#{act})|]
      _ -> "," <> act
    checkChars (charGroup, newSt) = "if(" <> charCond charGroup <> ") goto state_" <> tshow newSt <> ";"
    charCond = T.intercalate "||" . map charCond1 . NE.toList
    charCond1 (CChar c) = [interp|curCh == '#{showChar' CPP c}'|]
    charCond1 (CRange c1 c2) = [interp|(curCh >= '#{showChar' CPP c1}' && curCh <= '#{showChar' CPP c2}')|]
    charCond1 CAny = "true"
    charCond1 (CNot c) = "!(" <> charCond c <> ")"
