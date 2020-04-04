{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Lexer.CPP where

import qualified Data.IntSet as IS
import qualified Data.List.NonEmpty as NE
import Regex.Parse
import qualified Data.Text as T
import Lexer.Types
import Lang
import Utils

instance LexerWriter CPP where
  writeLexer _ accSt tokNames stList =
    [ ( "tokenType.h", [interp|
#ifndef TOKEN_TYPE_H
#define TOKEN_TYPE_H
#include <cstddef>
enum class TokenType : std::size_t {
  eof, #{T.intercalate "," (map ("Tok_"<>) tokNames)}
};
#endif
|]), ("lexer.h", [interp|
#ifndef LEXER_H
#define LEXER_H
#include <string>
#include "tokenType.h"
const std::string to_string(TokenType tt);
#if __has_include("token.h")
#include "token.h"
#else
struct Token{TokenType type; std::string text;};
Token mkToken(TokenType type, const std::string_view &text = "");
#endif
class Lexer {
  const std::string _input;
  const std::string_view input;
  std::string::const_iterator curChIx;
  std::string::const_iterator endIx;
  const bool debug;
public:
  Lexer(const std::string &input, bool debug);
  Token getNextToken();
};
#endif
|]), ("lexer.cpp", [interp|
#include "lexer.h"
#include <stdexcept>
#include <iostream>
const std::string to_string(TokenType tt){
static constexpr const char *names[] = { #{tokReflect} };
return names[static_cast<std::size_t>(tt)];
}
#if __has_include("token.h")
#else
Token mkToken(TokenType type, const std::string_view &text) {
  return Token{type, std::string(text)};
}\n
#endif\n
Lexer::Lexer(const std::string &input, bool debug=false)
: _input(input), input(_input), curChIx(input.cbegin()), endIx(input.cend()), debug(debug) {}
Token Lexer::getNextToken() {
  start:
  auto lastAccChIx = curChIx;
  auto startChIx = curChIx;
  char curCh;
  int accSt = -1;
  #{transTable}
  end:
  auto lastReadChIx = curChIx;
  curChIx = lastAccChIx;
  std::string_view text(&*startChIx, std::distance(startChIx, curChIx));
  switch(accSt){
    #{returnResult}
  }
  if (curChIx == endIx) {
  if (debug) std::cerr << "Got EOF while lexing \"" << text << "\"" << std::endl;
  return mkToken(TokenType::eof); }
  throw std::runtime_error("Unexpected input: " + std::string(startChIx, lastReadChIx));
}
|])]
    where
    accStS = IS.fromList $ map fst accSt
    checkAccepting st
      | st `IS.member` accStS
      = "lastAccChIx = curChIx; accSt = "<>tshow st <>";"
      | otherwise = ""
    returnResult = T.concat (foldr ((:) . returnResult1) [] accSt)
    tokReflect = T.intercalate "," . map (\x -> "\"" <> x <> "\"") $ "%eof":tokNames
    checkState (curSt, (_, charTrans)) = "state_" <> tshow curSt <> ":"
      <> checkAccepting curSt
      <> "if(curChIx == endIx) goto end;"
      <> "curCh = *curChIx; ++curChIx;"
      <> T.intercalate " else " (foldr ((:) . checkChars) [] charTrans)
      <> "goto end;"
    transTable = foldMap checkState stList
    returnResult1 (st, (Just name, act))
      = "case "<> tshow st <>":\
        \if (debug) std::cerr << \"Lexed token " <> name <> ": \\\"\" << text << \"\\\"\" << std::endl; \
        \return mkToken(TokenType::Tok_" <> name <> mkAct act <>");"
    returnResult1 (st, (Nothing, _))
      = "case "<> tshow st <>":\
        \if (debug) std::cerr << \"Skipping state " <> tshow st <> ": \\\"\" << text << \"\\\"\" << std::endl; \
        \goto start;"
    mkAct NoAction = ""
    mkAct (Action act) = "," <> act
    checkChars (charGroup, newSt) = "if(" <> charCond charGroup <> ") goto state_" <> tshow newSt <> ";"
    charCond = T.intercalate "||" . map charCond1 . NE.toList
    charCond1 (CChar c) = "curCh == " <> tshow c
    charCond1 (CRange c1 c2) = "(curCh >= " <> tshow c1 <> " && curCh <= " <> tshow c2 <> ")"
    charCond1 CAny = "true"
