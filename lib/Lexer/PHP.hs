{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Lexer.PHP() where

import qualified Data.List.NonEmpty as NE
import Regex.Parse
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.IntSet as IS
import Lexer.Types
import Lang
import Utils

instance LexerWriter PHP where
  writeLexer _ accSt tokNames stList =
    [ ("Lexer.php", [interp|
<?php

namespace Alpaca;

class Lexer
{
    const TOKEN_TYPE_EOF = 0;
    #{indent 1 tokDefns}

    private $input;
    private $curChIx;
    private $debug;

    public function __construct(string $input, bool $debug)
    {
        $this->input = $input;
        $this->curChIx = 0;
        $this->debug = $debug;
    }

    public static function tokenToString(int $token): string
    {
        switch($token) {
            case 0: return '%eof';
            #{indent 3 tokToStr}
            default: throw new \\InvalidArgumentException("Unknown token $token");
        }
    }

    public function getNextToken(): array
    {
        $lastAccChIx = $this->curChIx;
        $startChIx = $this->curChIx;
        $accSt = -1;
        $len = strlen($this->input);
        #{indent 2 transTable}
        end:
        $lastReadChIx = $this->curChIx;
        $this->curChIx = $lastAccChIx;
        $text = substr($this->input, $startChIx, $lastAccChIx - $startChIx);
        switch($accSt) {
            #{indent 3 returnResult}
        }

        if ($this->curChIx >= $len) {
            if ($this->debug)  printf("Got EOF while lexing \\"%s\\"\\n", $text);
            return [self::TOKEN_TYPE_EOF, null];
        }
        throw new \\InvalidArgumentException("Unexpected input: " . substr($this->input, $startChIx, $lastReadChIx));
    }
}
|])]
    where
    indent = indentLang 4
    returnResult = T.intercalate "\n" (map returnResult1 accSt)
    returnResult1 :: (Int, (Maybe Text, Action)) -> Text
    returnResult1 (st, (Just name, act)) = [interp|
        case #{st}:
            if ($this->debug) printf("Lexed token #{name}: \\"%s\\"\\n", $text);
            return [self::TOKEN_TYPE_#{T.toUpper name}, #{mkAct act}];
        |]
    returnResult1 (st, (Nothing, _)) = [interp|
        case #{st}:
            if ($this->debug) printf("Skipping state #{tshow st}: \\"%s\\"\\n", $text);
            return $this->getNextToken();
        |]
    accStS = IS.fromList $ map fst accSt
    checkAccepting st
      | st `IS.member` accStS = [interp|
        $lastAccChIx = $this->curChIx;
        $accSt = #{st};
        |] :: Text
      | otherwise = ""
    checkState :: (Int, (a, [(NE.NonEmpty CharPattern, Int)])) -> Maybe Text
    checkState (curSt, (_, charTrans)) = Just [interp|
      state_#{curSt}:
          #{indent 1 $ checkAccepting curSt}
          if ($this->curChIx >= $len) goto end;
          $curCh = $this->input[$this->curChIx];
          $this->curChIx+=1;
          #{indent 1 $ T.intercalate "\nelse " $ map checkChars charTrans}
          goto end;
      |]
    transTable = T.intercalate "\n" $ mapMaybe checkState stList
    tokDefns = T.intercalate "\n" $ zipWith (\x n -> [interp|const TOKEN_TYPE_#{T.toUpper x} = #{n};|] :: Text) tokNames [1::Word ..]
    tokToStr = T.intercalate "\n" $ zipWith (\x n -> [interp|case #{n}: return '#{x}';|] :: Text) tokNames [1::Word ..]
    mkAct NoAction = "null"
    mkAct (Action act) = act
    checkChars :: (NE.NonEmpty CharPattern, Int) -> Text
    checkChars (charGroup, newSt) = [interp|
        if (#{charCond charGroup}) goto state_#{newSt};
        |]
    charCond = T.intercalate " || " . map charCond1 . NE.toList
    charCond1 :: CharPattern -> Text
    charCond1 (CChar c) = [interp|$curCh === #{tshow c}|]
    charCond1 (CRange c1 c2) = [interp|($curCh >= #{tshow c1} && $curCh <= #{tshow c2})|]
    charCond1 CAny = "true"
