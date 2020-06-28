{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Lexer.PHP() where

import qualified Data.List.NonEmpty as NE
import Regex.Parse
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
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

    public function __construct(string $input, int $curChIx, bool $debug)
    {
        $this->input = $input;
        $this->curChIx = $curChIx;
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
        $curSt = 0;
        while ($curSt >= 0) {
        if (in_array($curSt, [#{T.intercalate "," $ map (tshow . fst) accSt}])) {
                $lastAccChIx = $this->curChIx;
                $accSt = $curSt;
            }

            if ($this->curChIx >= strlen($this->input))
                break;

            $curCh = $this->input[$this->curChIx];
            $this->curChIx+=1;
            switch($curSt) {
                #{indent 4 transTable}
            }
            break;
        }

        $lastReadChIx = $this->curChIx;
        $this->curChIx = $lastAccChIx;
        $text = substr($this->input, $startChIx, $lastAccChIx);
        switch($accSt) {
          #{indent 3 returnResult}
        }

        if ($this->curChIx >= strlen($this->input)) {
            if ($this->debug)  printf('Got EOF while lexing "%s"', $text);
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
            if ($this->debug) printf('Lexed token #{name}: "%s"', $text);
            return [self::TOKEN_TYPE_#{T.toUpper name}, #{mkAct act}];
      |]
    returnResult1 (st, (Nothing, _)) = [interp|
        case #{st}:
            if ($this->debug) printf('Skipping state #{tshow st}: "%s"', $text);
            return $this->getNextToken();
      |]
    checkState :: (Int, (a, [(NE.NonEmpty CharPattern, Int)])) -> Maybe Text
    checkState (_, (_, [])) = Nothing
    checkState (curSt, (_, charTrans)) = Just [interp|
      case #{tshow curSt}:
        #{indent 1 $ T.intercalate " else " (map checkChars charTrans)}
        break;
      |]
    transTable = T.intercalate "\n" $ mapMaybe checkState stList
    tokDefns = T.intercalate "\n" $ zipWith (\x n -> [interp|const TOKEN_TYPE_#{T.toUpper x} = #{n};|] :: Text) tokNames [1::Word ..]
    tokToStr = T.intercalate "\n" $ zipWith (\x n -> [interp|case #{n}: return '#{x}';|] :: Text) tokNames [1::Word ..]
    mkAct NoAction = "null"
    mkAct (Action act) = act
    checkChars :: (NE.NonEmpty CharPattern, Int) -> Text
    checkChars (charGroup, newSt) = [interp|
        if (#{charCond charGroup}) {
            $curSt = #{newSt};
            continue;
        }
      |]
    charCond = T.intercalate " || " . map charCond1 . NE.toList
    charCond1 :: CharPattern -> Text
    charCond1 (CChar c) = [interp|$curCh === #{tshow c}|]
    charCond1 (CRange c1 c2) = [interp|($curCh >= #{tshow c1} && $curCh <= #{tshow c2})|]
    charCond1 CAny = "true"
