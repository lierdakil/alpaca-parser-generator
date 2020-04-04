module Lexer.Types where

import Data.Proxy
import Data.Text (Text)
import Regex.Parse
import Lexer.FA
import Data.List.NonEmpty (NonEmpty)

class LexerWriter lang where
  writeLexer :: Proxy lang
    -> [(Int, (Maybe Text, Action))] -- accepting states
    -> [Text] -- token names
    -> [(Int, (StateAttr, [(NonEmpty CharPattern, Int)]))] -- all states
    -> [(FilePath, Text)]
