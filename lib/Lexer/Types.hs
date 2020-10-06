module Lexer.Types (LexerWriter(..), StateData(..), StateAttr(..)) where

import Data.Proxy
import Data.Text (Text)
import Regex.Parse
import Lexer.FA
import Data.List.NonEmpty (NonEmpty)
import Lang

class Lang lang => LexerWriter lang where
  writeLexer :: Proxy lang
    -> [(Int, StateData)] -- accepting states
    -> [Text] -- token names
    -> [(Int, (StateAttr, [(NonEmpty CharPattern, Int)]))] -- all states
    -> [(FilePath, Text)]
