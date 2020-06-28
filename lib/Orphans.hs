{-# LANGUAGE FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Orphans where

import Debug.Trace
import Lang
import Lexer.Types
import Parser.Types

instance {-# OVERLAPPABLE #-} Lang l => LexerWriter l where
  writeLexer _ _ _ _ = trace "Lexer not implemented" []

instance {-# OVERLAPPABLE #-} (Lang l, Parser p) => ParserWriter p l where
  writeParser _ _ _ _ = trace "Parser not implemented" []
