module Parser.LR (
    module Parser.Types
  , module Lang
  , lr0Parser
  , lr1Parser
  , slrParser
  , lalrParser
  ) where

import Parser.Types
import Parser.LR.Point
import Parser.LR.Build
import Parser.LR.LALR
import Parser.LR.SLR
import Parser.LR.CPP()
import Parser.LR.Python()
import Data.Proxy
import Lang

lr1Parser :: Proxy (LRParser LR1Point)
lr1Parser = Proxy

lr0Parser :: Proxy (LRParser LR0Point)
lr0Parser = Proxy

slrParser :: Proxy (LRParser SLRPoint)
slrParser = Proxy

lalrParser :: Proxy (LRParser LALRPoint)
lalrParser = Proxy
