module Parser.LR (
    module Parser.Types
  , lr0Parser
  , lr1Parser
  , slrParser
  , lalrParser
  , LR1Parser
  , LR0Parser
  , SLRParser
  , LALRParser
  ) where

import Parser.Types
import Parser.LR.Point
import Parser.LR.Build
import Parser.LR.LALR
import Parser.LR.SLR
import Parser.LR.CPP()
import Parser.LR.CSharp()
import Parser.LR.Python()
import Data.Proxy

type LR1Parser = LRParser LR1Point
type LR0Parser = LRParser LR0Point
type SLRParser = LRParser SLRPoint
type LALRParser = LRParser LALRPoint

lr1Parser :: Proxy LR1Parser
lr1Parser = Proxy

lr0Parser :: Proxy LR0Parser
lr0Parser = Proxy

slrParser :: Proxy SLRParser
slrParser = Proxy

lalrParser :: Proxy LALRParser
lalrParser = Proxy
