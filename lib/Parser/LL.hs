module Parser.LL (
    module Parser.Types
  , module Lang
  , llParser
  ) where

import Parser.Types
import Parser.LL.Build
import Parser.LL.CPP()
import Parser.LL.CSharp()
import Parser.LL.Python()
import Data.Proxy
import Lang

llParser :: Proxy LLParser
llParser = Proxy
