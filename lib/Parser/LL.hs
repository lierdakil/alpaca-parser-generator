module Parser.LL (
    module Parser.Types
  , llParser
  , LLParser
  ) where

import Parser.Types
import Parser.LL.Build
import Parser.LL.CPP()
import Parser.LL.CSharp()
import Parser.LL.Python()
import Data.Proxy

llParser :: Proxy LLParser
llParser = Proxy
