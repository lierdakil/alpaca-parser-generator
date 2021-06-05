module Parser.LL (
    module Parser.Types
  , llParser
  , LLParser
  ) where

import Parser.Types
import Parser.LL.Build
import Parser.LL.Langs.CPP()
import Parser.LL.Langs.CSharp()
import Parser.LL.Langs.Python()
import Parser.LL.Langs.JS()
import Data.Proxy

llParser :: Proxy LLParser
llParser = Proxy
