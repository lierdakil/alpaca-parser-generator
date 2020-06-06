module Parser.Recursive (
    module Parser.Types
  , recursiveParser
  , RecursiveParser
  ) where

import Parser.Types
import Parser.Recursive.Build
import Parser.Recursive.CPP()
import Parser.Recursive.Python()
import Parser.Recursive.JS()
import Parser.Recursive.CSharp()
import Data.Proxy

recursiveParser :: Proxy RecursiveParser
recursiveParser = Proxy
