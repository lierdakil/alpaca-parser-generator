module Parser.Recursive (
    module Parser.Types
  , module Lang
  , recursiveParser
  ) where

import Parser.Types
import Parser.Recursive.Build
import Parser.Recursive.CPP()
import Parser.Recursive.Python()
import Parser.Recursive.CSharp()
import Data.Proxy
import Lang

recursiveParser :: Proxy RecursiveParser
recursiveParser = Proxy
