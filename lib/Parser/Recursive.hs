module Parser.Recursive (
    module Parser.Types
  , recursiveParser
  , RecursiveParser
  ) where

import Parser.Types
import Parser.Recursive.Build
import Parser.Recursive.Langs.CPP()
import Parser.Recursive.Langs.Python()
import Parser.Recursive.Langs.JS()
import Parser.Recursive.Langs.CSharp()
import Data.Proxy

recursiveParser :: Proxy RecursiveParser
recursiveParser = Proxy
