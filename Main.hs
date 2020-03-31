module Main where

import Lexer
import ParseRecursive
import ParseLL
import ParseLR
import ParseSLR
import ParseLALR
import System.Environment

main :: IO ()
main = do
  [inputFile] <- getArgs
  input <- readFile inputFile
  let (lexic, _:grammar) = break (=="%%") $ lines input
      (tokens, lexer) = makeLexer (filter (not . null) lexic) CPP
  writeFile "lexer.h" lexer
  -- writeFile "parser.h" $ makeParser $ unlines grammar
  -- writeFile "parserLL.h" $ makeLLParser tokens $ unlines grammar
  -- writeFile "parserLR0.h" $ makeLRParser (Proxy :: Proxy LR0Point) "ParserLR0" tokens $ unlines grammar
  writeFile "parserLR1.h" $ makeLRParser (Proxy :: Proxy LR1Point) "ParserLR1" tokens $ unlines grammar
  writeFile "parserSLR.h" $ makeLRParser (Proxy :: Proxy SLRPoint) "ParserSLR" tokens $ unlines grammar
  writeFile "parserLALR.h" $ makeLALRParser "ParserLALR" tokens $ unlines grammar
