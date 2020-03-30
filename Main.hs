module Main where

import Lexer
import ParseRecursive
import ParseLL
import System.Environment

main :: IO ()
main = do
  [inputFile] <- getArgs
  input <- readFile inputFile
  let (lexic, _:grammar) = break (=="%%") $ lines input
      (tokens, lexer) = makeLexer (filter (not . null) lexic) CPP
  writeFile "lexer.h" lexer
  writeFile "parser.h" $ makeParser $ unlines grammar
  writeFile "parserLL.h" $ makeLLParser tokens $ unlines grammar
