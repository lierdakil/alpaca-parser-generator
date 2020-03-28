module Main where

import Lexer
import ParseRecursive
import System.Environment

main :: IO ()
main = do
  [inputFile] <- getArgs
  input <- readFile inputFile
  let (tokens, _:grammar) = break (=="%%") $ lines input
  writeFile "lexer.h" $ makeLexer (filter (not . null) tokens) CPP
  writeFile "parser.h" $ makeParser $ unlines grammar
