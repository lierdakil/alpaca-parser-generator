{-# LANGUAGE FlexibleContexts #-}
module Main where

import Lexer
import ParseRecursive
import ParseLL
import ParseLR
import ParseSLR
import ParseLALR
import System.Environment
import System.FilePath
import System.Directory
import MonadTypes

main :: IO ()
main = do
  [inputFile] <- getArgs
  input <- readFile inputFile
  let (lexic, _:grammar) = break (=="%%") $ lines input
      rootdir = takeDirectory inputFile
  setCurrentDirectory rootdir
  runInIO $ do
    (debug, dfa) <- makeDFA (filter (not . null) lexic)
    writeFiles debug
    (tokens, lexer) <- writeLexer dfa CPP
    writeFiles lexer
    wrap "recursive parser" $ makeParser (unlines grammar) "recursiveParser"
    wrap "LL(1) parser" $ makeLLParser (unlines grammar) "llParser" tokens
    wrap "LR(0) parser" $ makeLRParser (Proxy :: Proxy LR0Point) (unlines grammar) "lr0Parser" "ParserLR0" tokens
    wrap "LR(1) parser" $ makeLRParser (Proxy :: Proxy LR1Point) (unlines grammar) "lr1Parser" "ParserLR1" tokens
    wrap "SLR parser" $ makeLRParser (Proxy :: Proxy SLRPoint) (unlines grammar) "slrParser" "ParserSLR" tokens
    wrap "LALR parser" $ makeLALRParser (unlines grammar) "lalrParser" "ParserLALR" tokens
  return ()

wrap n m = (writeFiles =<< censor (map (("Warning in "<>n<>": ")<>)) m)
  `catchError`
  (lift . lift . putStrLn . unlines . map (("Error in "<>n<>": ")<>))
writeFiles = mapM_ (lift . lift . uncurry writeFile)
