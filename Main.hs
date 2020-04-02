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
      writeFiles = mapM_ (lift . lift . uncurry writeFile)
      rootdir = takeDirectory inputFile
  setCurrentDirectory rootdir
  runInIO $ do
    (debug, dfa) <- makeDFA (filter (not . null) lexic)
    writeFiles debug
    (tokens, lexer) <- writeLexer dfa CPP
    writeFiles lexer
    (writeFiles =<< makeParser (unlines grammar) "recursiveParser")
      `catchError` (lift . lift . putStrLn)
    writeFiles =<< makeLLParser (unlines grammar) "llParser" tokens
    -- writeFiles =<< makeLRParser (unlines grammar)
  return ()
  -- writeFile "parserLL.h" $ makeLLParser tokens $ unlines grammar
  -- writeFile "parserLR0.h" $ makeLRParser (Proxy :: Proxy LR0Point) "ParserLR0" tokens $ unlines grammar
  -- writeFile "parserLR1.h" $ makeLRParser (Proxy :: Proxy LR1Point) "ParserLR1" tokens $ unlines grammar
  -- writeFile "parserSLR.h" $ makeLRParser (Proxy :: Proxy SLRPoint) "ParserSLR" tokens $ unlines grammar
  -- writeFile "parserLALR.h" $ makeLALRParser "ParserLALR" tokens $ unlines grammar
