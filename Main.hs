{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
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
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  [inputFile] <- getArgs
  input <- T.readFile inputFile
  let (lexic, _:grammarLines) = break (=="%%") $ T.lines input
      rootdir = takeDirectory inputFile
      grammar = T.unlines grammarLines
  setCurrentDirectory rootdir
  runInIO $ do
    (debug, dfa) <- makeDFA (filter (not . T.null) lexic)
    writeFiles debug
    (tokens, lexer) <- writeLexer dfa CPP
    writeFiles lexer
    wrap "recursive parser" $ makeParser grammar "recursiveParser"
    wrap "LL(1) parser" $ makeLLParser grammar "llParser" tokens
    wrap "LR(0) parser" $ makeLRParser (Proxy :: Proxy LR0Point) grammar "lr0Parser" "ParserLR0" tokens
    wrap "LR(1) parser" $ makeLRParser (Proxy :: Proxy LR1Point) grammar "lr1Parser" "ParserLR1" tokens
    wrap "SLR parser" $ makeLRParser (Proxy :: Proxy SLRPoint) grammar "slrParser" "ParserSLR" tokens
    wrap "LALR parser" $ makeLALRParser grammar "lalrParser" "ParserLALR" tokens
  return ()

wrap n m =
  censor (map (("Warning in "<>n<>": ")<>)) (writeFiles =<< m)
    `catchError`
    (tell . map (("Error in "<>n<>": ")<>))
writeFiles = mapM_ (lift . lift . uncurry T.writeFile)
