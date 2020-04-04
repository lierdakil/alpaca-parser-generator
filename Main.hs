{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Main where

import Lexer
import Parser.Recursive
import Parser.LL
-- import ParseLR
-- import ParseSLR
-- import ParseLALR
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
  let (lexicRaw, _:grammarLines) = break (=="%%") $ T.lines input
      rootdir = takeDirectory inputFile
      grammar = T.unlines grammarLines
      lexic = filter (not . T.null) lexicRaw
  setCurrentDirectory rootdir
  runInIO $ do
    writeFiles =<< makeLexer cpp lexic
    writeFiles =<< makeLexer python lexic
    wrap "recursive parser" $ makeParser cpp recursiveParser ParserOptions{
        parserOptionsName = "Parser"
      , parserOptionsBaseFileName = "recursiveParser"
      , parserOptionsGrammarDefinition = grammar
    }
    wrap "recursive parser" $ makeParser python recursiveParser ParserOptions{
        parserOptionsName = "Parser"
      , parserOptionsBaseFileName = "recursiveParser"
      , parserOptionsGrammarDefinition = grammar
    }
    wrap "LL(1) parser" $ makeParser cpp llParser ParserOptions{
        parserOptionsName = "LLParser"
      , parserOptionsBaseFileName = "llParser"
      , parserOptionsGrammarDefinition = grammar
    }
    wrap "LL(1) parser" $ makeParser python llParser ParserOptions{
        parserOptionsName = "LLParser"
      , parserOptionsBaseFileName = "llParser"
      , parserOptionsGrammarDefinition = grammar
    }
    -- wrap "LL(1) parser" $ makeLLParser grammar "llParser" tokens
    -- wrap "LR(0) parser" $ makeLRParser (Proxy :: Proxy LR0Point) grammar "lr0Parser" "ParserLR0" tokens
    -- wrap "LR(1) parser" $ makeLRParser (Proxy :: Proxy LR1Point) grammar "lr1Parser" "ParserLR1" tokens
    -- wrap "SLR parser" $ makeLRParser (Proxy :: Proxy SLRPoint) grammar "slrParser" "ParserSLR" tokens
    -- wrap "LALR parser" $ makeLALRParser grammar "lalrParser" "ParserLALR" tokens
  return ()

wrap n m =
  censor (map (("Warning in "<>n<>": ")<>)) (writeFiles =<< m)
    `catchError`
    (tell . map (("Error in "<>n<>": ")<>))
writeFiles = mapM_ (lift . lift . lift . uncurry T.writeFile)
