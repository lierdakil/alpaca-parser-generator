{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Main where

import Lexer
import Parser.Recursive
import Parser.LL
import Parser.LR
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
    wrap "LR(0) parser" $ makeParser cpp lr0Parser ParserOptions{
        parserOptionsName = "LR0Parser"
      , parserOptionsBaseFileName = "lr0Parser"
      , parserOptionsGrammarDefinition = grammar
    }
    wrap "LR(1) parser" $ makeParser cpp lr1Parser ParserOptions{
        parserOptionsName = "LR1Parser"
      , parserOptionsBaseFileName = "lr1Parser"
      , parserOptionsGrammarDefinition = grammar
    }
    wrap "SLR parser" $ makeParser cpp slrParser ParserOptions{
        parserOptionsName = "SLRParser"
      , parserOptionsBaseFileName = "slrParser"
      , parserOptionsGrammarDefinition = grammar
    }
    wrap "LALR parser" $ makeParser cpp lalrParser ParserOptions{
        parserOptionsName = "LALRParser"
      , parserOptionsBaseFileName = "lalrParser"
      , parserOptionsGrammarDefinition = grammar
    }
    wrap "LALR parser" $ makeParser python lalrParser ParserOptions{
        parserOptionsName = "LALRParser"
      , parserOptionsBaseFileName = "lalrParser"
      , parserOptionsGrammarDefinition = grammar
    }
  return ()

wrap n m =
  censor (map (("Warning in "<>n<>": ")<>)) (writeFiles =<< m)
    `catchError`
    (tell . map (("Error in "<>n<>": ")<>))
writeFiles = mapM_ (lift . lift . lift . uncurry T.writeFile)
