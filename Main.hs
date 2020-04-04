{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables, RankNTypes
  , ExistentialQuantification, AllowAmbiguousTypes,
  MultiParamTypeClasses, FlexibleInstances #-}
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
import Data.Proxy
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative hiding (Parser)
import qualified Options.Applicative as OA
import Lexer.Types()

runProgram (LangParserProxy lang parserMethod) parserName baseFileName inputFile = do
  input <- T.readFile inputFile
  let (lexicRaw, _:grammarLines) = break (=="%%") $ T.lines input
      rootdir = takeDirectory inputFile
      grammar = T.unlines grammarLines
      lexic = filter (not . T.null) lexicRaw
  setCurrentDirectory rootdir
  runInIO $ do
    writeFiles =<< makeLexer lang lexic
    wrap parserName $ makeParser lang parserMethod ParserOptions{
        parserOptionsName = parserName
      , parserOptionsBaseFileName = baseFileName
      , parserOptionsGrammarDefinition = grammar
    }

langReader = eitherReader go
  where go "cpp" = Right $ \(ParserProxy p) -> LangParserProxy cpp p
        go "c++" = Right $ \(ParserProxy p) -> LangParserProxy cpp p
        go "python" = Right $ \(ParserProxy p) -> LangParserProxy python p
        go "py" = Right $ \(ParserProxy p) -> LangParserProxy python p
        go _ = Left "Invalid value, allowed values: cpp, c++, python, py"

data ParserProxy = forall p. (Parser p, ParserWriter p CPP, ParserWriter p Python)
                => ParserProxy { unPP :: Proxy p }
data LangParserProxy = forall l p. (Parser p, ParserWriter p l, LexerWriter l)
                    => LangParserProxy (Proxy l) (Proxy p)

parserReader :: ReadM ParserProxy
parserReader = eitherReader go
  where go :: String -> Either String ParserProxy
        go "recursive" = Right $ ParserProxy recursiveParser
        go "ll1" = Right (ParserProxy llParser)
        go "lr0" = Right (ParserProxy lr0Parser)
        go "lr1" = Right (ParserProxy lr1Parser)
        go "slr" = Right (ParserProxy slrParser)
        go "lalr" = Right (ParserProxy lalrParser)
        go _ = Left "Invalid value, allowed values: recursive, ll1, lr0, lr1, slr, lalr"

parser :: OA.Parser (IO ())
parser = runProgram
  <$> (option langReader
       ( short 'l'
      <> long "lang"
      <> help "Target language, default cpp"
      <> metavar "cpp|python"
      <> value (\(ParserProxy p) -> LangParserProxy cpp p))
  <*> option parserReader
       ( short 'p'
      <> long "parser"
      <> help "Parser method, default lalr"
      <> metavar "recursive|ll1|lr0|lr1|slr|lalr"
      <> value (ParserProxy lalrParser)))
  <*> strOption
       ( short 'n'
      <> long "name"
      <> help "Parser class name, default \"Parser\""
      <> metavar "NAME"
      <> value "Parser")
  <*> strOption
       ( short 'b'
      <> long "basename"
      <> help "Parser output file base name, default \"parser\""
      <> metavar "FILENAME"
      <> value "parser")
  <*> strArgument
       ( help "Grammar input file"
      <> metavar "GRAMMARFILE" )


main :: IO ()
main = join $ execParser opts
  where
  opts = info (parser <**> helper)
    ( fullDesc
   <> progDesc "Anæmic Lexer and PArser Creation Algorithm"
   <> header "ALPACA" )

wrap n m =
  censor (map (("Warning in "<>n<>": ")<>)) (writeFiles =<< m)
    `catchError`
    (tell . map (("Error in "<>n<>": ")<>))
writeFiles = mapM_ (lift . lift . lift . uncurry T.writeFile)
