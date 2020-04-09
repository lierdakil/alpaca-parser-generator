{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables, RankNTypes
  , ExistentialQuantification, AllowAmbiguousTypes,
  MultiParamTypeClasses, FlexibleInstances #-}
module Main where

import Lexer
import Parser.Recursive
import Parser.LL
import Parser.LR
import MonadTypes

import System.Environment
import System.FilePath
import System.Directory
import Data.Proxy
import Control.Monad
import Data.List
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative hiding (Parser)
import qualified Options.Applicative as OA

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
  where go x | Just f <- find ((map toLower x `elem`) . fst) langTbl = Right $ snd f
        go _ = Left $ "Invalid value, allowed values: " <> intercalate ", " (concatMap fst langTbl)

langTbl = [
    (ncpp, \(ParserProxy p) -> LangParserProxy cpp p)
  , (ncs , \(ParserProxy p) -> LangParserProxy csharp p)
  , (npy , \(ParserProxy p) -> LangParserProxy python p)
  ]
  where
  ncpp = ["cpp", "c++"]
  ncs = ["c#", "csharp", "cs"]
  npy = ["python", "py"]

data ParserProxy = forall p.
  ( Parser p
  , ParserWriter p CPP
  , ParserWriter p CSharp
  , ParserWriter p Python
  ) => ParserProxy { unPP :: Proxy p }
data LangParserProxy = forall l p. (Parser p, ParserWriter p l, LexerWriter l)
                    => LangParserProxy (Proxy l) (Proxy p)

parserReader :: ReadM ParserProxy
parserReader = eitherReader go
  where go :: String -> Either String ParserProxy
        go x | Just pp <- lookup (map toLower x) parsTbl = Right pp
        go _ = Left $ "Invalid value, allowed values: " <> intercalate ", " (map fst parsTbl)

parsTbl = [
    ("recursive", ParserProxy recursiveParser)
  , ("rec"      , ParserProxy recursiveParser)
  , ("ll1"      , ParserProxy llParser       )
  , ("lr0"      , ParserProxy lr0Parser      )
  , ("lr1"      , ParserProxy lr1Parser      )
  , ("slr"      , ParserProxy slrParser      )
  , ("lalr"     , ParserProxy lalrParser     )
  ]

parser :: OA.Parser (IO ())
parser = runProgram
  <$> (option langReader
       ( short 'l'
      <> long "lang"
      <> help "Target language, default cpp"
      <> metavar (intercalate "|" (concatMap fst langTbl))
      <> value (\(ParserProxy p) -> LangParserProxy cpp p))
  <*> option parserReader
       ( short 'p'
      <> long "parser"
      <> help "Parser method, default lalr"
      <> metavar (intercalate "|" (map fst parsTbl))
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
   <> progDesc "ALPACA generates mostly human-readable, if somewhat\
      \ inefficient, simple parsers in multiple target languages."
   <> header "ALPACA - Anemic Lexer and PArser Creation Algorithm" )

wrap n m =
  censor (map (("Warning in "<>n<>": ")<>)) (writeFiles =<< m)
    `catchError`
    (tell . map (("Error in "<>n<>": ")<>))
writeFiles = mapM_ (lift . lift . lift . uncurry T.writeFile)
