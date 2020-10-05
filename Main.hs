{-# LANGUAGE FlexibleContexts, OverloadedStrings, ExistentialQuantification #-}
module Main where

import Lexer
import Orphans()
import Parser.Recursive
import Parser.LL
import Parser.LR

import System.FilePath
import System.Directory
import Data.Proxy
import Control.Monad
import Data.List
import Data.Char
import Data.Bifunctor
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative hiding (Parser)
import qualified Options.Applicative as OA
import Data.Version
import Paths_alpaca_parser_generator

type MainProgram = Bool -> Text -> FilePath -> FilePath -> IO ()

runProgram :: (LexerWriter lang, ParserWriter parser lang) =>
              Proxy lang -> Proxy parser -> MainProgram
runProgram lang parserMethod debugLexer parserName baseFileName inputFile = do
  input <- T.readFile inputFile
  let (lexicRaw, grammarLines) = second (drop 1) . break ((=="%%") . T.filter (/='\r')) $ T.lines input
      rootdir = takeDirectory inputFile
      grammar = T.unlines grammarLines
      lexic = filter (not . T.null) lexicRaw
  setCurrentDirectory rootdir
  runInIO $ do
    writeFiles =<< makeLexer lang debugLexer lexic
    wrap parserName $ makeParser lang parserMethod ParserOptions{
        parserOptionsName = parserName
      , parserOptionsBaseFileName = baseFileName
      , parserOptionsGrammarDefinition = grammar
    }

enumReader :: [([String], a)] -> ReadM a
enumReader tbl = eitherReader go
  where go x | Just f <- find ((map toLower x `elem`) . fst) tbl
             = Right $ snd f
        go _ = Left $ "Invalid value, allowed values: "
             <> intercalate ", " (concatMap fst tbl)

langTbl :: [([String], ParserProxy -> MainProgram)]
langTbl = [
    (ncpp, \(ParserProxy p) -> runProgram cpp p)
  , (ncs , \(ParserProxy p) -> runProgram csharp p)
  , (npy , \(ParserProxy p) -> runProgram python p)
  , (njs , \(ParserProxy p) -> runProgram js p)
  ]
  where
  ncpp = ["cpp", "c++"]
  ncs = ["c#", "csharp", "cs"]
  npy = ["python", "py"]
  njs = ["js", "javascript"]

data ParserProxy = forall p.
  ( Parser p
  , ParserWriter p CPP
  , ParserWriter p CSharp
  , ParserWriter p Python
  , ParserWriter p JS
  ) => ParserProxy { unPP :: Proxy p }

parsTbl :: [([String], ParserProxy)]
parsTbl = [
    (["recursive", "rec"], ParserProxy recursiveParser)
  , (["ll1"]             , ParserProxy llParser       )
  , (["lr0"]             , ParserProxy lr0Parser      )
  , (["lr1"]             , ParserProxy lr1Parser      )
  , (["slr"]             , ParserProxy slrParser      )
  , (["lalr"]            , ParserProxy lalrParser     )
  ]

parser :: OA.Parser (IO ())
parser = (option (enumReader langTbl)
       ( short 'l'
      <> long "lang"
      <> help "Target language, default cpp"
      <> metavar (intercalate "|" (concatMap fst langTbl))
      <> value (\(ParserProxy p) -> runProgram cpp p))
  <*> option (enumReader parsTbl)
       ( short 'p'
      <> long "parser"
      <> help "Parser method, default lalr"
      <> metavar (intercalate "|" (concatMap fst parsTbl))
      <> value (ParserProxy lalrParser)))
  <*> switch
       ( long "debug-lexer"
      <> help "Output lexer finite automata graphs in GraphViz format")
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

versionFlag :: OA.Parser (IO () -> IO ())
versionFlag = infoOption
  ( showVersion version )
  (  short 'v'
  <> long "version"
  <> help "Show version")

main :: IO ()
main = join $ execParser opts
  where
  opts = info (parser <**> helper <**> versionFlag)
    ( fullDesc
   <> progDesc "ALPACA generates mostly human-readable, if somewhat\
      \ inefficient, simple parsers in multiple target languages."
   <> header "ALPACA - Anemic Lexer and PArser Creation Algorithm" )

wrap :: T.Text -> MyMonadT IO [(FilePath, Text)] -> MyMonadT IO ()
wrap n m =
  censor (map (("Warning in "<>n<>": ")<>)) (writeFiles =<< m)
    `catchError`
    (tell . map (("Error in "<>n<>": ")<>))

writeFiles :: [(FilePath, Text)] -> MyMonadT IO ()
writeFiles = mapM_ (lift . lift . lift . uncurry T.writeFile)
