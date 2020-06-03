{-# LANGUAGE DataKinds, MultiParamTypeClasses, DeriveTraversable, ExplicitForAll #-}
module Parser.Types (
    module Parser.Types
  , module MonadTypes
  , module Lang
  ) where

import Data.Proxy
import Data.Text (Text)
import Grammar
import MonadTypes
import Lang

data ParserOptions g = ParserOptions {
    parserOptionsName :: Text
  , parserOptionsBaseFileName :: FilePath
  , parserOptionsGrammarDefinition :: g
  } deriving (Functor, Foldable, Traversable)

class Parser parser where
  buildParser :: Monad m => Proxy parser -> ParserOptions Rules -> MyMonadT m ([(FilePath,Text)], parser)

class (Parser parser, Lang lang) => ParserWriter parser lang where
  writeParser :: Proxy lang -> Tops -> ParserOptions a -> parser -> [(FilePath,Text)]

makeParser :: (Parser parser, ParserWriter parser lang, Monad m) => Proxy lang -> Proxy parser
           -> ParserOptions Text -> MyMonadT m [(FilePath,Text)]
makeParser l p opts = do
  grammar <- traverse parse opts
  let Grammar gtop rules = parserOptionsGrammarDefinition grammar
  (debug, parser) <- buildParser p grammar{parserOptionsGrammarDefinition=rules}
  return $ debug <> writeParser l gtop opts parser
