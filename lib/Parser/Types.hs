{-# LANGUAGE DataKinds, MultiParamTypeClasses, DeriveFunctor, DeriveTraversable, ExplicitForAll #-}
module Parser.Types where

import Data.Proxy
import Data.Text (Text)
import Grammar
import MonadTypes

data ParserOptions g = ParserOptions {
    parserOptionsName :: Text
  , parserOptionsBaseFileName :: FilePath
  , parserOptionsGrammarDefinition :: g
  } deriving (Functor, Foldable, Traversable)

class Parser parser where
  buildParser :: Monad m => Proxy parser -> Rules -> MyMonadT m parser

class ParserWriter parser lang where
  writeParser :: forall a. Proxy lang -> Text -> ParserOptions a -> parser -> [(FilePath,Text)]

makeParser :: (Parser parser, ParserWriter parser lang, Monad m) => Proxy lang -> Proxy parser
           -> ParserOptions Text -> MyMonadT m [(FilePath,Text)]
makeParser l p opts = do
  grammar <- traverse parse opts
  let Grammar gtop rules = parserOptionsGrammarDefinition grammar
  parser <- buildParser p rules
  return $ writeParser l gtop opts parser
