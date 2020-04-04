{-# LANGUAGE FlexibleContexts, OverloadedStrings, QuasiQuotes, RecordWildCards #-}
module Parser.LL.Build where

import Grammar
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M
import Text.Layout.Table
import Control.Monad
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import MonadTypes
import Utils
import Data.Text (Text)
import qualified Data.Text as T
import Parser.Types
import Control.Monad.State

type Table = M.Map (Symbol, [Symbol]) (S.Set Symbol, Maybe Text)

data LLParser = LLParser {
    llStartSymbol :: Symbol
  , llTerminals :: [Symbol]
  , llNonTerminals :: [Symbol]
  , llActions :: M.Map (Symbol,Symbol) ([Symbol], Maybe Text)
  }

instance Parser LLParser where
  --buildParser :: Monad m => Proxy parser -> Grammar -> MyMonadT m parser
  buildParser _ = buildLLParser

buildLLParser :: Monad m => ParserOptions Rules -> MyMonadT m ([(FilePath, Text)],LLParser)
buildLLParser ParserOptions{..} = do
  tokens <- get
  lr <- isLeftRecursive r
  when lr $ throwError ["LL(1) parser can not handle left-recursive grammar"]
  cells <- M.fromList . concat <$> mapM (fmap catMaybes . forM tokens . writeCell) nonTerms
  let debug = [(parserOptionsBaseFileName <> ".txt", printTable t)]
  return (debug, LLParser{
      llStartSymbol = startSymbol
    , llTerminals = tokens
    , llNonTerminals = map NonTerm nonTerms
    , llActions = cells
    })
  where
  rules = parserOptionsGrammarDefinition
  writeCell nonterm term
    | Just (act, b:rest) <- M.lookup (NonTerm nonterm, term) tt
    = do
        unless (null rest) $
          tell [[interp|LL parser has multiple rules in the same cell: #{tshow (b:rest)}|],
                [interp|Choosing #{tshow b}|]]
        return $ Just ((term, NonTerm nonterm), (b, act))
    | otherwise = return Nothing
  tt = M.fromListWith (liftM2 (++)) . foldMap r2t $ M.toList t
  r2t ((nt, b), (ts, act)) = map (\term -> ((nt, term), (act, [b]))) $ S.toList ts
  startSymbol = let (Rule h _ :| _) = rules in NonTerm h
  r = mkRulesMap rules
  t = buildTable r
  nonTerms = M.keys r

printTable :: Table -> Text
printTable t = T.pack $ tableString (repeat def) unicodeS (titlesH $ map T.unpack titles) rows
  -- unlines [tshow titles, tshow rows]
  where
  nonTerms = S.toList . S.fromList $ map (fst . fst) tl
  terms = S.toList $ S.unions $ map fst $ M.elems t
  titles = "" : map showSymbol nonTerms
  tl = M.toList t
  tt = M.fromListWith (++) $ foldMap r2t tl
  r2t ((nt, b), (ts, _)) = map (\term -> ((nt, term), [b])) $ S.toList ts
  rows = map makeRow terms
  makeRow term = colsAllG top $ [T.unpack $ showSymbol term] : map (makeCell term) nonTerms
  makeCell term nonTerm
    | Just cell <- M.lookup (nonTerm, term) tt = map showBody' cell
    | otherwise = []
  showBody' [] = "ε"
  showBody' x = T.unpack $ showBody x

buildTable :: RulesMap -> Table
buildTable r = M.fromListWith (<>) $ foldMap (uncurry oneRule) rules
  where
  rules = M.toList r
  oneRule h = map (uncurry (oneAlt h)) . NE.toList
  oneAlt h alpha act = ((NonTerm h, alpha), (followSet `S.union` firstAlphaNoEps, act))
    where
    followSet
      | Nothing `S.member` firstAlpha
      = follow r (NonTerm h)
      | otherwise = S.empty
    firstAlpha = first r alpha
    firstAlphaNoEps = S.map fromJust (S.delete Nothing firstAlpha)

indexTable :: LLParser -> ([[Maybe Word]], M.Map ([Symbol], Maybe Text) Word)
indexTable LLParser{..}
    = flip runState M.empty
    $ mapM (\nt -> mapM (makeCell nt) llTerminals) llNonTerminals
  where
  makeCell nt t
    | Just x@(b, ma) <- M.lookup (t, nt) llActions
    = do
        am <- get
        let (ai, am')
               | Just i <- M.lookup x am = (i, am)
               | otherwise = let s = fromIntegral (M.size am) in (s, M.insert x s am)
        put am'
        return $ Just ai
    | otherwise = return Nothing
