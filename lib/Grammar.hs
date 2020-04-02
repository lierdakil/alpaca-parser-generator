{-# LANGUAGE FlexibleContexts #-}
module Grammar (
    Symbol(..)
  , Rule(..)
  , Alt
  , RulesMap
  , Rules
  , parse
  , showBody
  , showSymbol
  , mkRulesMap
  , first
  , follow
  ) where

import GrammarLex
import GrammarParse
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import qualified Data.List.NonEmpty as NE
import Control.Monad.State

type Alt = ([Symbol], Maybe String)
type RulesMap = M.Map String (NE.NonEmpty Alt)
type Rules = NE.NonEmpty Rule

parse :: String -> Rules
parse = grammar . scan

scan :: String -> [Token]
scan s = either error id $ runAlex s go
  where
  go = do
    tok <- alexMonadScan
    (tok :) <$> if tok /= TEOF
                then go
                else return []

showBody :: [Symbol] -> String
showBody = unwords . map showSymbol

showSymbol :: Symbol -> String
showSymbol TermEof = "%eof"
showSymbol (Term t) = t
showSymbol (NonTerm t) = t

mkRulesMap :: Rules -> RulesMap
mkRulesMap = M.fromList . map ruleToTuple . NE.toList
  where ruleToTuple (Rule h alts) = (h, alts)

first :: RulesMap -> [Symbol] -> S.Set (Maybe Symbol)
first r = first' S.empty
  where
  first' seen (NonTerm t:xs)
    | t `S.member` seen = S.empty
    | Nothing `S.member` firstT
    = S.delete Nothing firstT `S.union` first'' xs
    | otherwise = firstT
    where
    first'' = first' $ S.insert t seen
    firstT = S.unions (map (first'' . fst) $ maybe [] NE.toList $ M.lookup t r)
  first' _ (TermEof:_) = S.singleton $ Just TermEof
  first' _ (Term t:_) = S.singleton $ Just (Term t)
  first' _ [] = S.singleton Nothing

follow :: RulesMap -> Symbol -> S.Set Symbol
follow r t@(NonTerm _) = evalState (follow' t) S.empty
  where
  rules = M.toList r
  follow' x = do
    seen <- gets (S.member x)
    if not seen
    then do
      modify (S.insert x)
      S.unions <$> mapM (uncurry oneRule) rules
    else return S.empty
    where
    oneRule h = fmap S.unions . mapM (oneAlt h . fst)
    oneAlt h b
      | _:beta <- dropWhile (/=x) b = go beta
      | otherwise = return S.empty
      where
      go beta
        | Nothing `S.member` firstBeta
        = S.union <$> noEpsilon (S.delete Nothing firstBeta)
                  <*> follow' (NonTerm h)
        | otherwise = noEpsilon firstBeta
        where firstBeta = first r beta
              noEpsilon f = (S.mapMonotonic fromJust f `S.union`) <$> oneAlt h beta
follow _ _ = S.empty
