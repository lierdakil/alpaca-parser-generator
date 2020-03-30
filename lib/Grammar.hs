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

type Alt = ([Symbol], Maybe String)
type RulesMap = M.Map String [Alt]
type Rules = [Rule]

parse :: String -> [Rule]
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
mkRulesMap = M.fromList . map ruleToTuple
  where ruleToTuple (Rule h alts) = (h, alts)

first :: RulesMap -> [Symbol] -> S.Set (Maybe Symbol)
first r = first'
  where
  first' (NonTerm t:xs)
    | Nothing `S.member` firstT
    = S.delete Nothing firstT `S.union` first r (filter (/= NonTerm t) xs)
    | otherwise = firstT
    where
    firstT = S.unions (map (first' . fst) $ fromMaybe [] $ M.lookup t r)
  first' (TermEof:_) = S.singleton $ Just TermEof
  first' (Term t:_) = S.singleton $ Just (Term t)
  first' [] = S.singleton Nothing

follow :: RulesMap -> Symbol -> S.Set Symbol
follow r t@(NonTerm _) = follow' S.empty t
  where
  rules = M.toList r
  follow' seen x
    | x `elem` seen = S.empty
    | otherwise = S.unions $ map (uncurry oneRule) rules
    where
    oneRule h = S.unions . map (oneAlt h . fst)
    oneAlt h b
      | _:beta <- dropWhile (/=x) b = go beta
      | otherwise = S.empty
      where
      go beta
        | Nothing `S.member` firstBeta
        = noEpsilon (S.delete Nothing firstBeta)
          `S.union` follow' (S.insert x seen) (NonTerm h)
        | otherwise = noEpsilon firstBeta
        where firstBeta = first r beta
              noEpsilon f = S.mapMonotonic fromJust f `S.union` oneAlt h beta
follow _ _ = S.empty
