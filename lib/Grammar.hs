{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Grammar (
    Symbol(..)
  , Rule(..)
  , Alt
  , RulesMap
  , Rules
  , Grammar(..)
  , BodyWithAction(..)
  , Assoc
  , AssocType(..)
  , parse
  , showBody
  , showSymbol
  , mkRulesMap
  , first
  , follow
  , isLeftRecursive
  ) where

import Grammar.Lex
import Grammar.Parse
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Control.Monad.State
import Control.Monad.Writer.Class
import MonadTypes
import Data.Text (Text)
import qualified Data.Text as T

type Alt = BodyWithAction
type RulesMap = M.Map Text (NonEmpty Alt)
type Rules = NonEmpty Rule

parse :: Monad m => Text -> MyMonadT m Grammar
parse = fmap grammar . scan

scan :: Monad m => Text -> MyMonadT m [Token]
scan s = either (throwError . pure . T.pack) return $ runAlex (T.unpack s) go
  where
  go = do
    tok <- alexMonadScan
    (tok :) <$> if tok /= TEOF
                then go
                else return []

showBody :: [Symbol] -> Text
showBody = T.unwords . map showSymbol

showSymbol :: Symbol -> Text
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
    firstT = S.unions (map (first'' . bwaBody) $ maybe [] NE.toList $ M.lookup t r)
  first' _ (TermEof:_) = S.singleton $ Just TermEof
  first' _ (Term t:_) = S.singleton $ Just (Term t)
  first' _ [] = S.singleton Nothing

isLeftRecursive :: MonadWriter [Text] f => RulesMap -> f Bool
isLeftRecursive r = or <$> mapM isRuleLeftRec (M.toList r)
  where
  isRuleLeftRec (h, alts) = or <$> mapM (isBodyLeftRec h . bwaBody) (NE.toList alts)

  isBodyLeftRec h xs
    | isLeftRec h xs = do
        tell ["Rule " <> h <> " -> " <> showBody xs <> " is left-recursive."]
        return True
    | otherwise = return False

  isLeftRec :: Text -> [Symbol] -> Bool
  isLeftRec nt = first' (S.singleton nt)
    where
    first' seen (NonTerm t:xs)
      | t `S.member` seen = True
      | Nothing `S.member` first r [NonTerm t]
      = first'' xs
      | otherwise = firstT
      where
      first'' = first' $ S.insert t seen
      firstT = any (first'' . bwaBody) $ maybe [] NE.toList $ M.lookup t r
    first' _ (TermEof:_) = False
    first' _ (Term _:_) = False
    first' _ [] = False

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
    oneRule h = fmap S.unions . mapM (oneAlt h . bwaBody)
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
