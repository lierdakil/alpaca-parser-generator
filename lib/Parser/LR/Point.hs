{-# LANGUAGE FlexibleContexts
           , TypeFamilies
           , ScopedTypeVariables
           , PatternSynonyms
           , OverloadedStrings
           #-}
module Parser.LR.Point where

import Grammar
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T
import MonadTypes


pattern ExtendedStartRule :: Text
pattern ExtendedStartRule = "%S"

extendedStartRuleBody :: Text -> [Symbol]
extendedStartRuleBody start = [NonTerm start, TermEof]

type LRState p = S.Set p
type LRAutomaton p = (LRState p, M.Map (LRState p, Symbol) (LRState p))

class (Ord a, Ord (Lookahead a)) => LRPoint a where
  type family Lookahead a
  lr0 :: a -> LR0Point
  modLr0 :: a -> LR0Point -> a
  pointLookahead :: a -> S.Set (Lookahead a)
  modLookahead :: a -> S.Set (Lookahead a) -> a
  startPoint :: Text -> a
  makeFirstPoint :: RulesMap -> a -> Text -> [Symbol] -> [Symbol] -> Maybe Text -> Maybe Assoc -> a
  lookaheadMatches :: a -> Symbol -> Bool
  showLookahead :: a -> Text
  pointAssoc :: a -> Maybe Assoc
  pointAssoc = lr0PointAssoc . lr0
  postprocess :: Monad m => LRAutomaton a -> MyMonadT m (LRAutomaton a)
  postprocess = return

data LR0Point = LR0Point {
    lr0PointAction :: Maybe Text
  , lr0PointHead :: Text
  , lr0PointLeft :: [Symbol]
  , lr0PointRight :: [Symbol]
  , lr0PointAssoc :: Maybe Assoc
  } deriving (Show, Eq, Ord)

instance LRPoint LR0Point where
  type Lookahead LR0Point = Void
  lr0 = id
  modLr0 _ x = x
  pointLookahead = const S.empty
  modLookahead x _ = x
  startPoint rule = LR0Point Nothing ExtendedStartRule [] (extendedStartRuleBody rule) Nothing
  lookaheadMatches _ _ = True
  makeFirstPoint _ _ h b _ act = LR0Point act h [] b
  showLookahead = const ""

data LR1Point = LR1Point {
    lr1Lr0Point :: LR0Point
  , lr1PointLookahead :: S.Set Symbol
  } deriving (Show, Eq, Ord)

instance LRPoint LR1Point where
  type Lookahead LR1Point = Symbol
  lr0 = lr1Lr0Point
  modLr0 p v = p{lr1Lr0Point = v}
  pointLookahead = lr1PointLookahead
  modLookahead p v = p{lr1PointLookahead=v}
  startPoint rule = LR1Point (startPoint rule) (S.singleton TermEof)
  makeFirstPoint r p@LR1Point{lr1PointLookahead=la} h b beta act assoc
    = LR1Point (makeFirstPoint r (lr0 p) h b beta act assoc) $
      if Nothing `S.member` firstBeta
      then S.union la (S.map fromJust (S.delete Nothing firstBeta))
      else S.map fromJust firstBeta
    where firstBeta = first r beta
  lookaheadMatches p x = S.member x (lr1PointLookahead p)
  showLookahead = T.intercalate "/" . map showSymbol . S.toList . pointLookahead

nextSym :: LRPoint a => a -> Maybe Symbol
nextSym p | (x:_) <- pointRight p = Just x
          | otherwise = Nothing

nextPoint :: LRPoint a => Symbol -> a -> Maybe a
nextPoint s p = modLr0 p <$> nextPoint' (lr0 p)
  where nextPoint' p'@LR0Point{lr0PointRight=(x:xs), lr0PointLeft=ys}
          | x == s = Just $ p'{lr0PointRight=xs, lr0PointLeft=x:ys}
        nextPoint' _ = Nothing

pointHead :: LRPoint a => a -> Text
pointHead = lr0PointHead . lr0

pointLeft :: LRPoint a => a -> [Symbol]
pointLeft = reverse . lr0PointLeft . lr0

pointRight :: LRPoint a => a -> [Symbol]
pointRight = lr0PointRight . lr0

pointAction :: LRPoint a => a -> Maybe (((Text, [Symbol]), Maybe Text), Maybe Assoc)
pointAction p
  | null (pointRight p) = Just (((pointHead p, pointLeft p), lr0PointAction (lr0 p)), pointAssoc p)
  | otherwise = Nothing
