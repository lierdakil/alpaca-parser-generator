{-# LANGUAGE TypeFamilies #-}
module ParseSLR where

import Grammar
import ParseLR
import qualified Data.Set as S
import Data.Function
import Data.List

data SLRPoint = SLRPoint {
    lr0Point :: LR0Point
  , slrLookahead :: S.Set Symbol
  } deriving (Show)

instance Eq SLRPoint where
  (==) = (==) `on` lr0Point

instance Ord SLRPoint where
  compare = compare `on` lr0Point

instance LRPoint SLRPoint where
  type Lookahead SLRPoint = Symbol
  lr0 = lr0Point
  modLr0 p v = p{lr0Point = v}
  pointLookahead = slrLookahead
  modLookahead p v = p{slrLookahead=v}
  startPoint rule = SLRPoint (startPoint rule) $ S.singleton TermEof
  showLookahead p = intercalate "/" $ map showSymbol (S.toList $ slrLookahead p)
  makeFirstPoint r SLRPoint{lr0Point=p} h b beta act
    = SLRPoint (makeFirstPoint r p h b beta act) $ follow r (NonTerm h)
  lookaheadMatches p x = S.member x (slrLookahead p)
