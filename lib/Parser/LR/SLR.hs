{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
module Parser.LR.SLR where

import Grammar
import Parser.LR.Point
import qualified Data.Set as S
import Data.Function
import qualified Data.Text as T

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
  showLookahead p = T.intercalate "/" $ map showSymbol (S.toList $ slrLookahead p)
  makeFirstPoint r SLRPoint{lr0Point=p} h b beta act assoc
    = SLRPoint (makeFirstPoint r p h b beta act assoc) $ follow r (NonTerm h)
  lookaheadMatches p x = S.member x (slrLookahead p)
