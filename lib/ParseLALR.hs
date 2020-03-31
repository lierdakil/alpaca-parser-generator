{-# LANGUAGE TypeFamilies, TypeApplications, RecordWildCards, TupleSections #-}
module ParseLALR (makeLALRParser) where

import Grammar
import ParseLR
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Function
import Data.List
import Data.Maybe

makeLALRParser :: String -> [Symbol] -> String -> String
makeLALRParser name tokens input = writeLRParser name tokens r . lalrify $ buildLRAutomaton @LR1Point start r
  where rules = parse input
        start = let Rule h _ : _ = rules in h
        r = mkRulesMap (Rule "%S" [([NonTerm start], Nothing)] : rules)

lalrify :: LRAutomaton LR1Point -> LRAutomaton LALRPoint
lalrify t = (lookup' (fst t), M.fromListWith checkSame . map conv $ tl)
  where
    conv ((stk, x), stv) = ((lookup' stk, x), lookup' stv)
    lookup' x = fromJust $ M.lookup (lr0s x) gstates
    states = fst t : S.toList (S.fromList $ map snd tl)
    gstates = M.fromListWith (error . show)
      . map mkLalr
      . groupBy ((==) `on` S.map lr0)
      . sortBy (compare `on` S.map lr0)
      $ states
    mkLalr :: [LRState LR1Point] -> (LRState LR0Point, LRState LALRPoint)
    mkLalr xs = (S.map lr0 (head xs), combined $ map S.toList xs)
      where combined :: [[LR1Point]] -> S.Set LALRPoint
            combined = S.fromList . map combine . transpose
            combine ps@(p1:_) = LALRPoint{
                lalr0Point = lr0 p1
              , lalrLookahead = S.unions (map lr1PointLookahead ps)
            }
            combine [] = error "x_X"
    tl = M.toList (snd t)
    lr0s = S.map lr0

checkSame :: LRState LALRPoint -> LRState LALRPoint -> LRState LALRPoint
checkSame a b = if a==b
  then S.fromList $ zipWith combine (S.toList a) (S.toList b)
  else error "not same"
  where
    combine p1 p2 = p2{lalrLookahead = lalrLookahead p1 `S.union` lalrLookahead p2}

data LALRPoint = LALRPoint {
    lalr0Point :: LR0Point
  , lalrLookahead :: S.Set Symbol
  } deriving (Show,Eq,Ord)

instance LRPoint LALRPoint where
  type Lookahead LALRPoint = Symbol
  lr0 = lalr0Point
  modLr0 p v = p{lalr0Point = v}
  pointLookahead = lalrLookahead
  modLookahead p v = p{lalrLookahead=v}
  startPoint rule = LALRPoint (startPoint rule) $ S.singleton TermEof
  showLookahead p = intercalate "/" $ map showSymbol (S.toList $ lalrLookahead p)
  makeFirstPoint r LALRPoint{lalr0Point=p, lalrLookahead=la} h b beta act
    = LALRPoint (makeFirstPoint r p h b beta act) $
      if Nothing `S.member` firstBeta
      then S.union la (S.map fromJust (S.delete Nothing firstBeta))
      else S.map fromJust firstBeta
    where firstBeta = first r beta
  lookaheadMatches p x = S.member x (lalrLookahead p)
