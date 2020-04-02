{-# LANGUAGE TypeFamilies
           , TypeApplications
           , GeneralizedNewtypeDeriving
           , UndecidableInstances
           #-}
module ParseLALR (makeLALRParser) where

import Grammar
import ParseLR
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Function
import Data.List
import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..))
import MonadTypes

makeLALRParser :: (Monad m)
             => String -> FilePath -> String -> [Symbol]
             -> MyMonadT m [(FilePath,String)]
makeLALRParser input base name tokens
  = writeLRParser base name tokens r . lalrify $ buildLRAutomaton @LR1Point start r
  where rules = parse input
        start = let Rule h _ : _ = rules in h
        r = mkRulesMap (Rule "%S" (([NonTerm start], Nothing) :| []) : rules)

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
            combine ps@(p1:_) = LALRPoint $ p1{
              lr1PointLookahead = S.unions (map lr1PointLookahead ps)
            }
            combine [] = error "x_X"
    tl = M.toList (snd t)
    lr0s = S.map lr0

checkSame :: LRState LALRPoint -> LRState LALRPoint -> LRState LALRPoint
checkSame a b = if a==b then a
                else error "not same"

newtype LALRPoint = LALRPoint {
    lr1Point :: LR1Point
  } deriving (Show,Eq,Ord,LRPoint)
