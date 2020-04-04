{-# LANGUAGE TypeFamilies
           , FlexibleContexts
           , OverloadedStrings
           #-}
module Parser.LR.LALR(LALRPoint) where

import Grammar
import Parser.LR.Point
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import MonadTypes
import Utils

lalrify :: Monad m => LRAutomaton LALRPoint -> MyMonadT m (LRAutomaton LALRPoint)
lalrify t = (,) (lookup' (fst t)) . M.fromAscList
    <$> mapM ensureUnique (NE.groupAllWith fst (map conv tl))
  where
    ensureUnique (x :| []) = return x
    ensureUnique ((k,x) :| xs)
      = if all ((==x) . snd) xs
        then return (k, x)
        else do
          tell $ "While building LALR automaton, state "<>tshow k<>" became \
                 \ non-deterministic:"
                 : map tshow (x:map snd xs)
          tell [ "Will use " <> tshow x ]
          return (k, x)
    conv ((stk, x), stv) = ((lookup' stk, x), lookup' stv)
    lookup' x = fromJust $ M.lookup (lr0s x) gstates
    states = fst t : S.toList (S.fromList $ map snd tl)
    gstates = M.fromList -- each key is unique due to grouping
      . map mkLalr
      . NE.groupAllWith (S.map lr0)
      $ states
    mkLalr :: NonEmpty (LRState LALRPoint) -> (LRState LR0Point, LRState LALRPoint)
    mkLalr (x :| xs) = (S.map lr0 x, combined (x:|xs))
      where combined :: NonEmpty (S.Set LALRPoint) -> S.Set LALRPoint
            combined = S.fromList . map (LALRPoint . combine . fmap lr1Point) . transpose' . NE.map S.toList
            combine (p1:|ps) = p1{
              lr1PointLookahead = S.unions (map lr1PointLookahead (p1:ps))
            }
            transpose' :: NonEmpty [a] -> [NonEmpty a]
            transpose' = map NE.fromList . transpose . NE.toList
    tl = M.toList (snd t)
    lr0s = S.map lr0

newtype LALRPoint = LALRPoint {
    lr1Point :: LR1Point
  } deriving (Show,Eq,Ord)

instance LRPoint LALRPoint where
    type Lookahead LALRPoint = Symbol
    lr0 = lr0 . lr1Point
    modLr0 p v = LALRPoint $ (modLr0 . lr1Point) p v
    pointLookahead = pointLookahead . lr1Point
    modLookahead p v = LALRPoint $ (modLookahead . lr1Point) p v
    startPoint rule = LALRPoint $ startPoint rule
    makeFirstPoint r p h b beta act
      = LALRPoint $ makeFirstPoint r (lr1Point p) h b beta act
    lookaheadMatches = lookaheadMatches . lr1Point
    showLookahead = showLookahead . lr1Point
    postprocess = lalrify
