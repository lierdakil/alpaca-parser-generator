{-# LANGUAGE TypeFamilies
           , TypeApplications
           , GeneralizedNewtypeDeriving
           , UndecidableInstances
           , FlexibleContexts
           , OverloadedStrings
           #-}
module ParseLALR (makeLALRParser) where

import Grammar
import ParseLR
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NE
import MonadTypes
import Data.Text (Text)
import qualified Data.Text as T
import Utils

makeLALRParser :: (Monad m)
             => Text -> FilePath -> Text -> [Symbol]
             -> MyMonadT m [(FilePath,Text)]
makeLALRParser input base name tokens
  = do
    rules <- parse input
    let Rule start _ :| _ = rules
        r = mkRulesMap (Rule ExtendedStartRule (([NonTerm start], Nothing) :| []) <| rules)
    automaton <- lalrify $ buildLRAutomaton @LR1Point start r
    writeLRParser base name tokens r automaton

lalrify :: Monad m => LRAutomaton LR1Point -> MyMonadT m (LRAutomaton LALRPoint)
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
    mkLalr :: NonEmpty (LRState LR1Point) -> (LRState LR0Point, LRState LALRPoint)
    mkLalr (x :| xs) = (S.map lr0 x, combined (x:|xs))
      where combined :: NonEmpty (S.Set LR1Point) -> S.Set LALRPoint
            combined = S.fromList . map combine . transpose' . NE.map S.toList
            combine (p1:|ps) = LALRPoint $ p1{
              lr1PointLookahead = S.unions (map lr1PointLookahead (p1:ps))
            }
            transpose' :: NonEmpty [a] -> [NonEmpty a]
            transpose' = map NE.fromList . transpose . NE.toList
    tl = M.toList (snd t)
    lr0s = S.map lr0

newtype LALRPoint = LALRPoint {
    lr1Point :: LR1Point
  } deriving (Show,Eq,Ord,LRPoint)
