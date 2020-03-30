{-# LANGUAGE FlexibleContexts
           , TypeFamilies
           , TypeApplications
           , ScopedTypeVariables
           #-}
module ParseLR (Proxy(..), LR1Point, LR0Point, makeLRParser, buildLRAutomaton) where

import Grammar
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Data.List
import Data.Void
import Data.Proxy
import Control.Monad.State
import Text.Layout.Table
import qualified Control.Arrow as A

class Ord a => LRPoint a where
  type family Lookahead a
  setLR :: a -> [Symbol] -> [Symbol] -> a
  pointLeft :: a -> [Symbol]
  pointRight :: a -> [Symbol]
  startPoint :: String -> a
  showLookahead :: a -> String
  makeFirstPoint :: RulesMap -> a -> [Symbol] -> [Symbol] -> a

instance LRPoint LR1Point where
  type Lookahead LR1Point = Symbol
  pointLeft = reverse . lr1PointLeft
  pointRight = lr1PointRight
  setLR p l r = p{lr1PointLeft=l, lr1PointRight=r}
  startPoint rule = LR1Point [] [NonTerm rule] (S.singleton TermEof)
  showLookahead LR1Point{lr1PointLookahead=la} = intercalate "/" (map showSymbol $ S.toList la)
  makeFirstPoint r LR1Point{lr1PointLookahead=la} b beta = LR1Point [] b $
      if Nothing `S.member` firstBeta
      then S.union la (S.map fromJust (S.delete Nothing firstBeta))
      else S.map fromJust firstBeta
    where firstBeta = first r beta

data LR1Point = LR1Point {
    lr1PointLeft :: [Symbol]
  , lr1PointRight :: [Symbol]
  , lr1PointLookahead :: S.Set Symbol
  } deriving (Show, Eq, Ord)

data LR0Point = LR0Point {
    lr0PointLeft :: [Symbol]
  , lr0PointRight :: [Symbol]
  } deriving (Show, Eq, Ord)

instance LRPoint LR0Point where
  type Lookahead LR0Point = Void
  pointLeft = reverse . lr0PointLeft
  pointRight = lr0PointRight
  setLR p l r = p{lr0PointLeft=l, lr0PointRight=r}
  startPoint rule = LR0Point [] [NonTerm rule]
  showLookahead _ = ""
  makeFirstPoint _ _ b _ = LR0Point [] b

nextPoint :: LRPoint a => Symbol -> a -> Maybe a
nextPoint s p
  | x:beta <- pointRight p
  , s == x
  = Just $ setLR p  (x:pointLeft p) beta
  | otherwise = Nothing
nextSym :: LRPoint a => a -> Maybe Symbol
nextSym p | (x:_) <- pointRight p = Just x
          | otherwise = Nothing

type LRState p = S.Set p
type LRAutomaton p = (LRState p, M.Map (LRState p, Symbol) (LRState p))

makeLRParser :: forall p. LRPoint p => Proxy p -> [Symbol] -> String -> String
makeLRParser _ _ = writeLRAutomaton . buildLRAutomaton @p . parse

writeLRAutomaton :: LRPoint p => LRAutomaton p -> String
writeLRAutomaton aut = "\
\/*\n\
\" <> printTable aut <> "\
\\n*/\n\
\"

buildLRAutomaton :: forall p. LRPoint p => Rules -> LRAutomaton p
buildLRAutomaton rules = (,) startState . fst . flip execState (M.empty, S.empty) $ buildGotoTable startState
  where
  r = mkRulesMap rules
  oldStartRule = let (Rule h _) = head rules in h
  startState = S.singleton $ startPoint oldStartRule
  -- buildGotoTable :: LR1State -> m ()
  buildGotoTable i = do
    seen <- gets (S.member i . snd)
    unless seen $ do
      modify (A.second (S.insert i))
      let cls = S.toList (pointClosure r i)
          nextSyms = S.toList . S.fromList $ mapMaybe nextSym cls
      forM_ nextSyms $ \s -> do
        let iNext = S.fromList $ mapMaybe (nextPoint s) cls
        modify (A.first (M.insert (i, s) iNext))
        buildGotoTable iNext

printTable :: LRPoint p => LRAutomaton p -> String
printTable t = --show t
  tableString (repeat def) unicodeS (titlesH ["stateNo", "stateDef"]) stateRows
  <> "\n" <> tableString (repeat def) unicodeS (titlesH titles) (map rowG rows)
  where
  stateMapList = zip states [0::Word ..]
  stateMap = M.fromList stateMapList
  stateRows = map (\(s, n) -> colsAllG top [[show n], showState s]) stateMapList
  showState = map showPoint . S.toList
  showPoint p
    = unwords (map showSymbol (pointLeft p) <> ["."] <> map showSymbol (pointRight p)) <> ","
      <> showLookahead p
  states = fst t : (S.toList . S.fromList $ map snd tl)
  symbols = S.toList . S.fromList $ map (snd . fst) tl
  titles = "" : map showSymbol symbols
  tl = M.toList (snd t)
  rows = map makeRow stateMapList
  makeRow (st, n) = show n : map (makeCell st) symbols
  makeCell st sym
    | Just cell <- M.lookup (st, sym) (snd t) = show $ fromJust (M.lookup cell stateMap)
    | otherwise = ""

pointClosure :: LRPoint a => RulesMap -> S.Set a -> S.Set a
pointClosure r = flip evalState S.empty . fmap S.unions . mapM doAdd . S.toList
  where
  doAdd p
    | (NonTerm nt:beta) <- pointRight p = do
    seen <- gets (S.member nt)
    if not seen
    then do
      modify (S.insert nt)
      let Just alts = M.lookup nt r
          new = map (\(b, _) -> makeFirstPoint r p b beta) alts
      S.union (S.fromList (p : new)) . S.unions <$> mapM doAdd new
    else return (S.singleton p)
  doAdd x = return (S.singleton x)
