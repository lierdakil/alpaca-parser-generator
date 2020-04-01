{-# LANGUAGE TupleSections #-}
module FA (
    StateAttr
  , NFA
  , DFA
  , nfaToDFA
  , simplifyDFA
  ) where

import RegexParse (Action, CharPattern)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.List.NonEmpty as NE
import Data.List
import Data.Maybe
import Control.Monad.State
import Control.Applicative
import Control.Arrow
import Data.Function

type StateAttr = [(Maybe String, Action)]
type NFA = IM.IntMap (StateAttr, M.Map (Maybe (NE.NonEmpty CharPattern)) [Int])
type DFA = IM.IntMap (StateAttr, M.Map (NE.NonEmpty CharPattern) Int)

nfaToDFASt :: NFA -> State (Int, IS.IntSet, [(Int, (StateAttr, IS.IntSet))]) DFA
nfaToDFASt nfa = do
  (st, seen, unseen) <- get
  case find ((`IS.notMember` seen) . fst) unseen of
    Just (t, (tacc, tst)) -> do
        let newseen = IS.insert t seen
        put (st, newseen, unseen)
        cm <- forM (moves tst) $ \(ch, newsts) -> do
          let (uacc, u) = ecls nfa newsts
          case find ((==u) . snd . snd) unseen of
            Just (st3, _) ->
              return $ M.singleton ch st3
            Nothing -> do
              st' <- gets (\(x, _, _) -> x + 1)
              modify (\(_, x, y) -> (st', x, (st', (uacc, u)):y))
              return $ M.singleton ch st'
        nm <- nfaToDFASt nfa
        return $ IM.union nm (IM.singleton t (tacc, M.unions cm))
    _ -> return IM.empty
  where
  moves = concatMap moves1 . IS.toList
  moves1 st
    | Just (_, trans) <- IM.lookup st nfa
    = mapMaybe (\(ch, sts) -> (,sts) <$> ch) $ M.toList trans
    | otherwise = []

ecls :: Ord a =>
          IM.IntMap (StateAttr, M.Map (Maybe a) [IS.Key])
          -> [IS.Key] -> (StateAttr, IS.IntSet)
ecls nfa st = (accs, IS.fromList stl)
  where
  accs = fst $ ecls' st
  stl = snd $ ecls' st
  -- ecls' :: [Int] -> (Bool, [Int])
  ecls' arg = let res = map ecls1 arg
                  acc = concatMap fst res
                  sts = concatMap snd res
              in (acc, sts)
  -- ecls1 :: Int -> (Bool, [Int])
  ecls1 st2
    | Just (acc, trans) <- IM.lookup st2 nfa
    = let newst = concat . maybeToList $ M.lookup Nothing trans
          (restacc, rest) = ecls' newst
      in (acc <|> restacc, st2:newst <> rest)
    | otherwise = ([], [])

simplifyDFA :: DFA -> DFA
simplifyDFA dfa = IM.fromList $ mapMaybe simpDFA lst
  where
  lst = IM.toList dfa
  simpDFA (k, v)
    | k `IM.member` equalStates
    = Nothing
    | otherwise
    = Just (k, second (M.map replace) v)
  replace st
    | Just nst <- IM.lookup st equalStates
    = nst
    | otherwise = st
  equalStates =
      IM.fromList
    . concatMap (\(x:xs) -> map ((, fst x) . fst) xs)
    . groupBy ((==) `on` snd)
    . sortOn snd
    $ lst

nfaToDFA :: NFA -> DFA
nfaToDFA nfa = evalState (nfaToDFASt nfa)
 (0, IS.empty, [(0,) $ ecls nfa [0]])
