{-# LANGUAGE TupleSections #-}
module FA (
    StateAttr
  , NFA
  , DFA
  , nfaToDFA
  , simplifyDFA
  , nfaToGraphviz
  , dfaToGraphviz
  ) where

import RegexParse (Action, CharPattern(..))
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

nfaToGraphviz :: NFA -> String
nfaToGraphviz fa = "digraph{rankdir=LR;" <> concatMap node l <> "}"
  where l = IM.toList fa
        node (i, (a, t)) = show i <> "[label=\""<> intercalate "/" lbl <>"\"" <> acc <> "];"
                                  <> concatMap (trans i) (M.toList t)
                           where lbl = mapMaybe fst a
                                 acc = if not $ null lbl then ", peripheries=2" else ""
        trans i (c, ss) = concatMap (\s -> show i <> " -> " <> show s <> "[label=\""<> showCharPattern c<>"\"];") ss

dfaToGraphviz :: DFA -> String
dfaToGraphviz fa = "digraph{rankdir=LR;" <> concatMap node l <> "}"
  where l = IM.toList fa
        node (i, (a, t)) = show i <> "[label=\""<> intercalate "/" lbl <>"\"" <> acc <> "];"
                                  <> concatMap (trans i) (M.toList t)
                           where lbl = mapMaybe fst a
                                 acc = if not $ null lbl then ", peripheries=2" else ""
        trans i (c, ss) = (\s -> show i <> " -> " <> show s <> "[label=\""<> showCharPattern (Just c)<>"\"];") ss

showCharPattern :: Maybe (NE.NonEmpty CharPattern) -> String
showCharPattern Nothing = "ε"
showCharPattern (Just (x NE.:| rest)) = concatMap show1 $ x : rest
  where show1 (CChar c) = ['\'', c, '\'']
        show1 (CRange a b) = ['[',a,'-',b,']']
        show1 CAny = "'.'"

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
        return $ IM.unionWith u0 nm (IM.singleton t (tacc, M.unions cm))
    _ -> return IM.empty
  where
  u0 (a1, m1) (a2, m2) = (a1 <> a2, M.unionWith u1 m1 m2)
  u1 a b = if a == b then a else error "Multiple-state transition in DFA"
  moves = M.toList . M.unionsWith (<>) . map moves1 . IS.toList
  moves1 st
    | Just (_, trans) <- IM.lookup st nfa
    = M.mapKeysMonotonic fromJust . M.delete Nothing $ trans
    | otherwise = M.empty

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
