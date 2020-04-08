{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Lexer.FA (
    StateAttr
  , NFA
  , DFA
  , nfaToDFA
  , simplifyDFA
  , nfaToGraphviz
  , dfaToGraphviz
  ) where

import Regex.Parse (Action, CharPattern(..))
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.List
import Data.Maybe
import Control.Monad.State
import Control.Arrow
import Data.Text (Text)
import qualified Data.Text as T
import Utils

type StateAttr = S.Set (Maybe Text, Action)
type NFA = IM.IntMap (StateAttr, M.Map (Maybe (NonEmpty CharPattern)) [Int])
type DFA = IM.IntMap (StateAttr, M.Map (NonEmpty CharPattern) Int)

nfaToGraphviz :: NFA -> Text
nfaToGraphviz fa = "digraph{rankdir=LR;" <> foldMap node l <> "}"
  where l = IM.toList fa
        node (i, (a, t)) = tshow i <> "[label=\""<> T.intercalate "/" lbl <> "\"" <> acc <> "];"
                                  <> foldMap (trans i) (M.toList t)
                           where lbl = mapMaybe fst $ S.toList a
                                 acc = if not $ null lbl then ", peripheries=2" else ""
        trans i (c, ss) = foldMap (\s -> tshow i <> " -> " <> tshow s <> "[label=\""<> showCharPattern c<>"\"];") ss

dfaToGraphviz :: DFA -> Text
dfaToGraphviz fa = "digraph{rankdir=LR;" <> foldMap node l <> "}"
  where l = IM.toList fa
        node (i, (a, t)) = tshow i <> "[label=\""<> T.intercalate "/" lbl <>"\"" <> acc <> "];"
                                  <> foldMap (trans i) (M.toList t)
                           where lbl = mapMaybe fst $ S.toList a
                                 acc = if not $ null lbl then ", peripheries=2" else ""
        trans i (c, ss) = (\s -> tshow i <> " -> " <> tshow s <> "[label=\""<> showCharPattern (Just c)<>"\"];") ss

showCharPattern :: Maybe (NonEmpty CharPattern) -> Text
showCharPattern Nothing = "ε"
showCharPattern (Just (x :| rest)) = foldMap show1 $ x : rest
  where show1 (CChar c) = T.pack ['\'', c, '\'']
        show1 (CRange a b) = T.pack ['[',a,'-',b,']']
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
ecls nfa = ecls' (S.empty, IS.empty)
  where
  ecls' accum@(accAttr, accStates) (x:xs)
    | x `IS.member` accStates = ecls' accum xs
    | Just (attr, trans) <- IM.lookup x nfa
    = let newst = concat . maybeToList $ M.lookup Nothing trans
      in  ecls' (attr <> accAttr, IS.insert x accStates) (xs <> newst)
    | otherwise
    = ecls' (accAttr, IS.insert x accStates) xs
  ecls' accum [] = accum

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
    . foldMap (\(x:|xs) -> map ((, fst x) . fst) xs)
    . NE.groupAllWith snd
    $ lst

nfaToDFA :: NFA -> DFA
nfaToDFA nfa = evalState (nfaToDFASt nfa)
 (0, IS.empty, [(0,) $ ecls nfa [0]])
