{-# LANGUAGE TupleSections, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Werror #-}
module Lexer.FA (
    StateAttr
  , NFA
  , DFA
  , nfaToDFA
  , simplifyDFA
  , nfaToGraphviz
  , dfaToGraphviz
  , StateData(..)
  , containsCR
  ) where

import Regex.Parse (Action, Type, CharPattern(..), Greediness(..))
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.List
import Data.Maybe
import Data.Tuple (swap)
import Control.Monad.State
import Control.Arrow
import Data.Text (Text)
import qualified Data.Text as T
import Utils

data StateData = StateData { saNum :: Int, saName :: Maybe Text, saAct :: Action, saType :: Type, saGreed :: Greediness } deriving (Show, Eq, Ord)
type StateAttr = S.Set StateData
type NFA = IM.IntMap (StateAttr, M.Map (Maybe (NonEmpty CharPattern)) [Int])
type DFA = IM.IntMap (StateAttr, M.Map (NonEmpty CharPattern) Int)

nfaToGraphviz :: NFA -> Text
nfaToGraphviz fa = "digraph{rankdir=LR;" <> foldMap node l <> "}"
  where l = IM.toList fa
        node (i, (a, t)) = tshow i <> "[label=\""<> T.intercalate "/" lbl <> "\"" <> acc <> "];"
                                  <> foldMap (trans i) (M.toList t)
                           where lbl = mapMaybe saName $ S.toList a
                                 acc = if not $ null lbl then ", peripheries=2" else ""
        trans i (c, ss) = foldMap (\s -> tshow i <> " -> " <> tshow s <> "[label=\""<> showCharPattern c<>"\"];") ss

dfaToGraphviz :: DFA -> Text
dfaToGraphviz fa = "digraph{rankdir=LR;" <> foldMap node l <> "}"
  where l = IM.toList fa
        node (i, (a, t)) = tshow i <> "[label=\""<> T.intercalate "/" lbl <>"\"" <> acc <> "];"
                                  <> foldMap (trans i) (M.toList t)
                           where lbl = mapMaybe saName $ S.toList a
                                 acc = if not $ null lbl then ", peripheries=2" else ""
        trans i (c, ss) = (\s -> tshow i <> " -> " <> tshow s <> "[label=\""<> showCharPattern (Just c)<>"\"];") ss

showCharPattern :: Maybe (NonEmpty CharPattern) -> Text
showCharPattern Nothing = "ε"
showCharPattern (Just (x :| rest)) = foldMap show1 $ x : rest
  where show1 (CChar '"') = T.pack "\'\\\"\'"
        show1 (CChar c) = tshow' c
        show1 (CRange a b) = "[" <> tshow' a <> "-" <> tshow' b <> "]"
        show1 CAny = "."
        show1 (CNot xs) = "~(" <> showCharPattern (Just xs) <> ")"
        tshow' = T.replace "\\" "\\\\" . tshow

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
  moves = M.toList . enrich . M.unionsWith (<>) . map moves1 . IS.toList
  moves1 st
    | Just (_, trans) <- IM.lookup st nfa
    = M.map IS.fromList . M.mapKeysMonotonic fromJust . M.delete Nothing $ trans
    | otherwise = M.empty
  -- this computes transitive closure of xs over `contains` defined below
  -- using dfs; also removes duplication
  enrich xs =
    let ks = S.toList . S.fromList $ concatMap NE.toList $ M.keys xs
        xs' = M.fromListWith (<>) [(k, v) | (k1, v) <- M.toList xs, k <- NE.toList k1]
        es = M.fromListWith (<>)
              [ (k1, [k2]) | k1 <- ks, k2 <- ks, k1 /= k2, containsCR k2 k1 ]
        getRow k = (S.singleton k, IS.unions $ map (xs' M.!) $ dfs es S.empty [k])
        mergeKeys = swapMap . swapMap
        swapMap :: (Ord k, Ord v, Semigroup k) => M.Map k v -> M.Map v k
        swapMap = M.fromListWith (<>) . map swap . M.toList
        setToNE s = case S.toList s of
          (x:xss) -> x NE.:| xss
          _ -> error "empty option"
    in M.mapKeysMonotonic setToNE $ mergeKeys $ M.fromListWith (<>) $ map getRow ks
  dfs _ vis [] = S.toList vis
  dfs g vis (x:xs)
    | x `S.member` vis = dfs g vis xs
    | otherwise = dfs g (S.insert x vis) $ case g M.!? x of
        Just xs' -> xs' ++ xs
        Nothing  -> xs
-- this function is asymmetric because we want to
-- prefer more concrete definitions to less concrete,
-- i.e. prefer char to range, etc.
containsCR :: CharPattern -> CharPattern -> Bool
containsCR CAny _ = True  -- any contains anything
containsCR _ CAny = False -- nothing besides any contains any
containsCR (CNot a) (CNot b) = all (not . containsCR (CNot b)) a
containsCR (CNot a) (CChar x) = and $ do
  a' <- a
  pure $ not $ containsCR a' (CChar x)
containsCR (CNot a) (CRange x y) = and $ do
  a' <- a
  pure $ not $ case a' of -- a' intersects range
    CAny -> True
    CChar w -> x <= w && w <= y
    CRange v w -> w >= x || y >= v
    CNot _ -> error "Negative range in a negative range; this is a bug."
containsCR _a (CNot _b) = False
containsCR (CChar c) (CRange a b) = c == a && c == b -- if range is one-character
containsCR (CChar c1) (CChar c2) = c1 == c2
containsCR (CRange a b) (CChar c1) = c1 >= a && c1 <= b
containsCR (CRange a b) (CRange c d) = a <= c && b >= d

ecls :: Ord a =>
          IM.IntMap (StateAttr, M.Map (Maybe a) [IS.Key])
          -> IS.IntSet -> (StateAttr, IS.IntSet)
ecls nfa = ecls' (S.empty, IS.empty) . IS.toList
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
 (0, IS.empty, [(0,) $ ecls nfa $ IS.singleton 0])
