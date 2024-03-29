{-# LANGUAGE TupleSections, FlexibleContexts, OverloadedStrings, RecordWildCards #-}
module Lexer.Build (
    module MonadTypes
  , makeLexer
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import qualified Data.IntMap as IM
import Data.List.NonEmpty (NonEmpty(..))
import Data.List
import Data.Maybe
import Control.Arrow
import Regex.Parse
import Regex.Lex (alexMonadScan, runAlex, Token(..))
import Grammar (Symbol(..))
import Lexer.FA
import MonadTypes
import Data.Text (Text)
import qualified Data.Text as T
import Utils
import Lexer.Types
import Data.Proxy

makeLexer :: (LexerWriter lang, Monad m) => Proxy lang
          -> Bool -> [Text] -> MyMonadT m [(FilePath,Text)]
makeLexer lang outputDebug input = do
  defs <- liftEither . left T.lines $ mapM (fmap regex . scanLine) input
  let nfa = evalState (buildNFA defs) 0
      dfa = simplifyDFA . nfaToDFA $ nfa
      debug = if outputDebug
        then (("nfa.gv", nfaToGraphviz nfa) :) . (("dfa.gv", dfaToGraphviz dfa) :)
        else id
      stList :: [(IM.Key, (StateAttr, [(NonEmpty CharPattern, Int)]))]
      stList = map (second (second (fmap (first snd) . M.toList))) $ IM.toList dfa

  accSt <- catMaybes <$> mapM (\(f, (s, _)) -> fmap (f,) <$> isSingle f s) stList
  let tokNames = nub $ mapMaybe (\(_, x) -> (, saType x) <$> saName x) accSt
      terminals = (TermEof, NoType) : map (first Term) tokNames
  put terminals
  return . debug $ writeLexer lang accSt tokNames stList
  where
    isSingle f xs
      | S.null xs = return Nothing
      | S.size xs == 1 = return $ Just (S.findMin xs)
      | otherwise = do
        tell ["Lexer: Multiple actions/tokens match the same state " <> tshow f <> ": " <> T.intercalate ", " (map showSD $ S.toList xs) <> ". Choosing the first option."]
        return (Just $ S.findMin xs)
    showSD StateData{saName=Just nm, saNum=num}
      = nm <> " (line " <> tshow num <> ")"
    showSD StateData{saName=Nothing, saNum=num}
      = "<unnamed> (line " <> tshow num <> ")"

newState :: State Int Int
newState = state $ \s -> (s+1, s+1)

nonAcc :: M.Map (Maybe (Prio, NonEmpty CharPattern)) [Int]
       -> (StateAttr, M.Map (Maybe (Prio, NonEmpty CharPattern)) [Int])
nonAcc = (,) S.empty

regex1ToNFASt :: Prio -> RegexPatternSingle -> State Int NFA
regex1ToNFASt pr (PGroup ch) = do
  startSt <- get
  endSt <- newState
  return $ IM.singleton startSt (nonAcc $ M.singleton (Just (pr, ch)) [endSt])
regex1ToNFASt pr (PMaybe pat) = do
  s2 <- get
  nfa <- regexToNFASt pr pat
  s3 <- get
  return $ IM.insertWith mapUnion s2 (nonAcc $ M.singleton Nothing [s3]) nfa
regex1ToNFASt pr (PKleene pat) = regex1ToNFASt pr (PMaybe [PPositive pat])
regex1ToNFASt pr (PPositive pat) = do
  s2 <- get
  nfa <- regexToNFASt pr pat
  s3 <- get
  return $ IM.insertWith mapUnion s3 (nonAcc $ M.singleton Nothing [s2]) nfa
regex1ToNFASt pr (PAlternative pats) = altNFA True $ map (regexToNFASt pr) pats

altNFA :: Bool -> [State Int NFA] -> State Int NFA
altNFA mkEndNode mnfas = do
  startSt <- get
  nfas <- forM mnfas $ \mnfa -> do
    start <- newState
    nfa <- mnfa
    end <- get
    return ((start, end), nfa)
  endSt <- if mkEndNode then Just <$> newState else return Nothing
  let starts = map (fst . fst) nfas
      ends = map (snd . fst) nfas
      nfas' = map snd nfas
      endsmap = case endSt of
        Just x -> (IM.fromList (map (, nonAcc $ M.singleton Nothing [x]) ends) :)
        Nothing -> id
  return $ IM.insertWith mapUnion startSt (nonAcc $ M.singleton Nothing starts)
         $ IM.unionsWith mapUnion (endsmap nfas')

mapUnion :: Ord k =>
            (StateAttr, M.Map k [a2])
            -> (StateAttr, M.Map k [a2])
            -> (StateAttr, M.Map k [a2])
mapUnion (a, x) (b, y) = (a <> b, M.unionWith (++) x y)

regexToNFASt :: Prio -> RegexPattern -> State Int NFA
regexToNFASt pr = foldr
  (\p -> (IM.unionWith mapUnion <$> regex1ToNFASt pr p <*>))
  (return IM.empty)

regexToNFA :: (Int, Maybe Text, Action, Type, Greediness) -> RegexPattern -> State Int NFA
regexToNFA (num, name, action, typ, greed) pat = do
  res1 <- regexToNFASt num pat
  lastSt <- get
  return $ IM.insertWith mapUnion lastSt (S.singleton (StateData num name action typ greed), M.empty) res1

build1NFA :: Int -> RegexDef -> State Int NFA
build1NFA num (RegexDef mbname greed pat mbact mbtyp)
  = regexToNFA (num, mbname, mbact, mbtyp, greed) pat
-- build1DFA = fmap (simplifyDFA . nfaToDFA . regexToNFA . regex) . scan

buildNFA :: [RegexDef] -> State Int NFA
buildNFA [x] = build1NFA 1 x
buildNFA xs = altNFA False $ zipWith build1NFA [1..] xs

scanLine :: Text -> Either Text [Token]
scanLine s = left T.pack $ runAlex (T.unpack s) go
  where
  go = do
    tok <- alexMonadScan
    (tok :) <$> if tok /= TEOF
                then go
                else return []
