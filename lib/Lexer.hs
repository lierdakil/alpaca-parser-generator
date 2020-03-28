{-# LANGUAGE TupleSections #-}
module Lexer (makeLexer, Lang(..)) where

import qualified Data.Map as M
import Control.Monad.State
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.List.NonEmpty as NE
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Function
import Control.Arrow
import RegexParse
import RegexLex (alexMonadScan, runAlex, Token(..))

type StateAttr = Maybe (Maybe String, Action)
type NFA = IM.IntMap (StateAttr, M.Map (Maybe (NE.NonEmpty CharPattern)) [Int])
type DFA = IM.IntMap (StateAttr, M.Map (NE.NonEmpty CharPattern) Int)

newState :: State Int Int
newState = state $ \s -> (s+1, s+1)

nonAcc :: M.Map (Maybe (NE.NonEmpty CharPattern)) [Int]
       -> (StateAttr, M.Map (Maybe (NE.NonEmpty CharPattern)) [Int])
nonAcc = (,) Nothing

regex1ToNFASt :: RegexPatternSingle -> State Int NFA
regex1ToNFASt (PGroup ch) = do
  startSt <- get
  endSt <- newState
  return $ IM.singleton startSt (nonAcc $ M.singleton (Just ch) [endSt])
regex1ToNFASt (PMaybe pat) = do
  startSt <- get
  patNfa <- regexToNFASt pat
  endSt <- get
  return $ IM.insertWith mapUnion startSt (nonAcc $ M.singleton Nothing [endSt]) patNfa
regex1ToNFASt (PKleene pat) = do
  startSt <- get
  loopStartSt <- newState
  patNfa <- regexToNFASt pat
  loopEndSt <- get
  endSt <- newState
  return $ IM.insertWith mapUnion startSt (nonAcc $ M.singleton Nothing [endSt, loopStartSt])
         $ IM.insertWith mapUnion loopEndSt (nonAcc $ M.singleton Nothing [loopStartSt, endSt])
           patNfa
regex1ToNFASt (PPositive pat) = do
  startSt <- get
  patNfa <- regexToNFASt pat
  endSt <- get
  return $ IM.insertWith mapUnion endSt (nonAcc $ M.singleton Nothing [startSt]) patNfa
regex1ToNFASt (PAlternative pats) = altNFA $ map regexToNFASt pats

altNFA :: [State Int NFA] -> State Int NFA
altNFA mnfas = do
  startSt <- get
  nfas <- forM mnfas $ \mnfa -> do
    start <- newState
    nfa <- mnfa
    end <- get
    return ((start, end), nfa)
  endSt <- newState
  let starts = map (fst . fst) nfas
      ends = map (snd . fst) nfas
      nfas' = map snd nfas
      endsmap = IM.fromList $ map (, nonAcc $ M.singleton Nothing [endSt]) ends
  return $ IM.insertWith mapUnion startSt (nonAcc $ M.singleton Nothing starts)
         $ IM.unionsWith mapUnion (endsmap:nfas')

mapUnion :: Ord k =>
            (StateAttr, M.Map k [a2])
            -> (StateAttr, M.Map k [a2])
            -> (StateAttr, M.Map k [a2])
mapUnion (a, x) (b, y) = (a <|> b, M.unionWith (++) x y)

regexToNFASt :: RegexPattern -> State Int NFA
regexToNFASt = foldr
  (\p -> (IM.unionWith mapUnion <$> regex1ToNFASt p <*>))
  (return IM.empty)

regexToNFA :: (Maybe String, Action) -> RegexPattern -> State Int NFA
regexToNFA (name, action) pat = do
  res1 <- regexToNFASt pat
  lastSt <- get
  return $ IM.insertWith mapUnion lastSt (Just (name, action), M.empty) res1

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
                  acc = foldl' (<|>) Nothing $ map fst res
                  sts = concatMap snd res
              in (acc, sts)
  -- ecls1 :: Int -> (Bool, [Int])
  ecls1 st2
    | Just (acc, trans) <- IM.lookup st2 nfa
    = let newst = concat . maybeToList $ M.lookup Nothing trans
          (restacc, rest) = ecls' newst
      in (acc <|> restacc, st2:newst <> rest)
    | otherwise = (Nothing, [])

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

build1NFA :: RegexDef -> State Int NFA
build1NFA (RegexDef mbname pat mbact) = regexToNFA (mbname, mbact) pat
-- build1DFA = fmap (simplifyDFA . nfaToDFA . regexToNFA . regex) . scan

buildNFA :: [RegexDef] -> State Int NFA
buildNFA [x] = build1NFA x
buildNFA xs = altNFA $ map build1NFA xs

scanLine :: String -> [Token]
scanLine s = either error id $ runAlex s go
  where
  go = do
    tok <- alexMonadScan
    (tok :) <$> if tok /= TEOF
                then go
                else return []

buildDFA :: [RegexDef] -> DFA
buildDFA defs = simplifyDFA . nfaToDFA $ evalState (buildNFA defs) 0

data Lang = CPP

makeLexer :: [String] -> Lang -> String
makeLexer = writeLexer . buildDFA . map (regex . scanLine)

writeLexer :: DFA -> Lang -> String
writeLexer dfa CPP = "\
\#ifndef LEXER_H\n\
\#define LEXER_H\n\
\#include <string>\n\
\#include <stdexcept>\n\
\#include <iostream>\n\
\enum class TokenType { eof, " <> intercalate "," (map ("Tok_"<>) tokNames) <> "};\
\std::string to_string(TokenType tt) { switch(tt) {" <> tokReflect <> " }}\
\\n\
\#if __has_include(\"token.h\")\n\
\#include \"token.h\"\n\
\#else\n\
\struct Token{TokenType type; std::string text;};\
\Token mkToken(TokenType type, const std::string& text = \"\") {\
  \return Token{type, text};\
\}\n\
\#endif\n\
\class Lexer {\
  \std::string input;\
  \std::size_t curChIx;\
  \bool debug;\
\public:\
  \Lexer(const std::string &input, bool debug=false) \
  \: input(input), curChIx(0), debug(debug) {}\
  \Token getNextToken() {\
    \std::string buf;\
    \std::string text;\
    \std::size_t lastAccChIx = curChIx;\
    \int curSt = 0;\
    \int accSt = -1;\
    \while (curSt != -1) {\
      \" <> checkAccState <> "\
      \if (curChIx >= input.size())\
        \break;\
      \char curCh = input[curChIx];\
      \curChIx += 1;\
      \curSt = transTable(curCh, curSt);\
      \buf += curCh;\
    \}\
    \curChIx = lastAccChIx;\
    \" <> returnResult <> "\
    \if (curChIx >= input.size()) { \
    \if (debug) std::cerr << \"Got EOF while lexing \\\"\" << text << \"\\\"\" << std::endl; \
    \return mkToken(TokenType::eof); }\
    \throw std::runtime_error(\"Unexpected input: \" + buf);\
  \}\
  \int transTable(char curCh, int curSt) { " <> transTable <> " return -1; } \
\};\n\
\#endif\n"
  where
  tokReflect = "case TokenType::eof: return \"%eof\";" <> concatMap tokReflect1 tokNames
  tokReflect1 tn = "case TokenType::Tok_" <> tn <> ": return \"" <> tn <> "\";"
  tokNames = nub $ mapMaybe (fst . snd) accSt
  returnResult = concat (foldr ((:) . returnResult1) [] accSt)
  returnResult1 (st, (Just name, act))
    = "if (accSt == "<> show st <>") { \
      \if (debug) std::cerr << \"Lexed token " <> name <> ": \\\"\" << text << \"\\\"\" << std::endl; \
      \return mkToken(TokenType::Tok_" <> name <> mkAct act <>"); }"
  returnResult1 (st, (Nothing, _))
    = "if (accSt == "<> show st <>") { \
      \if (debug) std::cerr << \"Skipping state " <> show st <> ": \\\"\" << text << \"\\\"\" << std::endl; \
      \return getNextToken(); }"
  mkAct NoAction = ""
  mkAct (Action act) = "," <> act
  checkAccState = "if( "
     <> intercalate " || " (foldr ((:) . checkAccState1 . fst) [] accSt)
     <> ") { lastAccChIx = curChIx; text = buf; accSt = curSt; }"
  checkAccState1 st = "curSt == " <> show st
  accSt = mapMaybe (\(f, (s, _)) -> (f,) <$> s) stList
  transTable = "switch(curSt) { " <> concat (foldr ((:) . checkState) [] stList) <> " } "
  checkState (_, (_, [])) = ""
  checkState (curSt, (_, charTrans)) = "case " <> show curSt <> ":"
    <> intercalate " else " (foldr ((:) . checkChars) [] charTrans)
    <> "break;"
  checkChars (charGroup, newSt) = "if(" <> charCond charGroup <> ") return " <> show newSt <> ";"
  charCond = intercalate "||" . map charCond1 . NE.toList
  charCond1 (CChar c) = "curCh == " <> show c
  charCond1 (CRange c1 c2) = "(curCh >= " <> show c1 <> " && curCh <= " <> show c2 <> ")"
  charCond1 CAny = "true"
  stList = map (second (second M.toList)) $ IM.toList dfa
