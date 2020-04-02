{-# LANGUAGE FlexibleContexts
           , TypeFamilies
           , TypeApplications
           , ScopedTypeVariables
           , TupleSections
           #-}
module ParseLR (Proxy(..)
  , LRPoint(..)
  , LR1Point(..)
  , LR0Point(..)
  , LRAutomaton
  , LRState
  , makeLRParser
  , writeLRParser
  , buildLRAutomaton) where

import Grammar
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.List
import Data.Void
import Data.Char
import Data.Function
import Data.Proxy
import MonadTypes
import Control.Monad.State
import Text.Layout.Table
import qualified Control.Arrow as A

class (Ord a, Ord (Lookahead a)) => LRPoint a where
  type family Lookahead a
  lr0 :: a -> LR0Point
  modLr0 :: a -> LR0Point -> a
  pointLookahead :: a -> S.Set (Lookahead a)
  modLookahead :: a -> S.Set (Lookahead a) -> a
  startPoint :: String -> a
  makeFirstPoint :: RulesMap -> a -> String -> [Symbol] -> [Symbol] -> Maybe String -> a
  lookaheadMatches :: a -> Symbol -> Bool
  showLookahead :: a -> String

nextPoint :: LRPoint a => Symbol -> a -> Maybe a
nextPoint s p = modLr0 p <$> nextPoint' (lr0 p)
  where nextPoint' p'@LR0Point{lr0PointRight=(x:xs), lr0PointLeft=ys}
          | x == s = Just $ p'{lr0PointRight=xs, lr0PointLeft=x:ys}
        nextPoint' _ = Nothing
pointHead :: LRPoint a => a -> String
pointHead = lr0PointHead . lr0
pointLeft :: LRPoint a => a -> [Symbol]
pointLeft = reverse . lr0PointLeft . lr0
pointRight :: LRPoint a => a -> [Symbol]
pointRight = lr0PointRight . lr0
pointAction :: LRPoint a => a -> Maybe ((String, [Symbol]), String)
pointAction p
  | null (pointRight p) = ((pointHead p, pointLeft p), ) <$> lr0PointAction (lr0 p)
  | otherwise = Nothing

data LR0Point = LR0Point {
    lr0PointAction :: Maybe String
  , lr0PointHead :: String
  , lr0PointLeft :: [Symbol]
  , lr0PointRight :: [Symbol]
  } deriving (Show, Eq, Ord)

instance LRPoint LR0Point where
  type Lookahead LR0Point = Void
  lr0 = id
  modLr0 _ x = x
  pointLookahead = const S.empty
  modLookahead x _ = x
  startPoint rule = LR0Point Nothing "" [] [NonTerm rule]
  lookaheadMatches _ _ = True
  makeFirstPoint _ _ h b _ act = LR0Point act h [] b
  showLookahead = const ""

data LR1Point = LR1Point {
    lr1Lr0Point :: LR0Point
  , lr1PointLookahead :: S.Set Symbol
  } deriving (Show, Eq, Ord)

instance LRPoint LR1Point where
  type Lookahead LR1Point = Symbol
  lr0 = lr1Lr0Point
  modLr0 p v = p{lr1Lr0Point = v}
  pointLookahead = lr1PointLookahead
  modLookahead p v = p{lr1PointLookahead=v}
  startPoint rule = LR1Point (startPoint rule) (S.singleton TermEof)
  makeFirstPoint r p@LR1Point{lr1PointLookahead=la} h b beta act
    = LR1Point (makeFirstPoint r (lr0 p) h b beta act) $
      if Nothing `S.member` firstBeta
      then S.union la (S.map fromJust (S.delete Nothing firstBeta))
      else S.map fromJust firstBeta
    where firstBeta = first r beta
  lookaheadMatches p x = S.member x (lr1PointLookahead p)
  showLookahead = intercalate "/" . map showSymbol . S.toList . pointLookahead

nextSym :: LRPoint a => a -> Maybe Symbol
nextSym p | (x:_) <- pointRight p = Just x
          | otherwise = Nothing

type LRState p = S.Set p
type LRAutomaton p = (LRState p, M.Map (LRState p, Symbol) (LRState p))

makeLRParser :: forall p m. (LRPoint p, Monad m)
             => Proxy p -> String -> FilePath -> String -> [Symbol]
             -> MyMonadT m [(FilePath,String)]
makeLRParser _ input base name tokens
  = writeLRParser base name tokens r $ buildLRAutomaton @p start r
  where rules = parse input
        start = let Rule h _ :| _ = rules in h
        r = mkRulesMap (Rule "%S" (([NonTerm start, TermEof], Nothing) :| []) <| rules)

data Action p = Accept | Shift Word | Reduce ((String, [Symbol]), String) | Reject deriving (Show, Eq, Ord)

writeLRParser :: forall p m. (LRPoint p, Monad m) => FilePath -> String -> [Symbol] -> RulesMap -> LRAutomaton p -> MyMonadT m [(FilePath,String)]
writeLRParser base name tokens r t = do
  actionTable <- make2dTableM mkActionTableCell states tokens
  gotoTable <- make2dTableM mkGotoTableCell states nonTerminals
  return [
      (base <> ".txt",  printTable r t)
    , (base <> ".h", "\
\#ifndef "<> map toUpper name <> "_H\n\
\#define "<> map toUpper name <> "_H\n\
\#include \"lexer.h\"\n\
\#include \"parseResult.h\"\n\
\#include <stack>\n\
\#include <variant>\n\
\class "<>name<>" {\
  \Lexer *lex;\
  \bool debug;\
  \std::stack<std::pair<std::size_t,std::variant<ResultType,Token>>> stack;\
  \static const std::size_t Action["<>show (length states)<>"]["
    <>show (length tokens)<>"];\
  \static const std::size_t GOTO["<>show (length states)<>"]["
    <>show (length nonTerminals)<>"];\
  \std::size_t top() const;\
\public:\
  \"<>name<>"(Lexer *lex, bool debug = false);\
  \ResultType parse();\
\};\n\
\#endif\n\
\") , (base <> ".cpp", "\
  \#include \""<>base<>".h\"\n\
  \#include <stdexcept>\n\
  \#include <iostream>\n\
  \const std::size_t "<>name<>"::Action["<>show (length states)<>"]["
    <>show (length tokens)<>"] = {" <> actionTable <> "};\
  \const std::size_t "<>name<>"::GOTO["<>show (length states)<>"]["
    <>show (length nonTerminals)<>"] = {" <> gotoTable <> "};\
  \std::size_t "<>name<>"::top() const { return stack.empty() ? 0 : stack.top().first; }\
  \"<>name<>"::"<>name<>"(Lexer *lex, bool debug):lex(lex),debug(debug) {}\
  \ResultType "<>name<>"::parse() {\
    \Token a = lex->getNextToken();\
    \while (true) {\
      \auto action = Action[top()][static_cast<std::size_t>(a.type)];\
      \switch (action) {\
      \"<> concatMap writeAction actionsMapList <> "\
      \default:\
        \if(debug)std::cerr<<\"Shift to \"<<action<<std::endl;\
        \stack.push({action, a});\
        \a=lex->getNextToken();\
        \break;\
      \}\
    \}\
  \}\n")]
  where
  make2dTableM mkcell rows cols = intercalate "," <$> mapM mkrow rows
        where mkrow row = ("{"<>) . (<>"}") . intercalate "," <$> mapM (mkcell row) cols
  mkGotoTableCell st nt = return . maybe "0" show $
     M.lookup (st, NonTerm nt) (snd t) >>= flip M.lookup statesMap
  actionIndex a = show $ fromJust $ M.lookup a actionsMap
  mkActionTableCell st tok = actionIndex <$> case possibleActions st tok of
    [] -> return Reject
    [x] -> return x
    xs -> reportConflicts st tok xs >> return (head xs)
  reportConflicts st tok xs = tell $
    "Conflicts detected in state " <> stateIndex st <> " with token " <> showSymbol tok <> "."
    : "Choosing the first alternative (usually shift):"
    : map showAction xs
  stateIndex st = show $ fromJust $ M.lookup st statesMap
  showAction (Shift n) = "Shift to state " <> show n
  showAction (Reduce ((h, b), _)) = "Reduce " <> h <> " -> " <> showBody b
  showAction Accept = "Accept"
  showAction Reject = "Reject"
  possibleActions st tok = shiftActions st tok <> reduceActions st tok
  shiftActions st tok
    | Just st' <- M.lookup (st, tok) (snd t)
    , Just nst' <- M.lookup st' statesMap
    = [Shift nst']
    | otherwise = []
  reduceActions st tok
    | any lastPoint st, tok == TermEof = [Accept]
    | ps <- mapMaybe pointAction . filter (`lookaheadMatches` tok) $ S.toList (pointClosure r st)
    = map Reduce ps
    where lastPoint p | null (pointRight p)
                      , null (pointHead p)
                      = True
                      | otherwise = False
  writeAction (Shift _, _) = ""
  writeAction (a, n) = "case " <> show n <> ": "
    <> actionBody a
    <> "break;"
  actionBody Accept = "return std::get<0>(stack.top().second);"
  actionBody Reject = "throw std::runtime_error(\"Reject\");"
  actionBody (Shift st) = "\
    \stack.push("<> show st <>");\
    \a = lex->getNextToken();\
  \"
  actionBody (Reduce ((h, body), code)) = "{"
    <> "if(debug) std::cerr << \"Reduce using "<> h <>" -> "<>showBody body<>";\\n\";"
    <> concat (reverse $ zipWith showArg body [1::Word ..])
    <> "auto gt = " <> goto <> ";"
    <> "if(gt==0) throw std::runtime_error(\"No goto\");"
    <> "if(debug) std::cerr << top() << \" is now on top of the stack;\\n\""
    <> "<< gt <<\" will be placed on the stack\" << std::endl;"
    <> "stack.push({gt,"<>result<>"});"
    <> "}"
    where
      result = "([]("<>argDefs<>") {" <> code <> "})("<>args<>")"
      goto = "GOTO[top()]["<>show (nonTermIdx h)<>"/*"<>h<>"*/]"
      argDefs = intercalate "," $ zipWith showArgDef body [1::Word ..]
      args = intercalate "," $ zipWith showCallArg body [1::Word ..]
      showArgDef _ i = "const auto &_" <> show i
      showCallArg _ i = "_" <> show i
      showArg (NonTerm _) i = "auto _"<>show i<>"=std::get<0>(stack.top().second); stack.pop();"
      showArg _ i = "auto _"<>show i<>"=std::get<1>(stack.top().second); stack.pop();"
  actionsMap = M.fromList actionsMapList
  actionsMapList = zip actions [0::Word ..]
  states = fst t : S.toList (S.fromList $ map snd tl)
  tl = M.toList (snd t)
  nonTermIdx nt = fromJust $ M.lookup nt nonTerminalsMap
  nonTerminalsMap = M.fromList $ zip nonTerminals [0::Word ..]
  nonTerminals = nub $ mapMaybe (isNonTerm . snd) $ M.keys (snd t)
  isNonTerm (NonTerm x) = Just x
  isNonTerm _ = Nothing
  statesMapList = zip states [0::Word ..]
  statesMap = M.fromList statesMapList
  allReductions = nub $ concatMap getReduction states
  getReduction st = mapMaybe pointAction $ S.toList (pointClosure r st)
  actions = Reject : map (Shift . snd) (tail statesMapList)
    <> (Accept : map Reduce allReductions)

buildLRAutomaton :: forall p. LRPoint p => String -> RulesMap -> LRAutomaton p
buildLRAutomaton oldStartRule r = (,) startState . fst . flip execState (M.empty, S.empty) $ buildGotoTable startState
  where
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

printTable :: LRPoint p => RulesMap -> LRAutomaton p -> String
printTable r t = --show t
  tableString (repeat def) unicodeS (titlesH ["stateNo", "stateDef"]) stateRows
  <> "\n" <> tableString (repeat def) unicodeS (titlesH titles) (map rowG rows)
  where
  stateMapList = zip states [0::Word ..]
  stateMap = M.fromList stateMapList
  stateRows = map (\(s, n) -> colsAllG top [[show n], showState s]) stateMapList
  showState st = map showPoint (S.toList st) <> ("---" : map showPoint (S.toList new))
    where cls = pointClosure r st
          new = cls S.\\ st
  showPoint p
    = unwords (pointHead p : "->" : map showSymbol (pointLeft p) <> ["."] <> map showSymbol (pointRight p)) <> ","
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
pointClosure r = unify . flip evalState S.empty . fmap S.unions . mapM doAdd . S.toList
  where
  unify = S.fromList . map combine . NE.groupBy ((==) `on` lr0) . sortBy (compare `on` lr0) . S.toList
  combine (x NE.:| xs) = modLookahead x (S.unions $ map pointLookahead (x:xs))
  doAdd p | (NonTerm nt:beta) <- pointRight p = do
    seen <- gets (S.member p)
    if not seen
    then do
      modify (S.insert p)
      let Just alts = M.lookup nt r
          new = map (\(b, act) -> makeFirstPoint r p nt b beta act) $ NE.toList alts
      S.union (S.fromList (p : new)) . S.unions <$> mapM doAdd new
    else return S.empty
  doAdd x = return (S.singleton x)
