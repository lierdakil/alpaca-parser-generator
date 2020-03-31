{-# LANGUAGE FlexibleContexts
           , TypeFamilies
           , TypeApplications
           , ScopedTypeVariables
           , TupleSections
           #-}
module ParseLR (Proxy(..), LR1Point, LR0Point, makeLRParser, buildLRAutomaton) where

import Grammar
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Data.List
import Data.Void
import Data.Char
import Data.Proxy
import Control.Monad.State
import Text.Layout.Table
import qualified Control.Arrow as A

class Ord a => LRPoint a where
  type family Lookahead a
  nextPoint :: Symbol -> a -> Maybe a
  pointHead :: a -> String
  pointLeft :: a -> [Symbol]
  pointRight :: a -> [Symbol]
  pointAction :: a -> Maybe ((String, [Symbol]), String)
  startPoint :: String -> a
  showLookahead :: a -> String
  makeFirstPoint :: RulesMap -> a -> String -> [Symbol] -> [Symbol] -> Maybe String -> a
  lookaheadMatches :: a -> Symbol -> Bool

instance LRPoint LR1Point where
  type Lookahead LR1Point = Symbol
  nextPoint s p@LR1Point{lr1PointRight=x:beta}
    | s == x = Just $ p{lr1PointLeft=x:lr1PointLeft p,lr1PointRight=beta}
  nextPoint _ _ = Nothing
  pointHead = lr1PointHead
  pointLeft = reverse . lr1PointLeft
  pointRight = lr1PointRight
  pointAction p | null (pointRight p) = ((pointHead p, pointLeft p), ) <$> lr1PointAction p
                | otherwise = Nothing
  startPoint rule = LR1Point Nothing "" [] [NonTerm rule] (S.singleton TermEof)
  showLookahead LR1Point{lr1PointLookahead=la} = intercalate "/" (map showSymbol $ S.toList la)
  makeFirstPoint r LR1Point{lr1PointLookahead=la} h b beta act = LR1Point act h [] b $
      if Nothing `S.member` firstBeta
      then S.union la (S.map fromJust (S.delete Nothing firstBeta))
      else S.map fromJust firstBeta
    where firstBeta = first r beta
  lookaheadMatches p x = S.member x (lr1PointLookahead p)

data LR1Point = LR1Point {
    lr1PointAction :: Maybe String
  , lr1PointHead :: String
  , lr1PointLeft :: [Symbol]
  , lr1PointRight :: [Symbol]
  , lr1PointLookahead :: S.Set Symbol
  } deriving (Show, Eq, Ord)

data LR0Point = LR0Point {
    lr0PointAction :: Maybe String
  , lr0PointHead :: String
  , lr0PointLeft :: [Symbol]
  , lr0PointRight :: [Symbol]
  } deriving (Show, Eq, Ord)

instance LRPoint LR0Point where
  type Lookahead LR0Point = Void
  nextPoint s p@LR0Point{lr0PointRight=x:beta}
    | s == x = Just $ p{lr0PointLeft=x:lr0PointLeft p,lr0PointRight=beta}
  nextPoint _ _ = Nothing
  pointHead = lr0PointHead
  pointLeft = reverse . lr0PointLeft
  pointRight = lr0PointRight
  pointAction p | null (pointRight p) = ((pointHead p, pointLeft p), ) <$> lr0PointAction p
                | otherwise = Nothing
  startPoint rule = LR0Point Nothing "" [] [NonTerm rule]
  showLookahead _ = ""
  makeFirstPoint _ _ h b _ act = LR0Point act h [] b
  lookaheadMatches _ _ = True

nextSym :: LRPoint a => a -> Maybe Symbol
nextSym p | (x:_) <- pointRight p = Just x
          | otherwise = Nothing

type LRState p = S.Set p
type LRAutomaton p = (LRState p, M.Map (LRState p, Symbol) (LRState p))

makeLRParser :: forall p. LRPoint p => Proxy p -> String -> [Symbol] -> String -> String
makeLRParser _ name tokens input = writeLRParser name tokens rules . buildLRAutomaton @p $ rules
  where rules = parse input

data Action p = Accept | Shift Word | Reduce ((String, [Symbol]), String) | Reject deriving (Show, Eq, Ord)

writeLRParser :: LRPoint p => String -> [Symbol] -> Rules -> LRAutomaton p -> String
writeLRParser name tokens rules t = "\
\/*\n\
\" <> printTable r t <> "\
\\n*/\n\
\#ifndef "<> map toUpper name <> "_H\n\
\#define "<> map toUpper name <> "_H\n\
\#include \"lexer.h\"\n\
\#include \"parseResult.h\"\n\
\#include <stack>\n\
\#include <stdexcept>\n\
\#include <string>\n\
\#include <variant>\n\
\#include <functional>\n\
\class "<>name<>" {\
  \Lexer *lex;\
  \bool debug;\
  \std::stack<std::size_t> stack;\
  \std::stack<std::variant<ResultType,Token>> resultStack;\
  \static constexpr const std::size_t Action[]["
  <>show (length tokens)<>"] = {" <> actionTable <> "};\
  \static constexpr const std::size_t GOTO[]["
  <>show (length nonTerminals)<>"] = {" <> gotoTable <> "};\
\public:\
  \"<>name<>"(Lexer *lex, bool debug = false):lex(lex),debug(debug) {}\
  \ResultType parse() {\
    \stack.push(0);\
    \Token a = lex->getNextToken();\
    \while (!stack.empty()) {\
      \auto action = Action[stack.top()][static_cast<std::size_t>(a.type)];\
      \switch (action) {\
      \"<> concatMap writeAction actionsMapList <> "\
      \default:\
        \if(debug)std::cerr<<\"Shift to \"<<action<<std::endl;\
        \stack.push(action);\
        \resultStack.push(a);\
        \a=lex->getNextToken();\
        \break;\
      \}\
    \}\
    \throw new std::runtime_error(\"Empty stack while still parsing\");\
  \}\
\};\n\
\#endif\n\
\"
  where
  r = mkRulesMap rules
  make2dTable mkcell rows cols = intercalate "," $ map mkrow rows
    where mkrow row = "{"<>intercalate "," (map (mkcell row) cols)<>"}"
  actionTable = make2dTable mkActionTableCell states tokens
  gotoTable = make2dTable mkGotoTableCell states nonTerminals
  mkGotoTableCell st nt = maybe "0" show $
     M.lookup (st, NonTerm nt) (snd t) >>= flip M.lookup statesMap
  actionIndex a = show $ fromJust $ M.lookup a actionsMap
  mkActionTableCell st tok = actionIndex $ case possibleActions st tok of
    [] -> Reject
    [x] -> x
    xs -> reportConflicts st xs
  reportConflicts st xs = error $ "Conflicts detected in state " <> stateIndex st <> ":\n"
    <> unlines (map showAction xs)
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
  actionBody Accept = "return std::get<0>(resultStack.top());"
  actionBody Reject = "throw std::runtime_error(\"Reject\");"
  actionBody (Shift st) = "\
    \stack.push("<> show st <>");\
    \a = lex->getNextToken();\
  \"
  actionBody (Reduce ((h, body), code)) = "{"
    <> "if(debug) std::cerr << \"Reduce using "<> h <>" -> "<>showBody body<>"\" << std::endl;"
    <> concat (reverse $ zipWith showArg body [1::Word ..])
    <> "resultStack.push(([]("<>argDefs<>") {" <> code <> "})("<>args<>"));"
    <> "stack.push(GOTO[stack.top()]["<>show (nonTermIdx h)<>"/*"<>h<>"*/]);"
    <> "}"
    where
      argDefs = intercalate "," $ zipWith showArgDef body [1::Word ..]
      args = intercalate "," $ zipWith showCallArg body [1::Word ..]
      showArgDef _ i = "const auto &_" <> show i
      showCallArg _ i = "_" <> show i
      showArg (NonTerm _) i = "auto _"<>show i<>"=std::get<0>(resultStack.top()); resultStack.pop(); stack.pop();"
      showArg _ i = "auto _"<>show i<>"=std::get<1>(resultStack.top()); resultStack.pop(); stack.pop();"
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

printTable :: LRPoint p => RulesMap -> LRAutomaton p -> String
printTable r t = --show t
  tableString (repeat def) unicodeS (titlesH ["stateNo", "stateDef"]) stateRows
  <> "\n" <> tableString (repeat def) unicodeS (titlesH titles) (map rowG rows)
  where
  stateMapList = zip states [0::Word ..]
  stateMap = M.fromList stateMapList
  stateRows = map (\(s, n) -> colsAllG top [[show n], showState s]) stateMapList
  showState = map showPoint . S.toList . pointClosure r
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
pointClosure r = flip evalState S.empty . fmap S.unions . mapM doAdd . S.toList
  where
  doAdd p
    | (NonTerm nt:beta) <- pointRight p = do
    seen <- gets (S.member nt)
    if not seen
    then do
      modify (S.insert nt)
      let Just alts = M.lookup nt r
          new = map (\(b, act) -> makeFirstPoint r p nt b beta act) alts
      S.union (S.fromList (p : new)) . S.unions <$> mapM doAdd new
    else return (S.singleton p)
  doAdd x = return (S.singleton x)
