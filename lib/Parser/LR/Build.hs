{-# LANGUAGE FlexibleContexts
           , TypeFamilies
           , TypeApplications
           , ScopedTypeVariables
           , PatternSynonyms
           , OverloadedStrings
           , QuasiQuotes
           , TupleSections
           , RecordWildCards
           #-}
module Parser.LR.Build where

import Grammar
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.List
import MonadTypes
import Control.Monad.State
import Text.Layout.Table
import qualified Control.Arrow as A
import Data.Text (Text)
import qualified Data.Text as T
import Utils
import Parser.LR.Point
import Parser.Types
import Data.Function (on)

data Action = Shift Word
    | Reduce ((Text, [Symbol]), Maybe Text)
    | Reject
    deriving (Show, Eq, Ord)

data LRParser p = LRParser {
    lrTerminals :: [Symbol]
  , lrNonTerminals :: [Symbol]
  , lrStates :: [Word]
  , lrExpected :: M.Map Word [Text]
  , lrStateSym :: M.Map Word Symbol
  , lrAction :: M.Map (Word, Symbol) Action
  , lrGoto :: M.Map (Word, Symbol) Word
  , lrInitialState :: Word
  }

instance LRPoint p => Parser (LRParser p) where
  -- buildParser :: Monad m => Proxy parser -> ParserOptions Rules -> MyMonadT m ([(FilePath,Text)], parser)
  buildParser _ ParserOptions{..} = do
    tokens <- get
    automaton@(startState, transitionTable) <- postprocess $ buildLRAutomaton @p start r
    let shiftActions st tok
          | Just st' <- M.lookup (st, tok) transitionTable
          , Just nst' <- M.lookup st' stateMap
          = [(Shift nst', pointAssoc $ S.findMin st')]
          | otherwise = []
        mkGotoTableCell st nt = return . ((stateIndex st, nt),) . fromMaybe 0 $
           M.lookup (st, nt) transitionTable >>= flip M.lookup stateMap
        possibleActions st tok = tryResolveConflicts $ shiftActions st tok <> reduceActions st tok
        states = startState : S.toList (S.fromList $ M.elems transitionTable)
        nonTerminals = nub $ mapMaybe (isNonTerm . snd) $ M.keys transitionTable
        stateMap = M.fromList $ zip states [0::Word ..]
        mkActionTableCell st tok = ((stateIndex st, tok),) <$> do
          pa <- possibleActions st tok
          case pa of
            [] -> return Reject
            [x] -> return x
            (x:xs) -> reportConflicts st tok (x:|xs) >> return x
        reportConflicts st tok (x:|xs) = do
          tell $
            [interp'|Conflicts detected in state #{stateIndex st} with token #{showSymbol tok}.|]
            : map showAction (x:xs)
          tell ["Will use " <> showAction x]
        stateIndex st = fromJust $ M.lookup st stateMap
        expectedSymbols = map expected states
          where
            expected st
              = (stateIndex st, map next $ S.toList st)
            next st
              | (x:_) <- pointRight st = showSymbol x
              | otherwise = showLookahead st
        stateToSym = mapMaybe sym states
          where
            sym st
              | Just el <- S.lookupMin $ S.filter (not . null . pointLeft) st
              = Just (stateIndex st, last (pointLeft el))
              | otherwise
              = Nothing
    actionTable <- make2dTableM mkActionTableCell states tokens
    gotoTable <- make2dTableM mkGotoTableCell states nonTerminals
    let debug = [(parserOptionsBaseFileName<>".txt",printTable r automaton)]
    return (debug, LRParser {
        lrTerminals = tokens
      , lrNonTerminals = nonTerminals
      , lrStates = M.elems stateMap
      , lrExpected = M.fromList expectedSymbols
      , lrStateSym = M.fromList stateToSym
      , lrAction = M.fromList actionTable
      , lrGoto = M.fromList gotoTable
      , lrInitialState = stateIndex startState
      })
    where
    reduceActions st tok
      | ps <- mapMaybe pointAction . filter (`lookaheadMatches` tok)
                $ S.toList (pointClosure r st)
      = map (A.first Reduce) ps
    showAction (Shift n) = "Shift to state " <> tshow n
    showAction (Reduce ((h, b), _)) = [interp'|Reduce #{h} -> #{showBody b}|]
    showAction Reject = "Reject"
    make2dTableM mkcell rows cols = concat <$> mapM mkrow rows
        where mkrow row = mapM (mkcell row) cols
    rules = parserOptionsGrammarDefinition
    isNonTerm (NonTerm x) = Just $ NonTerm x
    isNonTerm _ = Nothing
    Rule start _ :| _ = rules
    r = mkRulesMap (Rule ExtendedStartRule (BodyWithAction Nothing (extendedStartRuleBody start) Nothing :| []) <| rules)

buildLRAutomaton :: forall p. LRPoint p => Text -> RulesMap -> LRAutomaton p
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

printTable :: LRPoint p => RulesMap -> LRAutomaton p -> Text
printTable r t = T.pack $ --tshow t
  tableString (repeat def) unicodeS (titlesH ["stateNo", "stateDef"]) stateRows
  <> "\n" <> tableString (repeat def) unicodeS (titlesH $ map T.unpack titles) (map (rowG . map T.unpack) rows)
  where
  stateMapList = zip states [0::Word ..]
  stateMap = M.fromList stateMapList
  stateRows = map (\(s, n) -> colsAllG top [[show n], map T.unpack $ showState s]) stateMapList
  showState st = map showPoint (S.toList st) <> ("---" : map showPoint (S.toList new))
    where cls = pointClosure r st
          new = cls S.\\ st
  showPoint p
    = T.unwords (pointHead p : "->" : map showSymbol (pointLeft p) <> ["."] <> map showSymbol (pointRight p)) <> ","
      <> showLookahead p
  states = fst t : (S.toList . S.fromList $ map snd tl)
  symbols = S.toList . S.fromList $ map (snd . fst) tl
  titles = "" : map showSymbol symbols
  tl = M.toList (snd t)
  rows = map makeRow stateMapList
  makeRow (st, n) = tshow n : map (makeCell st) symbols
  makeCell st sym
    | Just cell <- M.lookup (st, sym) (snd t) = tshow $ fromJust (M.lookup cell stateMap)
    | otherwise = ""

pointClosure :: LRPoint a => RulesMap -> S.Set a -> S.Set a
pointClosure r = unify . flip evalState S.empty . fmap S.unions . mapM doAdd . S.toList
  where
  unify = S.fromList . map combine . NE.groupAllWith lr0 . S.toList
  combine (x :| xs) = modLookahead x (S.unions $ map pointLookahead (x:xs))
  doAdd p | (NonTerm nt:beta) <- pointRight p = do
    seen <- gets (S.member p)
    if not seen
    then do
      modify (S.insert p)
      let Just alts = M.lookup nt r
          new = map (\BodyWithAction{..} -> makeFirstPoint r p nt bwaBody beta bwaAction bwaAssoc) $ NE.toList alts
      S.union (S.fromList (p : new)) . S.unions <$> mapM doAdd new
    else return S.empty
  doAdd x = return (S.singleton x)

tryResolveConflicts :: Monad m => [(Action, Maybe Assoc)] -> MyMonadT m [Action]
tryResolveConflicts [] = return []
tryResolveConflicts [x] = return [fst x]
tryResolveConflicts as = case undefined of
    _ | null (concat [lefta, righta, nonea]) -> failResult -- no assoc info
      | not (null noinfo) -> do
        tell ["Not all rules in conflict have associativity information"]
        failResult
      | countEq > 1 -> do
        tell ["Rules with the same priority but different associativity"]
        failResult
      | otherwise -> return result
  where
  result = uncurry resolve $ snd maxAssoc
  failResult = return $ map fst as
  (lefta, righta, nonea, noinfo) = foldr separate ([], [], [], []) as
  separate (x, Nothing) (l, r, na, ni) = (l, r, na, x:ni)
  separate (x, Just (AssocLeft, v)) (l, r, na, ni) = ((x,v):l, r, na, ni)
  separate (x, Just (AssocRight, v)) (l, r, na, ni) = (l, (x,v):r, na, ni)
  separate (x, Just (AssocNone, v)) (l, r, na, ni) = (l, r, (x,v):na, ni)
  maxLeft = max' AssocLeft lefta
  maxRight = max' AssocRight righta
  maxNone = max' AssocNone nonea
  max' t xs = let m = maximum' $ map snd xs
    in (m, (t, map fst $ filter ((==m) . Just . snd) xs))
  maximum' [] = Nothing
  maximum' xs = Just $ maximum xs
  assocs = [maxLeft, maxRight, maxNone]
  maxAssoc = maximumBy (compare `on` fst) assocs
  countEq :: Word
  countEq = foldr (\x acc -> if fst maxAssoc == fst x then 1+acc else acc) 0 assocs
  resolve _ [x] = [x]
  resolve AssocLeft xs = case filter isReduce xs of
    [] -> xs
    rs -> rs
  resolve AssocRight xs = case filter isShift xs of
    [] -> xs
    ss -> ss
  resolve AssocNone xs = xs
  isReduce Reduce{} = True
  isReduce _ = False
  isShift Shift{} = True
  isShift _ = False
