{-# LANGUAGE FlexibleContexts #-}
module ParseLL where

import Grammar
import Data.Maybe
import Data.List
import Data.Function
import qualified Data.Set as S
import qualified Data.Map as M
import Text.Layout.Table
import Control.Monad

type RulesMap = M.Map String [Alt]
type Rules = [Rule]
type Table = M.Map (Symbol, [Symbol]) (S.Set Symbol, Maybe String)

mkRulesMap :: Rules -> RulesMap
mkRulesMap = M.fromList . map ruleToTuple
  where ruleToTuple (Rule h alts) = (h, alts)

makeLLParser :: [Symbol] -> String -> String
makeLLParser tokens = buildLLParser tokens . parse

buildLLParser :: [Symbol] -> Rules -> String
buildLLParser tokens rules = "\
\/*\n" <> printTable t <> "\n*/\n\
\#ifndef PARSER_LL_H\n\
\#define PARSER_LL_H\n\
\#include \"lexer.h\"\n\
\#include \"parseResult.h\"\n\
\#include <stack>\n\
\#include <stdexcept>\n\
\#include <string>\n\
\#include <variant>\n\
\#include <functional>\n\
\class ParserLL {\
  \enum class NonTerminal : std::size_t { "<>intercalate ", " (map ("NT_" <>) nonTerms)<>" };\
  \using Symbol = std::variant<NonTerminal, TokenType, std::size_t>;\
  \static std::string to_string(NonTerminal nt) {\
  \static constexpr const char *names[] = {"
  <> intercalate "," (map quote nonTerms) <> " };\
  \return names[static_cast<std::size_t>(nt)];\
  \}\
  \Lexer *lex;\
  \Token curTok;\
  \bool debug;\
  \std::stack<Symbol> stack;\
  \std::stack<std::variant<ResultType,Token>> resultStack;\
  \static constexpr const std::size_t M[]["
  <>show (length tokens)<>"][2] = {" <> transTable <> "};\
\public:\
  \ParserLL(Lexer *lex, bool debug = false):lex(lex),debug(debug) {}\
  \ResultType parse() {\
    \stack.push("<> encodeSymbol startSymbol <> ");\
    \Token a = lex->getNextToken();\
    \while (!stack.empty()) {\
      \std::visit(\
          \[&a, this](auto X) {\
            \using T = std::decay_t<decltype(X)>;\
            \if constexpr (std::is_same_v<T, TokenType>) {\
              \if (a.type == X) {\
                \resultStack.push(a);\
                \a = lex->getNextToken();\
                \stack.pop();\
              \} else {\
                \throw std::runtime_error(\
                    \\"Found terminal \" + ::to_string(a.type) + \" but expected \" +\
                    \::to_string(X) + \".\");\
              \}\
            \} else if constexpr (std::is_same_v<T, NonTerminal>) {\
              \auto trans = M[static_cast<std::size_t>(X)]\
                                \[static_cast<std::size_t>(a.type)];\
              \stack.pop();\
              \stack.push(trans[1]);\
              \switch(trans[0]) {\
              \"<>bodies<>"\
              \case 0: throw std::runtime_error(\
                      \\"No transition for \"+to_string(X)+\
                      \\", \"+::to_string(a.type));\
              \}\
            \} else if constexpr (std::is_same_v<T, std::size_t>) {\
              \stack.pop();\
              \switch(X) {\
              \"<>actions<>"\
              \}\
            \}\
          \}, stack.top());\
    \}\
    \return std::get<0>(resultStack.top());\
  \}\
\};\n\
\#endif\n\
\"
  where
  quote x = '"':x <> "\""
  startSymbol = let (Rule h _) = head rules in NonTerm h
  r = mkRulesMap rules
  t = buildTable r
  nonTerms = M.keys r
  allBodies = nub $ map snd $ M.keys t
  allActions = nub $ concatMap (\(Rule _ alts) -> mapMaybe traverse' alts) rules
  traverse' (_, Nothing) = Nothing
  traverse' (x, Just v) = Just (x, v)
  transTable = makeTransTable
  bodies = concatMap (uncurry makeBody) $ zip [1::Word ..] allBodies
  actions = concatMap (uncurry makeAction) $ zip [1::Word ..] allActions
  makeBody n b = "case " <> show n <>": "
    <> "if(debug) std::cerr << to_string(X) << \" -> " <> showBody b <> "\" << std::endl;"
    <> concatMap (\s -> "stack.push(" <> encodeSymbol s <> ");") (reverse b)
    <> "break;"
  makeAction n (body, code)
    = "case " <> show n <>": {"
    <> concat (reverse $ zipWith showArg body [1::Word ..])
    <> "resultStack.push(([]("<>argDefs<>") {" <> code <> "})("<>args<>"));"
    <> "break; }"
    where
      argDefs = intercalate "," $ zipWith showArgDef body [1::Word ..]
      args = intercalate "," $ zipWith showCallArg body [1::Word ..]
      showArgDef _ i = "const auto &_" <> show i
      showCallArg _ i = "_" <> show i
      showArg (NonTerm _) i = "auto _"<>show i<>"=std::get<0>(resultStack.top()); resultStack.pop();"
      showArg _ i = "auto _"<>show i<>"=std::get<1>(resultStack.top()); resultStack.pop();"

  makeTransTable = tblStr
    where
      bodyMap = M.fromList $ zip allBodies [(0::Word)..]
      actionMap = M.fromList $ zip (map snd allActions) [(0::Word)..]
      tblStr = intercalate ","
             $ map (\row -> "{"<> intercalate "," (map showIdxs row) <>"}") cells
      showIdxs (a, b) = "{" <> showIdx a <> "," <> showIdx b <> "}"
      showIdx Nothing = "0"
      showIdx (Just x) = show (x+1)
      cells = map (flip map tokens . writeCell) nonTerms
      writeCell nonterm term
        | Just (act, b:_) <- M.lookup (NonTerm nonterm, term) tt
        = (M.lookup b bodyMap, act >>= flip M.lookup actionMap)
        | otherwise = (Nothing, Nothing)
      tt = M.fromListWith (liftM2 (++)) . concatMap r2t $ M.toList t
      r2t ((nt, b), (ts, act)) = map (\term -> ((nt, term), (act, [b]))) $ S.toList ts

tableToCode :: Table -> String
tableToCode t = "switch (nonTerminal) {" <>
  concatMap constructCase (groupBy ((==) `on` fst . fst) $ M.toList t)
  <> "}" <>
  "throw std::runtime_error(\
  \\"No transition for \"+to_string(nonTerminal)+\
  \\", \"+::to_string(terminal));"
  where
  constructCase xs@(((nt, _), _):_) = "case " <> encodeSymbol nt <> ":" <>
    intercalate " else " (map constructCond xs) <> "break;"
  constructCase _ = ""
  constructCond ((_, bs), (terms, _)) = "if(" <> termCond <> ") " <> handleBodies bs
    where
      termCond = intercalate "||" $ map (("terminal=="<>) . encodeSymbol)$ S.toList terms
      handleBodies b = "return {" <> intercalate "," (map encodeSymbol b) <> "};"

encodeSymbol :: Symbol -> String
encodeSymbol (NonTerm nt) = "NonTerminal::NT_" <> nt
encodeSymbol (Term t) = "TokenType::Tok_" <> t
encodeSymbol TermEof = "TokenType::eof"

printTable :: Table -> String
printTable t = tableString (repeat def) unicodeS (titlesH titles) (map rowG rows)
  -- unlines [show titles, show rows]
  where
  nonTerms = S.toList . S.fromList $ map (fst . fst) tl
  terms = S.toList $ S.unions $ map fst $ M.elems t
  titles = "" : map showSymbol terms
  tl = M.toList t
  tt = M.fromListWith (++) $ concatMap r2t tl
  r2t ((nt, b), (ts, _)) = map (\term -> ((nt, term), [b])) $ S.toList ts
  rows = map makeRow nonTerms
  makeRow nonTerm = showSymbol nonTerm : map (makeCell nonTerm) terms
  makeCell nonTerm term
    | Just cell <- M.lookup (nonTerm, term) tt = intercalate "\n" (map showBody' cell)
    | otherwise = ""
  showBody' [] = "ε"
  showBody' x = showBody x

buildTable :: RulesMap -> Table
buildTable r = M.fromListWith (<>) $ concatMap (uncurry oneRule) rules
  where
  rules = M.toList r
  oneRule h = map (uncurry (oneAlt h))
  oneAlt h alpha act = ((NonTerm h, alpha), (followSet `S.union` firstAlphaNoEps, act))
    where
    followSet
      | Nothing `S.member` firstAlpha
      = follow r (NonTerm h)
      | otherwise = S.empty
    firstAlpha = first r alpha
    firstAlphaNoEps = S.map fromJust (S.delete Nothing firstAlpha)

first :: RulesMap -> [Symbol] -> S.Set (Maybe Symbol)
first r = first'
  where
  first' (NonTerm t:xs)
    | Nothing `S.member` firstT
    = S.delete Nothing firstT `S.union` first r (filter (/= NonTerm t) xs)
    | otherwise = firstT
    where
    firstT = S.unions (map (first' . fst) $ fromMaybe [] $ M.lookup t r)
  first' (TermEof:_) = S.singleton $ Just TermEof
  first' (Term t:_) = S.singleton $ Just (Term t)
  first' [] = S.singleton Nothing

follow :: RulesMap -> Symbol -> S.Set Symbol
follow r t@(NonTerm _) = follow' S.empty t
  where
  rules = M.toList r
  follow' seen x
    | x `elem` seen = S.empty
    | otherwise = S.unions $ map (uncurry oneRule) rules
    where
    oneRule h = S.unions . map (oneAlt h . fst)
    oneAlt h b
      | _:beta <- dropWhile (/=x) b = go beta
      | otherwise = S.empty
      where
      go beta
        | Nothing `S.member` firstBeta
        = noEpsilon (S.delete Nothing firstBeta)
          `S.union` follow' (S.insert x seen) (NonTerm h)
        | otherwise = noEpsilon firstBeta
        where firstBeta = first r beta
              noEpsilon f = S.mapMonotonic fromJust f `S.union` oneAlt h beta
follow _ _ = S.empty
