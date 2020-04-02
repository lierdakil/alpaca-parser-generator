{-# LANGUAGE FlexibleContexts #-}
module ParseLL (makeLLParser) where

import Grammar
import Data.Maybe
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M
import Text.Layout.Table
import Control.Monad
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import MonadTypes

type Table = M.Map (Symbol, [Symbol]) (S.Set Symbol, Maybe String)

makeLLParser :: Monad m => String -> FilePath -> [Symbol] -> MyMonadT m [(FilePath,String)]
makeLLParser = buildLLParser . parse

buildLLParser :: Monad m => Rules -> FilePath -> [Symbol] -> MyMonadT m [(FilePath,String)]
buildLLParser rules basename tokens = do
  lr <- isLeftRecursive r
  when lr $ throwError ["LL(1) parser can not handle left-recursive grammar"]
  cells <- mapM (forM tokens . writeCell) nonTerms
  let transTable = intercalate "," . map ((braces . intercalate ",") . map showIdxs) $ cells
  return [
      (basename <> ".txt", printTable t)
    , (basename <> ".h", headerFile)
    , (basename <> ".cpp", sourceFile transTable)
    ]
  where
  headerFile = "\
\#ifndef PARSER_LL_H\n\
\#define PARSER_LL_H\n\
\#include \"lexer.h\"\n\
\#include \"parseResult.h\"\n\
\#include <stack>\n\
\#include <string>\n\
\#include <variant>\n\
\class ParserLL {\
  \enum class NonTerminal : std::size_t { "<>intercalate ", " (map ("NT_" <>) nonTerms)<>" };\
  \using Symbol = std::variant<NonTerminal, TokenType, std::size_t>;\
  \static const std::string to_string(NonTerminal nt);\
  \Lexer *lex;\
  \bool debug;\
  \std::stack<Symbol> stack;\
  \std::stack<std::variant<ResultType,Token>> resultStack;\
  \static const std::size_t M["<>show(length nonTerms)<>"]["<>show (length tokens)<>"][2];\
\public:\
  \ParserLL(Lexer *lex, bool debug);\
  \ResultType parse();\
\};\n\
\#endif\n\
\"
  sourceFile transTable = "\
\#include \""<>basename<>".h\"\n\
\#include <stdexcept>\n\
\#include <iostream>\n\
\const std::string ParserLL::to_string(NonTerminal nt) {\
  \static constexpr const char *names[] = {\
  \"<> intercalate "," (map quote nonTerms) <> " };\
  \return names[static_cast<std::size_t>(nt)];\
\}\
\const std::size_t ParserLL::M["<>show(length nonTerms)<>"][\
\"<>show (length tokens)<>"][2] = {" <> transTable <> "};\
\ParserLL::ParserLL(Lexer *lex, bool debug = false):lex(lex),debug(debug) {}\
\ResultType ParserLL::parse() {\
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
\"
  bodyMap = M.fromList $ zip allBodies [(0::Word)..]
  actionMap = M.fromList $ zip (map snd allActions) [(0::Word)..]
  showIdxs (a, b) = braces $ showIdx a <> "," <> showIdx b
  showIdx :: Maybe Word -> String
  showIdx Nothing = "0"
  showIdx (Just x) = show (x+1)
  writeCell nonterm term
    | Just (act, b:rest) <- M.lookup (NonTerm nonterm, term) tt
    = do
        unless (null rest) $
          tell ["LL parser has multiple rules in the same cell: " <> show (b:rest),
                "\tChoosing " <> show b]
        return (M.lookup b bodyMap, act >>= flip M.lookup actionMap)
    | otherwise = return (Nothing, Nothing)
  tt = M.fromListWith (liftM2 (++)) . concatMap r2t $ M.toList t
  r2t ((nt, b), (ts, act)) = map (\term -> ((nt, term), (act, [b]))) $ S.toList ts
  braces x = "{"<>x<>"}"
  quote x = '"':x <> "\""
  startSymbol = let (Rule h _ :| _) = rules in NonTerm h
  r = mkRulesMap rules
  t = buildTable r
  nonTerms = M.keys r
  allBodies = nub $ map snd $ M.keys t
  allActions = nub $ concatMap (\(Rule _ alts) -> mapMaybe traverse' $ NE.toList alts) rules
  traverse' (_, Nothing) = Nothing
  traverse' (x, Just v) = Just (x, v)
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

encodeSymbol :: Symbol -> String
encodeSymbol (NonTerm nt) = "NonTerminal::NT_" <> nt
encodeSymbol (Term t) = "TokenType::Tok_" <> t
encodeSymbol TermEof = "TokenType::eof"

printTable :: Table -> String
printTable t = tableString (repeat def) unicodeS (titlesH titles) rows
  -- unlines [show titles, show rows]
  where
  nonTerms = S.toList . S.fromList $ map (fst . fst) tl
  terms = S.toList $ S.unions $ map fst $ M.elems t
  titles = "" : map showSymbol terms
  tl = M.toList t
  tt = M.fromListWith (++) $ concatMap r2t tl
  r2t ((nt, b), (ts, _)) = map (\term -> ((nt, term), [b])) $ S.toList ts
  rows = map makeRow nonTerms
  makeRow nonTerm = colsAllG top $ [showSymbol nonTerm] : map (makeCell nonTerm) terms
  makeCell nonTerm term
    | Just cell <- M.lookup (nonTerm, term) tt = map showBody' cell
    | otherwise = []
  showBody' [] = "ε"
  showBody' x = showBody x

buildTable :: RulesMap -> Table
buildTable r = M.fromListWith (<>) $ concatMap (uncurry oneRule) rules
  where
  rules = M.toList r
  oneRule h = map (uncurry (oneAlt h)) . NE.toList
  oneAlt h alpha act = ((NonTerm h, alpha), (followSet `S.union` firstAlphaNoEps, act))
    where
    followSet
      | Nothing `S.member` firstAlpha
      = follow r (NonTerm h)
      | otherwise = S.empty
    firstAlpha = first r alpha
    firstAlphaNoEps = S.map fromJust (S.delete Nothing firstAlpha)
