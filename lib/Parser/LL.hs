{-# LANGUAGE FlexibleContexts, OverloadedStrings, QuasiQuotes #-}
module Parser.LL (makeLLParser) where

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
import Utils
import Data.Text (Text)
import qualified Data.Text as T

type Table = M.Map (Symbol, [Symbol]) (S.Set Symbol, Maybe Text)

makeLLParser :: Monad m => Text -> FilePath -> [Symbol] -> MyMonadT m [(FilePath,Text)]
makeLLParser t f s = parse t >>= buildLLParser f s

buildLLParser :: Monad m => FilePath -> [Symbol] -> Grammar -> MyMonadT m [(FilePath,Text)]
buildLLParser basename tokens (Grammar gtop rules) = do
  lr <- isLeftRecursive r
  when lr $ throwError ["LL(1) parser can not handle left-recursive grammar"]
  cells <- mapM (forM tokens . writeCell) nonTerms
  let transTable = T.intercalate "," . map ((braces . T.intercalate ",") . map showIdxs) $ cells
  return [
      (basename <> ".txt", printTable t)
    , (basename <> ".h", headerFile)
    , (basename <> ".cpp", sourceFile transTable)
    ]
  where
  headerFile = [interp|
#ifndef PARSER_LL_H
#define PARSER_LL_H
#include "lexer.h"
#include "parseResult.h"
#include <stack>
#include <string>
#include <variant>
#{gtop}
class ParserLL {
  enum class NonTerminal : std::size_t { #{T.intercalate ", " (map ("NT_" <>) nonTerms)} };
  using Symbol = std::variant<NonTerminal, TokenType, std::size_t>;
  static const std::string to_string(NonTerminal nt);
  Lexer *lex;
  bool debug;
  std::stack<Symbol> stack;
  std::stack<std::variant<ResultType,Token>> resultStack;
  static const std::size_t M[#{tshow(length nonTerms)}][#{tshow (length tokens)}][2];
public:
  ParserLL(Lexer *lex, bool debug);
  ResultType parse();
};
#endif
|]
  sourceFile transTable = [interp|
#include "#{basename}.h"
#include <stdexcept>
#include <iostream>
#{gtop}
const std::string ParserLL::to_string(NonTerminal nt) {
  static constexpr const char *names[] = { #{T.intercalate "," (map quote nonTerms)} };
  return names[static_cast<std::size_t>(nt)];
}
const std::size_t ParserLL::M[#{tshow(length nonTerms)}][#{tshow (length tokens)}][2] = {
  #{transTable}
};
ParserLL::ParserLL(Lexer *lex, bool debug = false):lex(lex),debug(debug) {}
ResultType ParserLL::parse() {
  stack.push(#{encodeSymbol startSymbol});
  Token a = lex->getNextToken();
  while (!stack.empty()) {
    std::visit(
        [&a, this](auto X) {
          using T = std::decay_t<decltype(X)>;
          if constexpr (std::is_same_v<T, TokenType>) {
            if (a.type == X) {
              resultStack.push(a);
              a = lex->getNextToken();
              stack.pop();
            } else {
              throw std::runtime_error(
                  "Found terminal " + ::to_string(a.type) + " but expected " +
                  ::to_string(X) + ".");
            }
          } else if constexpr (std::is_same_v<T, NonTerminal>) {
            auto trans = M[static_cast<std::size_t>(X)]
                              [static_cast<std::size_t>(a.type)];
            stack.pop();
            stack.push(trans[1]);
            switch(trans[0]) {
              #{bodies}
            case 0: throw std::runtime_error(
                    "No transition for "+to_string(X)+
                    ", "+::to_string(a.type));
            }
          } else if constexpr (std::is_same_v<T, std::size_t>) {
            stack.pop();
            switch(X) {
              #{actions}
            }
          }
        }, stack.top());
  }
  return std::get<0>(resultStack.top());
}
|]
  bodyMap = M.fromList $ zip allBodies [(0::Word)..]
  actionMap = M.fromList $ zip (map snd allActions) [(0::Word)..]
  showIdxs (a, b) = braces $ showIdx a <> "," <> showIdx b
  showIdx :: Maybe Word -> Text
  showIdx Nothing = "0"
  showIdx (Just x) = tshow (x+1)
  writeCell nonterm term
    | Just (act, b:rest) <- M.lookup (NonTerm nonterm, term) tt
    = do
        unless (null rest) $
          tell [[interp|LL parser has multiple rules in the same cell: #{tshow (b:rest)}|],
                [interp|Choosing #{tshow b}|]]
        return (M.lookup b bodyMap, act >>= flip M.lookup actionMap)
    | otherwise = return (Nothing, Nothing)
  tt = M.fromListWith (liftM2 (++)) . foldMap r2t $ M.toList t
  r2t ((nt, b), (ts, act)) = map (\term -> ((nt, term), (act, [b]))) $ S.toList ts
  braces x = "{"<>x<>"}"
  quote x = "\"" <> x <> "\""
  startSymbol = let (Rule h _ :| _) = rules in NonTerm h
  r = mkRulesMap rules
  t = buildTable r
  nonTerms = M.keys r
  allBodies = nub $ map snd $ M.keys t
  allActions = nub $ foldMap (\(Rule _ alts) -> mapMaybe traverse' $ NE.toList alts) rules
  traverse' (_, Nothing) = Nothing
  traverse' (x, Just v) = Just (x, v)
  bodies = foldMap (uncurry makeBody) $ zip [1::Word ..] allBodies
  actions = foldMap (uncurry makeAction) $ zip [1::Word ..] allActions
  makeBody n b = [interp|
  case #{tshow n}:
    if(debug) std::cerr << to_string(X) << " -> #{showBody b}" << std::endl;
    #{foldMap pushSymbol (reverse b)}
    break;
  |] :: Text
  pushSymbol s = [interp|stack.push(#{encodeSymbol s});|] :: Text
  makeAction n (body, code) = [interp|
    case #{tshow n}: {
      #{T.concat (reverse $ zipWith showArg body [1::Word ..])}
      resultStack.push(([](#{argDefs}) { #{code} })(#{args}));
      break;
    }|] :: Text
    where
      argDefs = T.intercalate "," $ zipWith showArgDef body [1::Word ..]
      args = T.intercalate "," $ zipWith showCallArg body [1::Word ..]
      showArgDef _ i = "const auto &_" <> tshow i
      showCallArg _ i = "_" <> tshow i
      showArg (NonTerm _) i = [interp|
        auto _#{tshow i}=std::get<0>(resultStack.top());
        resultStack.pop();|]
      showArg _ i = [interp|
        auto _#{tshow i}=std::get<1>(resultStack.top());
        resultStack.pop();|]

encodeSymbol :: Symbol -> Text
encodeSymbol (NonTerm nt) = "NonTerminal::NT_" <> nt
encodeSymbol (Term t) = "TokenType::Tok_" <> t
encodeSymbol TermEof = "TokenType::eof"

printTable :: Table -> Text
printTable t = T.pack $ tableString (repeat def) unicodeS (titlesH $ map T.unpack titles) rows
  -- unlines [tshow titles, tshow rows]
  where
  nonTerms = S.toList . S.fromList $ map (fst . fst) tl
  terms = S.toList $ S.unions $ map fst $ M.elems t
  titles = "" : map showSymbol terms
  tl = M.toList t
  tt = M.fromListWith (++) $ foldMap r2t tl
  r2t ((nt, b), (ts, _)) = map (\term -> ((nt, term), [b])) $ S.toList ts
  rows = map makeRow nonTerms
  makeRow nonTerm = colsAllG top $ [T.unpack $ showSymbol nonTerm] : map (makeCell nonTerm) terms
  makeCell nonTerm term
    | Just cell <- M.lookup (nonTerm, term) tt = map showBody' cell
    | otherwise = []
  showBody' [] = "ε"
  showBody' x = T.unpack $ showBody x

buildTable :: RulesMap -> Table
buildTable r = M.fromListWith (<>) $ foldMap (uncurry oneRule) rules
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