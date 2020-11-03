{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, QuasiQuotes
  , RecordWildCards
  , MultiParamTypeClasses
  , FlexibleContexts
  #-}
module Parser.LR.CPP () where

import Parser.LR.Build
import Lang
import Data.Char

import Grammar hiding (first)
import qualified Data.Map as M
import Data.Maybe
import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as T
import Utils
import Parser.LR.Point
import Parser.Types
import Control.Arrow

instance LRPoint p => ParserWriter (LRParser p) CPP where
  --writeParser :: Proxy lang -> Text -> ParserOptions a -> parser -> [(FilePath,Text)]
  writeParser _ gtop ParserOptions{..} LRParser{..} = [
      (base <> ".h", [interp|
#ifndef #{headerName}_H
#define #{headerName}_H
#include "lexer.h"
#include <stack>
#include <any>
#{topTop gtop}
class #{name}#{topInh gtop} {
  Lexer *lex;
  bool debug;
  std::stack<std::pair<std::size_t,std::any>> stack;
  static const std::size_t Action[#{statesLenStr}][#{termLenStr}];
  static const std::size_t GOTO[#{statesLenStr}][#{nonTermLenStr}];
  std::size_t top() const;
public:
  #{name}(Lexer *lex, bool debug = false);
  std::any parse();
};
#endif
|]) , (base <> ".cpp", [interp|
#include "#{baseText}.h"
#include <stdexcept>
#include <iostream>
static const std::string stateToString(std::size_t state) {
  static constexpr const char* names[] = {#{stateToString}};
  return names[state];
}
static const std::string expectedSym(std::size_t state) {
  static constexpr const char* names[] = {#{expectedSym}};
  return names[state];
}
const std::size_t #{name}::Action[#{statesLenStr}][#{termLenStr}] = {
  #{indent 1 actionTable}
};
const std::size_t #{name}::GOTO[#{statesLenStr}][#{nonTermLenStr}] = {
  #{indent 1 gotoTable}
};
std::size_t #{name}::top() const { return stack.empty() ? 0 : stack.top().first; }
#{name}::#{name}(Lexer *lex, bool debug):lex(lex),debug(debug) {}
std::any #{name}::parse() {
  Token a = lex->getNextToken();
  while (true) {
    auto action = Action[top()][static_cast<std::size_t>(a.first)];
    switch (action) {
    #{indent 2 actionCases}
    default:
      if(debug)std::cerr<<"Shift to "<<action<<std::endl;
      stack.push({action, std::move(a.second)});
      a=lex->getNextToken();
      break;
    }
  }
}|])]
    where
    indent = indentLang 2
    base = parserOptionsBaseFileName
    name = parserOptionsName
    tokens = lrTerminals
    states = lrStates
    nonTerminals = lrNonTerminals
    statesLenStr = tshow (length states)
    termLenStr = tshow (length tokens)
    nonTermLenStr = tshow (length nonTerminals)
    headerName = T.map toUpper name
    baseText = T.pack base
    braces :: Text -> Text
    braces x = "{"<>x<>"}"
    actionTable = T.intercalate ",\n" $ map (braces . T.intercalate "," . map tshow) actionTableRaw
    gotoTable = T.intercalate ",\n" $ map (braces . T.intercalate "," . map tshow) gotoTableRaw
    (actionTableRaw, (actionsMap, _)) = runState (mapM (forM tokens . actionCell) states) (M.empty, fromIntegral $ length states)
    gotoTableRaw = map (\st -> map (\nt -> fromJust $ M.lookup (st, nt) lrGoto) nonTerminals) states
    actionCell st tok = do
      let Just act = M.lookup (st, tok) lrAction
      mi <- gets (M.lookup act . fst)
      case mi of
        Just i -> return i
        Nothing -> case act of
          Shift i -> do
            modify (first $ M.insert act i)
            return i
          _ -> do
            i <- gets snd
            modify (M.insert act i *** (+1))
            return i
    stateToString = T.intercalate "," $ "\"·\"" : map (quote . showSymbol) (M.elems lrStateSym)
    expectedSym = T.intercalate "," $ map (quote . T.intercalate "/") $ M.elems lrExpected
    actionCases = T.intercalate "\n" $ mapMaybe writeAction $ M.toList actionsMap
    writeAction (Shift _, _) = Nothing
    writeAction (a, n) = Just [interp|
    case #{tshow n}: {
        #{indent 2 $ actionBody a}
      } break;
    |] :: Maybe Text
    actionBody Reject = [interp|
      std::string parsed=stateToString(top());
      auto lastSt = top();
      while(!stack.empty()) { stack.pop(); parsed = stateToString(top()) + " " + parsed; }
      throw std::runtime_error(
        "Rejection state reached after parsing \\""+parsed+"\\", when encoutered symbol \\""
        + ::to_string(a.first) + "\\" in state "
        + std::to_string(lastSt) + ". Expected \\"" + expectedSym(lastSt) +"\\"");
      |] :: Text
    actionBody (Shift _) = error "does not happen"
    actionBody (Reduce ((ExtendedStartRule, _), _)) = [interp|
      stack.pop();
      return std::move(stack.top().second);
      |]
    actionBody (Reduce ((h, body), mcode)) = [interp|
      if(debug) std::cerr << "Reduce using #{h} -> #{showBody body}\\n";
      #{T.intercalate "\n" (reverse $ zipWith showArg body [1::Word ..])}
      auto gt = GOTO[top()][#{tshow (nonTermIdx (NonTerm h))} /*#{h}*/];
      if(gt==0) throw std::runtime_error("No goto");
      if(debug) std::cerr << top() << " is now on top of the stack;\\n"
                          << gt << " will be placed on the stack" << std::endl;
      stack.push({gt,#{result}});
      |]
      where
        result :: Text
        result
          | Just code <- mcode
          = [interp|(#{cast $ T.strip code})|]
          | otherwise
          = "std::any()"
        cast :: Text -> Text
        cast y = case M.lookup (NonTerm h) lrTypes of
          Just (Type t) -> [interp|static_cast<#{t}&&>(#{y})|]
          _ -> y
        showArg x i = case M.lookup x lrTypes of
          Just (Type t) ->
            [interp|auto _#{i} = std::any_cast<#{t}&&>(std::move(stack.top().second)); stack.pop();|]
          _ ->
            [interp|auto _#{i} = std::move(stack.top().second); stack.pop();|]
    nonTermIdx nt = fromJust $ M.lookup nt nonTerminalsMap
    nonTerminalsMap = M.fromList $ zip nonTerminals [0::Word ..]
    quote x = "\"" <> x <> "\""
