{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, QuasiQuotes
  , RecordWildCards
  , MultiParamTypeClasses
  , FlexibleContexts
  #-}
module Parser.LL.CPP () where

import Parser.Types
import Utils
import Data.Maybe
import Data.List
import Parser.LL.Build
import Data.Text (Text)
import Grammar
import qualified Data.Text as T
import qualified Data.Map as M

instance ParserWriter LLParser CPP where
  -- writeParser :: Proxy lang -> Text -> ParserOptions a -> parser -> [(FilePath,Text)]
  writeParser _ gtop ParserOptions{..} p@LLParser{..} = [
      (basename <> ".h", headerFile)
    , (basename <> ".cpp", sourceFile)
    ]
    where
    basename = parserOptionsBaseFileName
    tokens = llTerminals
    headerFile = [interp|
\#ifndef PARSER_LL_H
\#define PARSER_LL_H
\#include "lexer.h"
\#include <stack>
\#include <string>
\#include <variant>
\#include <any>
#{topTop gtop}
class #{className}#{topInh gtop} {
  enum class NonTerminal : std::size_t { #{T.intercalate ", " (map ("NT_" <>) nonTerms)} };
  using Symbol = std::variant<NonTerminal, TokenType, std::size_t>;
  static const std::string to_string(NonTerminal nt);
  Lexer *lex;
  bool debug;
  std::stack<Symbol> stack;
  std::stack<std::any> resultStack;
  static const std::size_t M[#{tshow(length nonTerms)}][#{tshow (length tokens)}];
public:
  std::any parse();
  #{className}(Lexer *lex, bool debug = false);
};
\#endif
|]
    sourceFile = [interp|
\#include "#{basename}.h"
\#include <stdexcept>
\#include <iostream>
const std::string #{className}::to_string(NonTerminal nt) {
  static constexpr const char *names[] = { #{T.intercalate "," (map quote nonTerms)} };
  return names[static_cast<std::size_t>(nt)];
}
const std::size_t #{className}::M[#{tshow(length nonTerms)}][#{tshow (length tokens)}] = {
  #{indent 1 $ T.intercalate ",\n" $ map (braces . T.intercalate "," . map showIdx') transTable}
};
std::any #{className}::parse() {
#{className}::#{className}(Lexer *lex, bool debug):lex(lex),debug(debug) {}
  stack.push(#{encodeSymbol llStartSymbol});
  Token a = lex->getNextToken();
  while (!stack.empty()) {
    std::visit(
        [&a, this](auto X) {
          using T = std::decay_t<decltype(X)>;
          if constexpr (std::is_same_v<T, TokenType>) {
            if (a.first == X) {
              resultStack.push(std::move(a.second));
              a = lex->getNextToken();
              stack.pop();
            } else {
              throw std::runtime_error(
                  "Found terminal " + ::to_string(a.first) + " but expected " +
                  ::to_string(X) + ".");
            }
          } else if constexpr (std::is_same_v<T, NonTerminal>) {
            auto trans = M[static_cast<std::size_t>(X)]
                              [static_cast<std::size_t>(a.first)];
            stack.pop();
            stack.push(trans);
            switch(trans) {
            case 0: throw std::runtime_error("No transition for "+to_string(X)+
                      ", "+::to_string(a.first));
            #{indent 6 bodies}
            }
          } else if constexpr (std::is_same_v<T, std::size_t>) {
            stack.pop();
            switch(X) {
            #{indent 6 actions}
            }
          }
        }, stack.top());
  }
  return std::move(resultStack.top());
}
|]
    indent = indentLang 2
    className = parserOptionsName
    (transTable, actionMap) = indexTable p
    nonTerms = mapMaybe getNt llNonTerminals
    getNt (NonTerm x) = Just x
    getNt _ = Nothing
    showIdx' Nothing = "0"
    showIdx' (Just x) = showIdx x
    showIdx x = tshow (x+1)
    braces :: Text -> Text
    braces x = "{"<>x<>"}"
    quote x = "\"" <> x <> "\""
    actionList = sortOn snd $ M.toList actionMap
    bodies = T.intercalate "\n" $ map (uncurry makeBody) actionList
    actions = T.intercalate "\n" $ map (uncurry makeAction) actionList
    makeBody ((NonTerm nt, b), _) n = [interp|
    case #{showIdx n}:
      if(debug) std::cerr << "#{nt} -> #{showBody b}" << std::endl;
      #{indent 1 . T.intercalate "\n" $ map pushSymbol (reverse b)}
      break;
    |] :: Text
    makeBody _ _ = error "Should never happen"
    pushSymbol s = [interp|stack.push(#{encodeSymbol s});|] :: Text
    makeAction ((h, body), mcode) n = [interp|
      case #{showIdx n}: {
        #{indent 1 $ T.intercalate "\n" (reverse $ zipWith showArg body [1::Word ..])}
        resultStack.push(#{act});
        break;
      }|] :: Text
      where
        act :: Text
        act | Just code <- mcode
            = case M.lookup h llTypes of
                Just (Type t) -> [interp|static_cast<#{t}&&>(#{code})|]
                _ -> [interp|(#{code})|]
            | otherwise
            = "std::any()"
        showArg x i =
          case M.lookup x llTypes of
            Just (Type t) -> [interp|auto _#{tshow i}=std::any_cast<#{t}&&>(std::move(resultStack.top()));|]
            _ -> [interp|auto _#{tshow i}=std::move(resultStack.top());|]
          <> "resultStack.pop();"

encodeSymbol :: Symbol -> Text
encodeSymbol (NonTerm nt) = "NonTerminal::NT_" <> nt
encodeSymbol (Term t) = "TokenType::Tok_" <> t
encodeSymbol TermEof = "TokenType::eof"
