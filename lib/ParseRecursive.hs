module ParseRecursive where

import Grammar

import Data.Maybe
import Data.List
import qualified Data.Set as S

type ActionableRules = S.Set String

makeParser :: String -> String
makeParser = makeRecursiveParser . parse

makeRecursiveParser :: [Rule] -> String
makeRecursiveParser [] = error "No rules!"
makeRecursiveParser rules@(Rule h _:_)= "\
\#ifndef PARSER_H\n\
\#define PARSER_H\n\
\#include <stdexcept>\n\
\#include <iostream>\n\
\#include \"lexer.h\"\n\
\#include \"parseResult.h\"\n\
\class Parser {\
  \Lexer *lex;\
  \Token curTok;\
  \bool debug;\
\"<> parsers <> "\
\public:\
  \Parser(Lexer *lex, bool debug = false):lex(lex),debug(debug)\
  \{curTok = lex->getNextToken();}\
  \"<>returnType<>" parse() { return parse_"<>h<>"(); } \
\};\n\
\#endif\n"
  where
  returnType | h `S.member` actionableRules = "ResultType"
             | otherwise = "void"
  parsers = concatMap (makeRuleParser actionableRules) rules
  actionableRules = S.fromList $ map (\(Rule k _ ) -> k)
                    $ filter (\(Rule _ as) -> all (isJust . snd) as) rules

makeRuleParser :: ActionableRules -> Rule -> String
makeRuleParser ars (Rule h a) = "\
\"<>returnType<>" parse_"<>h<>"() {\
\" <> buildAlternatives ars h a <> "\
\}\
\"
  where returnType | h `S.member` ars = "ResultType"
                   | otherwise        = "void"

buildAlternatives :: ActionableRules -> String -> [Alt] -> String
buildAlternatives ars h [(x, act)] = printDebug h x <> buildBody ars (zip [1..] x) <> buildAction act
buildAlternatives _ _ [] = error "impossiburu"
buildAlternatives ars h xs = intercalate " else " (map (buildAlt ars h) xs) <> err
  where
  err | Just _ <- lookup [] xs = "" -- there is an else case
      | otherwise
      = "else { throw std::runtime_error(\"No alternative matched while parsing nonterminal "<>h<>":\" + to_string(curTok.type)); }"

printDebug :: String -> [Symbol] -> String
printDebug h b = "if (debug) std::cerr << \"" <> h <> " -> " <> showBody b <> "\" << std::endl;"

showBody :: [Symbol] -> String
showBody = unwords . map showSymbol
  where showSymbol TermEof = "%eof"
        showSymbol (Term t) = t
        showSymbol (NonTerm t) = t

buildAlt :: ActionableRules -> String -> Alt -> String
buildAlt _ h ([], act) = "{" <> printDebug h [] <> buildAction act <> "}"
buildAlt ars h (Term s:b, act) = "if(curTok.type == TokenType::Tok_"<>s<>"){\
  \"<> printDebug h (Term s:b) <> "\
  \auto val_1 = curTok; curTok = lex->getNextToken();"
  <> buildBody ars (zip [2..] b) <> buildAction act
  <> "}"
buildAlt _ h (b, _) = error $ "Can not handle body " <> h <> " -> " <> showBody b

buildAction :: Maybe String -> String
buildAction Nothing = ""
buildAction (Just act) = act

buildBody :: ActionableRules -> [(Word, Symbol)] -> String
buildBody ars = concatMap go
  where
  go (n, NonTerm s)
    | s `S.member` ars = "auto " <> val n <> " = parse_" <> s <> "();"
    | otherwise = "parse_" <> s <> "();"
  go (n, Term s) = template n ("Tok_" <> s)
  go (n, TermEof) = template n "eof"
  template n s = "\
  \if(curTok.type != TokenType::"<>s<>")\
    \throw std::runtime_error(\"Expected token "<>s<>", but got \" + to_string(curTok.type));\
  \auto " <> val n <> " = curTok;\
  \curTok = lex->getNextToken();\
  \"
  val = ("val_" <>) . show
