{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ParseRecursive where

import Grammar

import Data.Maybe
import qualified Data.Set as S
import MonadTypes
import Control.Monad
import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Utils

type ActionableRules = S.Set Text

makeParser :: Monad m => Text -> FilePath -> MyMonadT m [(FilePath,Text)]
makeParser s f = parse s >>= makeRecursiveParser f

makeRecursiveParser :: Monad m => FilePath -> Rules -> MyMonadT m [(FilePath,Text)]
makeRecursiveParser basename rules@(Rule h _:|_) = do
  lr <- isLeftRecursive (mkRulesMap rules)
  when lr $ throwError ["Recursive parser can not handle left-recursive grammar"]
  parsers <- mapM (makeRuleParser actionableRules) rules
  return [(basename <> ".h", "\
\#ifndef PARSER_H\n\
\#define PARSER_H\n\
\#include \"lexer.h\"\n\
\#include \"parseResult.h\"\n\
\class Parser {\
  \Lexer *lex;\
  \Token curTok;\
  \bool debug;\
\"<> foldMap fst parsers <> "\
\public:\
  \Parser(Lexer *lex, bool debug);\
  \"<>returnType<>" parse();\
\};\n\
\#endif\n")
    ,(basename <> ".cpp", "\
\#include \""<>T.pack basename<>".h\"\n\
\#include <stdexcept>\n\
\#include <iostream>\n\
\Parser::Parser(Lexer *lex, bool debug = false):lex(lex),debug(debug){\
  \curTok = lex->getNextToken();\
\}\
\"<>returnType<>" Parser::parse() { return parse_"<>h<>"(); }\
\"<> foldMap snd parsers)]
  where
  returnType | h `S.member` actionableRules = "ResultType"
             | otherwise = "void"
  actionableRules = S.fromList $ map (\(Rule k _ ) -> k)
                    $ filter (\(Rule _ as) -> all (isJust . snd) as)
                    $ NE.toList rules

makeRuleParser :: Monad m => ActionableRules -> Rule -> MyMonadT m (Text, Text)
makeRuleParser ars (Rule h a) = do
  alts <- buildAlternatives ars h a
  return (
      returnType <>" parse_"<>h<>"();"
    , returnType <>" Parser::parse_"<>h<>"() {" <> alts <> "}"
    )
  where returnType | h `S.member` ars = "ResultType"
                   | otherwise        = "void"

buildAlternatives :: Monad m => ActionableRules -> Text -> NonEmpty Alt -> MyMonadT m Text
buildAlternatives ars h ((x, act) :| []) = return $ printDebug h x <> buildBody ars (zip [1..] x) <> buildAction act
buildAlternatives ars h xs = (<> err) . T.intercalate " else " <$> mapM (buildAlt ars h) xs'
  where
  xs' = NE.toList xs
  err | Just _ <- lookup [] xs' = "" -- there is an else case
      | otherwise
      = "else { throw std::runtime_error(\"No alternative matched while parsing nonterminal "<>h<>":\" + to_string(curTok.type)); }"

printDebug :: Text -> [Symbol] -> Text
printDebug h b = "if (debug) std::cerr << \"" <> h <> " -> " <> showBody b <> "\" << std::endl;"

buildAlt :: Monad m => ActionableRules -> Text -> Alt -> MyMonadT m Text
buildAlt _ h ([], act) = return $ "{" <> printDebug h [] <> buildAction act <> "}"
buildAlt ars h (Term s:b, act) = return $ "if(curTok.type == TokenType::Tok_"<>s<>"){\
  \"<> printDebug h (Term s:b) <> "\
  \auto _1 = curTok; curTok = lex->getNextToken();"
  <> buildBody ars (zip [2..] b) <> buildAction act
  <> "}"
buildAlt _ h (b, _) = throwError . pure $ "Recursive parser can not handle body " <> h <> " -> " <> showBody b

buildAction :: Maybe Text -> Text
buildAction Nothing = ""
buildAction (Just act) = act

buildBody :: ActionableRules -> [(Word, Symbol)] -> Text
buildBody ars = T.concat . map go
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
  val = ("_" <>) . tshow
