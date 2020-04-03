{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, QuasiQuotes #-}
module ParseRecursive where

import Grammar

import Data.Maybe
import qualified Data.Set as S
import MonadTypes
import Control.Monad
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Utils

type ActionableRules = S.Set Text

makeParser :: Monad m => Text -> Lang -> FilePath -> MyMonadT m [(FilePath,Text)]
makeParser s l f = parse s >>= \(Grammar top rules) -> do
  lr <- isLeftRecursive (mkRulesMap rules)
  when lr $ throwError ["Recursive parser can not handle left-recursive grammar"]
  makeRecursiveParser l f top rules

makeRecursiveParser :: Monad m => Lang -> FilePath -> Text -> Rules -> MyMonadT m [(FilePath,Text)]
makeRecursiveParser CPP basename top rules@(Rule startNT _:|_) = do
  parsers <- mapM makeRuleParser rules
  return [(basename <> ".h", [interp|
#ifndef PARSER_H
#define PARSER_H
#include "lexer.h"
#{top}
class Parser {
  Lexer *lex;
  Token curTok;
  bool debug;
  #{indent 1 $ foldMap fst parsers}
public:
  Parser(Lexer *lex, bool debug);
  #{returnType startNT} parse();
};\n
#endif
|]) ,(basename <> ".cpp", [interp|
#include "#{basename}.h"
#include <stdexcept>
#include <iostream>
#{top}
Parser::Parser(Lexer *lex, bool debug = false):lex(lex),debug(debug){
  curTok = lex->getNextToken();
}
#{returnType startNT} Parser::parse() { return parse_#{startNT}(); }
#{foldMap snd parsers}
|])]
  where
  indent = indentLang CPP
  returnType :: Text -> Text
  returnType x | x `S.member` actionableRules = "ResultType"
               | otherwise = "void"
  actionableRules = S.fromList $ map (\(Rule k _ ) -> k)
                    $ filter (\(Rule _ as) -> all (isJust . snd) as)
                    $ NE.toList rules

  makeRuleParser :: Monad m => Rule -> MyMonadT m (Text, Text)
  makeRuleParser (Rule nt a) = do
    alts <- buildAlternatives nt a
    return (
        returnType nt <>" parse_"<>nt<>"();"
      , returnType nt <>" Parser::parse_"<>nt<>"() {" <> alts <> "}"
      )

  buildAlternatives :: Monad m => Text -> NonEmpty Alt -> MyMonadT m Text
  buildAlternatives h ((x, act) :| []) = return $ printDebug h x <> buildBody (zip [1..] x) <> buildAction act
  buildAlternatives h xs = (<> err) . T.intercalate " else " <$> mapM (buildAlt h) xs'
    where
    xs' = NE.toList xs
    err | Just _ <- lookup [] xs' = "" -- there is an else case
        | otherwise
        = "else { throw std::runtime_error(\"No alternative matched while parsing nonterminal "<>h<>":\" + to_string(curTok.type)); }"

  printDebug :: Text -> [Symbol] -> Text
  printDebug h b = "if (debug) std::cerr << \"" <> h <> " -> " <> showBody b <> "\" << std::endl;"

  buildAlt :: Monad m => Text -> Alt -> MyMonadT m Text
  buildAlt h ([], act) = return $ "{" <> printDebug h [] <> buildAction act <> "}"
  buildAlt h (Term s:b, act) = return $ "if(curTok.type == TokenType::Tok_"<>s<>"){\
    \"<> printDebug h (Term s:b) <> "\
    \auto _1 = curTok; curTok = lex->getNextToken();"
    <> buildBody (zip [2..] b) <> buildAction act
    <> "}"
  buildAlt h (b, _) = throwError . pure $ "Recursive parser can not handle body " <> h <> " -> " <> showBody b

  buildAction :: Maybe Text -> Text
  buildAction Nothing = ""
  buildAction (Just act) = act

  buildBody :: [(Word, Symbol)] -> Text
  buildBody = T.concat . map go
    where
    go (n, NonTerm s)
      | s `S.member` actionableRules = "auto " <> val n <> " = parse_" <> s <> "();"
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
-------------------------------------------------------
makeRecursiveParser Python basename top rules@(Rule startNT _:|_) = do
  parsers <- mapM makeRuleParser rules
  return [(basename <> ".py", [interp|
from lexer import TokenType
#{top}
class Parser:
    def __init__(self, lexer, debug = False):
        self.lex = lexer
        self.debug = debug
        self.curTok = self.lex.getNextToken()

    #{indent 1 $ T.intercalate "\n" $ NE.toList parsers}

    def parse(self):
        return self.parse_#{startNT}()

|])]
  where
  indent = indentLang Python
  actionableRules = S.fromList $ map (\(Rule k _ ) -> k)
                    $ filter (\(Rule _ as) -> all (isJust . snd) as)
                    $ NE.toList rules

  makeRuleParser :: Monad m => Rule -> MyMonadT m Text
  makeRuleParser (Rule nt a) = do
    alts <- buildAlternatives nt a
    return [interp|
      def parse_#{nt}(self):
          #{indent 1 alts}
      |]

  buildAlternatives :: Monad m => Text -> NonEmpty Alt -> MyMonadT m Text
  buildAlternatives h ((x, act) :| []) = return [interp|
    #{printDebug h x}
    #{buildBody (zip [1..] x)}
    #{buildAction act}
    |]
  buildAlternatives h xs = do
    alts <- mapM (buildAlt h) xs'
    return [interp|
      #{T.intercalate "\nel" alts}
      #{err}
      |]
    where
    xs' = NE.toList xs
    err :: Text
    err | Just _ <- lookup [] xs' = "" -- there is an else case
        | otherwise
        = [interp|
        else:
            raise Exception("No alternative matched while parsing nonterminal #{h}:" + str(self.curTok.type))
        |]

  printDebug :: Text -> [Symbol] -> Text
  printDebug h b = [interp|if self.debug: print("#{h} -> #{showBody b}")|]

  buildAlt :: Monad m => Text -> Alt -> MyMonadT m Text
  buildAlt h ([], act) = return [interp|
    if True:
        #{printDebug h []}
        #{buildAction act}
    |]
  buildAlt h (Term s:b, act) = return [interp|
    if self.curTok.type == TokenType.Tok_#{s}:
        #{indent 1 $ printDebug h (Term s:b)}
        _1 = self.curTok
        self.curTok = self.lex.getNextToken()
        #{indent 1 $ buildBody (zip [2..] b)}
        #{indent 1 $ buildAction act}
    |]
  buildAlt h (b, _) = throwError . pure $ "Recursive parser can not handle body " <> h <> " -> " <> showBody b

  buildAction :: Maybe Text -> Text
  buildAction Nothing = ""
  buildAction (Just act) = T.strip act

  buildBody :: [(Word, Symbol)] -> Text
  buildBody = T.intercalate "\n" . map go
    where
    go :: (Word, Symbol) -> Text
    go (n, NonTerm s)
      | s `S.member` actionableRules = [interp|#{val n} = self.parse_#{s}()|]
      | otherwise = [interp|self.parse_#{s}()|]
    go (n, Term s) = template n ("Tok_" <> s)
    go (n, TermEof) = template n "eof"
    template :: Word -> Text -> Text
    template n s = [interp|
    if self.curTok.type != TokenType.#{s}:
        raise Exception("Expected token #{s}, but got " + str(self.curTok.type))
    #{val n} = self.curTok
    self.curTok = self.lex.getNextToken()
    |]
    val = ("_" <>) . tshow
