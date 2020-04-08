{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}
module Parser.Recursive.Build (
    RecursiveParser(..)
  , RecursiveParserItem(..)
  , RecursiveParserItemAlternatives(..)
  , RecursiveParserItemDoesReturn(..)
  , Body(..)
  ) where

import Grammar

import Data.Maybe
import qualified Data.Set as S
import MonadTypes
import Control.Monad
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Parser.Types

instance Parser RecursiveParser where
  --buildParser :: Monad m => Grammar -> MyMonadT m parser
  buildParser _ ParserOptions{..} = do
    lr <- isLeftRecursive (mkRulesMap parserOptionsGrammarDefinition)
    when lr $ throwError ["Recursive parser can not handle left-recursive grammar"]
    (,) [] <$> buildRecursiveParser parserOptionsGrammarDefinition

newtype RecursiveParser = RecursiveParser {
    recursiveParserParsers :: NonEmpty RecursiveParserItem
  }

data RecursiveParserItem = RecursiveParserItem{
    recursiveParserItemHead :: Text
  , recursiveParserItemDoesReturn :: RecursiveParserItemDoesReturn
  , recursiveParserItemAlternatives :: RecursiveParserItemAlternatives
  }
data RecursiveParserItemDoesReturn = DoesReturnValue | DoesNotReturnValue
data RecursiveParserItemAlternatives
                 = SingleBody Body (Maybe Text)
                 | MultiBody [(Lookahead, (Body, Maybe Text))]

data Body = Body Text [(Symbol, RecursiveParserItemDoesReturn)]
type ActionableRules = S.Set Text
type Lookahead = S.Set (Maybe Symbol)

buildRecursiveParser :: Monad m => Rules -> MyMonadT m RecursiveParser
buildRecursiveParser rules = do
  parsers <- mapM makeRuleParser rules
  return RecursiveParser{
      recursiveParserParsers = parsers
    }
  where
  r = mkRulesMap rules
  doesReturn x | x `S.member` actionableRules = DoesReturnValue
               | otherwise = DoesNotReturnValue
  actionableRules = S.fromList $ map (\(Rule k _ ) -> k)
                    $ filter (\(Rule _ as) -> all (isJust . bwaAction) as)
                    $ NE.toList rules

  makeRuleParser :: Monad m => Rule -> MyMonadT m RecursiveParserItem
  makeRuleParser (Rule nt a) =
    RecursiveParserItem nt (doesReturn nt) <$> buildAlternatives nt (NE.toList a)

  buildAlternatives :: Monad m => Text -> [Alt] -> MyMonadT m RecursiveParserItemAlternatives
  buildAlternatives h [BodyWithAction{..}] = return $
    SingleBody (Body (printDebug h bwaBody) (map doesReturnSym bwaBody)) bwaAction
  buildAlternatives h xs
    = MultiBody <$> mapM (buildAlt h) xs

  doesReturnSym :: Symbol -> (Symbol, RecursiveParserItemDoesReturn)
  doesReturnSym (NonTerm nt) = (NonTerm nt, doesReturn nt)
  doesReturnSym x = (x, DoesReturnValue)

  printDebug :: Text -> [Symbol] -> Text
  printDebug h b = h <> " -> " <> showBody b

  buildAlt :: Monad m => Text -> Alt -> MyMonadT m (Lookahead, (Body, Maybe Text))
  buildAlt h (BodyWithAction _ b act)
    = return (first r b, (Body (printDebug h b) (map doesReturnSym b), act))
