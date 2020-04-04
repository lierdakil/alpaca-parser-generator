{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, QuasiQuotes, RecordWildCards #-}
module Parser.Recursive.Build where

import Grammar

import Data.Maybe
import qualified Data.Set as S
import MonadTypes
import Control.Monad
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Parser.Types
import qualified Control.Arrow as A

instance Parser RecursiveParser where
  --buildParser :: Monad m => Grammar -> MyMonadT m parser
  buildParser _ (Grammar top rules) = do
    lr <- isLeftRecursive (mkRulesMap rules)
    when lr $ throwError ["Recursive parser can not handle left-recursive grammar"]
    buildRecursiveParser top rules

data RecursiveParser = RecursiveParser {
    recursiveParserTop :: Text
  , recursiveParserParsers :: NonEmpty RecursiveParserItem
  }

data RecursiveParserItem = RecursiveParserItem{
    recursiveParserItemHead :: Text
  , recursiveParserItemDoesReturn :: RecursiveParserItemDoesReturn
  , recursiveParserItemAlternatives :: RecursiveParserItemAlternatives
  }
data RecursiveParserItemDoesReturn = DoesReturnValue | DoesNotReturnValue
data RecursiveParserItemAlternatives
                 = SingleBody Body (Maybe Text)
                 | MultiBody [(Maybe Lookahead, (Body, Maybe Text))]

data Body = Body Text [(Symbol, RecursiveParserItemDoesReturn)]
type ActionableRules = S.Set Text
type Lookahead = Symbol

buildRecursiveParser :: Monad m => Text -> Rules -> MyMonadT m RecursiveParser
buildRecursiveParser top rules = do
  parsers <- mapM makeRuleParser rules
  return RecursiveParser{
      recursiveParserTop = top
    , recursiveParserParsers = parsers
    }
  where
  doesReturn x | x `S.member` actionableRules = DoesReturnValue
               | otherwise = DoesNotReturnValue
  actionableRules = S.fromList $ map (\(Rule k _ ) -> k)
                    $ filter (\(Rule _ as) -> all (isJust . snd) as)
                    $ NE.toList rules

  makeRuleParser :: Monad m => Rule -> MyMonadT m RecursiveParserItem
  makeRuleParser (Rule nt a) =
    RecursiveParserItem nt (doesReturn nt) <$> buildAlternatives nt (NE.toList a)

  buildAlternatives :: Monad m => Text -> [Alt] -> MyMonadT m RecursiveParserItemAlternatives
  buildAlternatives h [(x, act)] = return $
    SingleBody (Body (printDebug h x) (map doesReturnSym x)) act
  buildAlternatives h xs
    = MultiBody <$> mapM (buildAlt h) xs

  doesReturnSym :: Symbol -> (Symbol, RecursiveParserItemDoesReturn)
  doesReturnSym (NonTerm nt) = (NonTerm nt, doesReturn nt)
  doesReturnSym x = (x, DoesReturnValue)

  printDebug :: Text -> [Symbol] -> Text
  printDebug h b = h <> " -> " <> showBody b

  buildAlt :: Monad m => Text -> Alt -> MyMonadT m (Maybe Lookahead, (Body, Maybe Text))
  buildAlt h ([], act) = return (Nothing, (Body (printDebug h []) [], act))
  buildAlt h (b@(NonTerm _:_), _) = throwError . pure $ "Recursive parser can not handle body alternative starting with a nonterminal " <> printDebug h b
  buildAlt h (b@(s:_), act) = return
    (Just s, (Body (printDebug h b) (map doesReturnSym b), act))