{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, QuasiQuotes
  , RecordWildCards
  , MultiParamTypeClasses
  , FlexibleContexts
  #-}
module Parser.LR.Langs.JS () where

import Parser.LR.Build
import Lang

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

instance LRPoint p => ParserWriter (LRParser p) JS where
  --writeParser :: Proxy lang -> Text -> ParserOptions a -> parser -> [(FilePath,Text)]
  writeParser _ gtop ParserOptions{..} LRParser{..} = [
      (base <> ".js", [interp|
'use strict'

const {TokenType, tokToStr} = require('./lexer.js')

#{topTop gtop}

function stateToString(state) {
  return [ #{stateToString} ][state]
}

function expectedSym(state) {
  return [ #{expectedSym} ][state]
}

const Action = [
  #{indent 1 actionTable}
  ]
const GOTO = [
  #{indent 1 gotoTable}
  ]

class #{name}#{topInh gtop} {
  constructor(debug=false) {
    this.debug = debug
  }

  parse(tokens) {
    const stack = []
    function top() {
      if (stack.length > 0) return stack[stack.length-1][0]
      else return 0
    }
    let a = tokens.next().value
    while(true) {
      const action = Action[top()][a[0]]
      switch(action) {
      #{indent 3 $ T.intercalate "\n" actionCases}
      default:
        if (this.debug) console.log(`Shift to ${action}`)
        stack.push([action, a[1]])
        a=tokens.next().value
      }
    }
  }
}

module.exports = {#{name}}
|])]
    where
    base = parserOptionsBaseFileName
    name = parserOptionsName
    tokens = lrTerminals
    states = lrStates
    nonTerminals = lrNonTerminals
    braces :: Text -> Text
    braces x = "["<>x<>"]"
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
    stateToString = T.intercalate "," $ "\".\"" : map (quote . showSymbol) (M.elems lrStateSym)
    expectedSym = T.intercalate "," $ map (quote . T.intercalate "/") $ M.elems lrExpected
    actionCases = mapMaybe writeAction $ M.toList actionsMap
    writeAction (Shift _, _) = Nothing
    writeAction (a, n) = Just [interp|
    case #{tshow n}: {
      #{indent 1 $ actionBody a}
      break
    }|] :: Maybe Text
    actionBody Reject = [interp|
      const lastSt = top()
      const parsed = [stateToString(lastSt)]
      while (stack.length > 0) {
        stack.pop()
        parsed.unshift(stateToString(top()))
      }
      throw new Error(
        `Rejection state reached after parsing "${parsed.join(' ')}", when encoutered symbol "${tokToStr(a[0])}" in state ${lastSt}. Expected "${expectedSym(lastSt)}"`)
      |] :: Text
    actionBody (Shift _) = error "does not happen"
    actionBody (Reduce ((ExtendedStartRule, _), _)) = [interp|
      stack.pop()
      return stack.pop()[1]
      |]
    actionBody (Reduce ((h, body), mcode)) = [interp|
      if (this.debug) console.log("Reduce using #{h} -> #{showBody body}")
      #{T.intercalate "\n" (reverse $ zipWith showArg body [1::Word ..])}
      const gt = GOTO[top()][#{tshow (nonTermIdx (NonTerm h))}] // #{h}
      if (gt===0) throw new Exception("No goto")
      if (this.debug) {
        console.log(`${top()} is now on top of the stack`)
        console.log(`${gt} will be placed on the stack`)
      }
      stack.push([gt,(#{result})])
      |]
      where
        result :: Text
        result
          | Just code <- mcode
          = T.strip code
          | otherwise
          = "null"
        showArg _ i = [interp|const _#{i} = stack.pop()[1]|]
    nonTermIdx nt = fromJust $ M.lookup nt nonTerminalsMap
    nonTerminalsMap = M.fromList $ zip nonTerminals [0::Word ..]
    quote x = "\"" <> x <> "\""
    indent = indentLang 2
