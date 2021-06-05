{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, QuasiQuotes
  , RecordWildCards
  , MultiParamTypeClasses
  , FlexibleContexts
  #-}
module Parser.LL.Langs.JS () where

import Parser.Types
import Utils
import Data.Maybe
import Data.List
import Parser.LL.Build
import Data.Text (Text)
import Grammar
import qualified Data.Text as T
import qualified Data.Map as M

instance ParserWriter LLParser JS where
  -- writeParser :: Proxy lang -> Text -> ParserOptions a -> parser -> [(FilePath,Text)]
  writeParser _ gtop ParserOptions{..} p@LLParser{..} = [ (basename <> ".js", sourceFile) ]
    where
    basename = parserOptionsBaseFileName
    sourceFile = [interp|
'use strict'

const {TokenType, tokToStr} = require('./lexer.js')
#{topTop gtop}
const NonTerminal = {
  #{indent 1 $ T.intercalate ",\n" nonTermDefs}
}

function nonTermToStr(nt) {
  switch(nt) {
    #{indent 2 $ T.intercalate "\n" nonTermToStr}
  }
}

const token = 0;
const nonterminal = 1;
const action = 2;

const M = [
  #{indent 1 $ T.intercalate ",\n" $ map (braces . T.intercalate "," . map showIdx') transTable}
];

class #{className}#{topInh gtop} {
  constructor(lex, debug = false) {
    this.lex = lex
    this.debug = debug
    this.stack = []
    this.resultStack = []
  }

  parse() {
    this.stack.push(#{encodeSymbol llStartSymbol})
    let a = this.lex.getNextToken()
    while (this.stack.length > 0) {
      const [T, X] = this.stack.pop();
      if (T === token) {
        if (a[0] === X) {
          this.resultStack.push(a[1])
          a = this.lex.getNextToken()
        } else {
          throw new Error(`Found terminal ${tokToStr(a[0])} but expected ${tokToStr(X)}.`)
        }
      } else if (T === nonterminal) {
        const trans = M[X][a[0]]
        this.stack.push([action, trans])
        switch(trans) {
        case 0:
          throw new Error(`No transition for ${nonTermToStr(X)}, ${tokToStr(a[0])}`)
        #{indent 4 $ T.intercalate "\n" bodies}
        }
      } else if (T === action) {
        switch (X) {
        #{indent 4 $ T.intercalate "\n" actions}
        }
      }
    }

    return this.resultStack.pop()
  }
}

module.exports = {#{className}}
|]
    indent = indentLang 2
    nonTermDefs = zipWith nonTermDef [(0::Word) ..] nonTerms
    nonTermDef n s = [interp|NT_#{s}: #{n}|]
    nonTermToStr = zipWith nonTermToStr1 [(0::Word) ..] nonTerms
    nonTermToStr1 n s = [interp|case #{n}: return "#{s}"|]
    className = parserOptionsName
    (transTable, actionMap) = indexTable p
    nonTerms = mapMaybe getNt llNonTerminals
    getNt (NonTerm x) = Just x
    getNt _ = Nothing
    showIdx' Nothing = "0"
    showIdx' (Just x) = showIdx x
    showIdx x = tshow (x+1)
    braces :: Text -> Text
    braces x = "["<>x<>"]"
    actionList = sortOn snd $ M.toList actionMap
    bodies = map (uncurry makeBody) actionList
    actions = map (uncurry makeAction) actionList
    makeBody ((NonTerm nt, b), _) n = [interp|
      case #{showIdx n}:
        if (this.debug) console.log("#{nt} -> #{showBody b}")
        #{indent 1 . T.intercalate "\n" $ map pushSymbol (reverse b)}
        break
      |] :: Text
    makeBody _ _ = error "Should never happen"
    pushSymbol s = [interp|this.stack.push(#{encodeSymbol s})|] :: Text
    makeAction ((_, body), mcode) n = [interp|
      case #{showIdx n}: {
        #{indent 1 $ T.intercalate "\n" (reverse $ zipWith showArg body [1::Word ..])}
        this.resultStack.push(#{act})
        break
      }|] :: Text
      where
        act :: Text
        act | Just code <- mcode
            = [interp|(#{code})|]
            | otherwise
            = "null"
        showArg _ i = [interp|const _#{tshow i}=this.resultStack.pop()|]

encodeSymbol :: Symbol -> Text
encodeSymbol (NonTerm nt) = [interp|[nonterminal, NonTerminal.NT_#{nt}]|]
encodeSymbol (Term t) = [interp|[token, TokenType.Tok_#{t}]|]
encodeSymbol TermEof = [interp|[token, TokenType.eof]|]
