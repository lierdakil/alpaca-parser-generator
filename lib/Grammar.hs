module Grammar (Symbol(..), Rule(..), Alt, parse, showBody, showSymbol) where

import GrammarLex
import GrammarParse

type Alt = ([Symbol], Maybe String)

parse :: String -> [Rule]
parse = grammar . scan

scan :: String -> [Token]
scan s = either error id $ runAlex s go
  where
  go = do
    tok <- alexMonadScan
    (tok :) <$> if tok /= TEOF
                then go
                else return []

showBody :: [Symbol] -> String
showBody = unwords . map showSymbol

showSymbol :: Symbol -> String
showSymbol TermEof = "%eof"
showSymbol (Term t) = t
showSymbol (NonTerm t) = t
