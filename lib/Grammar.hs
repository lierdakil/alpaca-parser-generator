module Grammar (Symbol(..), Rule(..), Alt, parse) where

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
