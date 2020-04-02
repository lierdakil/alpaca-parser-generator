{
{-# LANGUAGE OverloadedStrings #-}
module GrammarLex where
import Data.Text (Text, pack)
}

%wrapper "monad"

tokens :-
<0> $white+              ;
<0> [a-z_][a-zA-Z_0-9]*  { mkTok TTerminal }
<0> [A-Z_][a-zA-Z_0-9]*  { mkTok TNonTerminal }
<0> \%eof                { mkTok $ const TTermEof }
<0> \-\>                 { mkTok $ const TArrow }
<0> \:?\:?\=             { mkTok $ const TArrow }
<0> \|                   { mkTok $ const TAlternative }
<0> \;                   { mkTok $ const TSep }
<0> \{                   { begin action }

<action> .*/\}           { mkTok TAction }
<action> \}              { begin 0 }

{
data Token =
    TArrow
  | TAlternative
  | TSep
  | TTermEof
  | TTerminal Text
  | TNonTerminal Text
  | TAction Text
  | TEOF
  deriving (Eq, Show)

alexEOF = return TEOF
mkTok f = token $ \(_,_,_,s) len -> f (pack (take len s))
}
