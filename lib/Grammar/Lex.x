{
{-# LANGUAGE OverloadedStrings #-}
module Grammar.Lex where
import Data.Text (Text, pack)
import qualified Data.Text as T
}

%wrapper "monad"

tokens :-
<0> $white+              ;
<0> [a-z_][a-zA-Z_0-9]*  { mkTok TTerminal }
<0> [A-Z_][a-zA-Z_0-9]*  { mkTok TNonTerminal }
<0> \%eof                { mkTok $ const TTermEof }
<0> \-\>                 { mkTok $ const TArrow }
<0> \:?\:?\=             { mkTok $ const TArrow }
<0> \:                   { mkTok $ const TArrow }
<0> \|                   { mkTok $ const TAlternative }
<0> \;                   { mkTok $ const TSep }
<0> \{                   { begin action }
<0> ^\%top\n             { begin top }
<0> \%left[0-9]+         { mkTok $ \s -> TLeft (read $ T.unpack (T.drop 5 s)) }
<0> \%right[0-9]+        { mkTok $ \s -> TRight (read $ T.unpack (T.drop 6 s)) }
<0> \%nonassoc[0-9]+     { mkTok $ \s -> TNonAssoc (read $ T.unpack (T.drop 9 s)) }

<action> .*/\}           { mkTok TAction }
<action> \}              { begin 0 }

<top> ^\%end\n           { begin 0 }
<top> .*\n               { mkTok TTop }

{
data Token =
    TArrow
  | TAlternative
  | TSep
  | TTermEof
  | TTerminal Text
  | TNonTerminal Text
  | TAction Text
  | TTop Text
  | TEOF
  | TLeft Word
  | TRight Word
  | TNonAssoc Word
  deriving (Eq, Show)

alexEOF = return TEOF
mkTok f = token $ \(_,_,_,s) len -> f (pack (take len s))
}
