{
module RegexLex where
}

%wrapper "monad"

tokens :-
<0> $white+              ;
<0> \/                   { begin regex  }
<0> [a-z_][a-zA-Z_0-9]*  { mkTok TName  }

<regex>  \[  { mkTok $ const TLBrace }
<regex>  \]  { mkTok $ const TRBrace }
<regex>  \(  { mkTok $ const TLParen }
<regex>  \)  { mkTok $ const TRParen }
<regex>  \.  { mkTok $ const TDot }
<regex>  \*  { mkTok $ const TStar }
<regex>  \+  { mkTok $ const TPlus }
<regex>  \?  { mkTok $ const TQuest }
<regex>  \-  { mkTok $ const TDash }
<regex>  \|  { mkTok $ const TAlt }
<regex>  \\. { mkTok $ \(_:c:_) -> TChar c }
<regex>  \/$white*  { begin action }
<regex>  .   { mkTok $ \(c:_) -> TChar c }

<action> .+        { mkTok TAction }

{
data Token =
    TLBrace
  | TRBrace
  | TLParen
  | TRParen
  | TDot
  | TStar
  | TPlus
  | TQuest
  | TDash
  | TAlt
  | TChar Char
  | TEOF
  | TName String
  | TAction String
  deriving (Eq, Show)

alexEOF = return TEOF
mkTok f = token $ \(_,_,_,s) len -> f (take len s)
}
