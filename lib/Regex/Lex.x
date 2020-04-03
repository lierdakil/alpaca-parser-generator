{
module Regex.Lex where
import Data.Text (Text)
import qualified Data.Text as T
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
<regex>  \\. { mkTok $ \s -> TChar $ T.index s 1 }
<regex>  \/$white*  { begin action }
<regex>  .   { mkTok $ \s -> TChar $ T.head s }

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
  | TName Text
  | TAction Text
  deriving (Eq, Show)

alexEOF = return TEOF
mkTok f = token $ \(_,_,_,s) len -> f (T.pack $ take len s)
}
