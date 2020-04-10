{
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
module Grammar.Lex where
import Data.Text (Text, pack)
import qualified Data.Text as T
}

%wrapper "monadUserState"

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
<0> \{                   { beginBraces }
<0> ^\%top               { mkTok $ const TTop }
<0> ^\%inherit           { mkTok $ const TInherit }
<0> \%left[0-9]+         { mkTok $ \s -> TLeft (read $ T.unpack (T.drop 5 s)) }
<0> \%right[0-9]+        { mkTok $ \s -> TRight (read $ T.unpack (T.drop 6 s)) }
<0> \%nonassoc[0-9]+     { mkTok $ \s -> TNonAssoc (read $ T.unpack (T.drop 9 s)) }

<braces> [^\{\}]+        { saveBracesString }
<braces> \n+             { saveBracesString }
<braces> \{              { beginBraces }
<braces> \}              { endBraces 0 }

{
data Token =
    TArrow
  | TAlternative
  | TSep
  | TTermEof
  | TTerminal Text
  | TNonTerminal Text
  | TBraces Text
  | TTop
  | TInherit
  | TEOF
  | TLeft Word
  | TRight Word
  | TNonAssoc Word
  deriving (Eq, Show)

alexEOF = return TEOF
mkTok f = token $ \(_,_,_,s) len -> f (pack (take len s))

data AlexUserState = AlexUserState {
    ausBraces :: Word8
  , ausBString :: Text
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState 0 ""

saveBracesString (_,_,_,input__) len = do
    us <- alexGetUserState
    alexSetUserState us{ausBString = ausBString us <> T.pack (take len input__)}
    alexMonadScan

beginBraces (_,_,_,input__) len = do
  sc <- alexGetStartCode
  if sc == braces
  then do
    us <- alexGetUserState
    alexSetUserState us{ ausBraces = ausBraces us + 1
                       , ausBString = ausBString us <> T.pack (take len input__)
                       }
  else alexSetStartCode braces
  alexMonadScan

endBraces code (_,_,_,input__) len = do
  us <- alexGetUserState
  if ausBraces us == 0
  then do
    alexSetStartCode code
    alexSetUserState us{ausBString = ""}
    return $ TBraces (ausBString us)
  else do
    alexSetUserState us{
          ausBraces = ausBraces us - 1
        , ausBString = ausBString us <> T.pack (take len input__)
        }
    alexMonadScan
}
