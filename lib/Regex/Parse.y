{
module Regex.Parse where

import Regex.Lex
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import qualified Data.Text as T
}

%name regex
%tokentype { Token }
%error { parseError }
%token
  '['  { TLBrace }
  ']'  { TRBrace }
  '('  { TLParen }
  ')'  { TRParen }
  '.'  { TDot }
  '*'  { TStar }
  '+'  { TPlus }
  '?'  { TQuest }
  '-'  { TDash }
  '|'  { TAlt }
  c    { TChar $$ }
  name { TName $$ }
  act  { TAction $$ }
  eof  { TEOF }

%left '|'

%%

Def
  : MbName Exp MbAct eof { RegexDef $1 $2 $3 }

MbName
  : name { Just $1 }
  |      { Nothing }

MbAct
  : act { Action $1 }
  |     { NoAction }

Exp
  : ExpSeq         { $1 }
  | Exp '|' ExpSeq { buildAlt $1 $3 }

ExpSeq
  : Single ExpSeq  { $1 $2 }
  |                { [] }

Single
  : '[' Grp ']' { (PGroup $2:) }
  | Char        { (PGroup (pure $1):) }
  | Single '*'  { (PKleene ($1 []):) }
  | Single '+'  { (PPositive ($1 []):) }
  | Single '?'  { (PMaybe ($1 []):) }
  | '(' Exp ')' { ($2 <>) }

Char
  : '.' { CAny }
  | c   { CChar $1 }

Rng
  : c '-' c { CRange $1 $3 }

CRng
  : Char { $1 }
  | Rng  { $1 }

Grp
  : CRng GrpCont  { $1 :| $2 }

GrpCont
  : CRng GrpCont { $1 : $2 }
  |              { [] }

{
data RegexDef = RegexDef (Maybe Text) RegexPattern Action
  deriving Show

data Action = NoAction | Action Text deriving (Show, Eq, Ord)

data CharPattern =
    CChar Char
  | CRange Char Char
  | CAny
  deriving (Show, Eq, Ord)

data RegexPatternSingle =
    PGroup (NonEmpty CharPattern)
  | PKleene RegexPattern
  | PPositive RegexPattern
  | PMaybe RegexPattern
  | PAlternative [RegexPattern]
  deriving Show

type RegexPattern = [RegexPatternSingle]

buildAlt :: RegexPattern -> RegexPattern -> RegexPattern
buildAlt [PAlternative x] neu = [PAlternative (neu:x)]
buildAlt x neu = [PAlternative [neu, x]]

parseError :: [Token] -> a
parseError x = error $ "Parse error at" <> show x
}
