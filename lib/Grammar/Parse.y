{
{-# LANGUAGE OverloadedStrings #-}
module Grammar.Parse (module Grammar.Parse, Type(..)) where

import Grammar.Lex
import Regex.Parse (Type(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import qualified Data.Text as T
}

%name grammar
%tokentype { Token }
%error { parseError }
%token
  '->' { TArrow }
  '|'  { TAlternative }
  ';'  { TSep }
  '::' { TDoubleColon }
  term { TTerminal $$ }
  teof { TTermEof }
  nont { TNonTerminal $$ }
  braces { TBraces $$ }
  eof  { TEOF }
  top  { TTop }
  inherit { TInherit }
  left { TLeft $$ }
  rght { TRight $$ }
  nona { TNonAssoc $$ }

%%

Start
  : Tops Rules eof  { Grammar $1 ($2 []) }

Tops
  : Tops top braces     { $1{topTop = topTop $1 <> $3} }
  | Tops inherit braces { $1{topInh = topInh $1 <> $3} }
  |                     { Tops "" "" }

Rules
  : Rules Rule { $1 . ($2 :) }
  | Rule       { ($1 :|) }

Rule
  : nont '->' Alternatives Type ';' { Rule $1 ($3 []) $4 }

Type
  : '::' braces { Type $ T.strip $2 }
  | { NoType }

Alternatives
  : Alternatives '|' BodyWithAction { $1 . ($3 :) }
  | BodyWithAction                  { ($1 :|) }

BodyWithAction
  : Assoc Body Action           { BodyWithAction $1 (reverse $2) $3 }

Assoc
  : left          { Just (AssocLeft, $1) }
  | rght          { Just (AssocRight, $1) }
  | nona          { Just (AssocNone, $1) }
  |               { Nothing }

Action
  : braces  { Just $1 }
  |         { Nothing }

Body
  : Body Symbol           { $2:$1 }
  |                       { [] }

Symbol
  : term                  { Term $1 }
  | nont                  { NonTerm $1 }
  | teof                  { TermEof }

{
data Grammar = Grammar Tops (NonEmpty Rule)
data Tops = Tops { topTop :: Text, topInh :: Text }
data Symbol = TermEof | Term Text | NonTerm Text deriving (Eq, Ord, Show)
data Rule = Rule {ruleName :: Text, ruleBodies :: (NonEmpty BodyWithAction), ruleType :: Type} deriving (Eq, Show)
data BodyWithAction = BodyWithAction {
    bwaAssoc :: Maybe Assoc
  , bwaBody :: Body
  , bwaAction :: Maybe Text
  } deriving (Eq, Show)
type Body = [Symbol]
type Assoc = (AssocType, Word)
data AssocType = AssocNone | AssocLeft | AssocRight
  deriving (Eq, Show, Ord)

parseError :: [Token] -> a
parseError x = error $ "Parse error at" <> show x
}
