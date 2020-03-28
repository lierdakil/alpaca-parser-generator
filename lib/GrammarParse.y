{
module GrammarParse where

import GrammarLex
}

%name grammar
%tokentype { Token }
%error { parseError }
%token
  '->' { TArrow }
  '|'  { TAlternative }
  ';'  { TSep }
  term { TTerminal $$ }
  teof { TTermEof }
  nont { TNonTerminal $$ }
  act  { TAction $$ }
  eof  { TEOF }

%left '|'

%%

Start
  : Rules eof  { reverse $1 }

Rules
  : Rules Rule { $2:$1 }
  | Rule       { [$1] }

Rule
  : nont '->' Alternatives ';' { Rule $1 (reverse $3) }

Alternatives
  : Alternatives '|' BodyWithAction { $3:$1 }
  | BodyWithAction                  { [$1] }

BodyWithAction
  : Body Action           { (reverse $1, $2) }

Action
  : act  { Just $1 }
  |      { Nothing }

Body
  : Body Symbol           { $2:$1 }
  |                       { [] }

Symbol
  : term                  { Term $1 }
  | nont                  { NonTerm $1 }
  | teof                  { TermEof }

{
data Symbol = TermEof | Term String | NonTerm String deriving (Eq, Ord, Show)
data Rule = Rule String [([Symbol], Maybe String)] deriving (Eq, Show)

parseError :: [Token] -> a
parseError x = error $ "Parse error at" <> show x
}
