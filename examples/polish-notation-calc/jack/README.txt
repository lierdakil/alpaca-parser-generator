                Simple fractional calculator

This program implements a simple fractional calculator.

The point of the exercise is to convince oneself that
it is in principle possible to implement Jack compiler
in Jack.

Short overview of features:

-   Basic algebra using Fraction datatype
-   Supported operators: +, -, *, /, including unary -
-   Common precedence and associativity is observed:
    all operators are left-associative, +,- have lower
    precedence than *,/, unary - has highest precedence.
-   Grouping with () also works
-   User-defined variables supported via = operator.
-   Has limited support for decimal fractional numbers (e.g. 1.23)
-   Parser can support arbitrary functions, but only ABS function
    (returning the absolute value) is defined.
-   Builtin command MEM to show defined variables
-   Builtin command HELP to show greeting text

The parser design is the usual fare with separate lexer and parser
stages. Lexer uses the typical finite automata design, while parser
uses LR approach (LALR to be exact, but SLR would work too). DFA
transition graph is included as dfa.png. LR parser points and transition
table are included in parser.txt.

To produce the parser code, ALPaCA parser generator was used (my project,
available at https://github.com/lierdakil/alpaca-parser-generator).
Lexer.jack, Parser.jack and Token.jack files are generated.
Grammar definition is in the source/syntax.xy file.

All Jack sources are in the source/ directory. Compiled vm files are
in the app/ directory.

Have fun,
Nick "Lierdakil" Yakimov
