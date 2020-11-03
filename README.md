# ALPACA - Anemic Lexer and PArser Creation Algorithm

ALPACA generates mostly human-readable, if somewhat inefficient, simple
parsers in multiple target languages.

The primary motivation behind ALPACA is to use it as a teaching tool, as a simplified example of other parser generators, which are generally much more powerful, but at the cost of complexity. For instance, YACC derivatives like Bison, are generally hard to setup, and wiring those together with a lexer can also be a bit of a challenge. ANTLR4 is simpler, but due to it using LL(*) parsing rather than more common LR(1) derivatives, it can be a little alien. Haskell's Alex and Happy are much better in this regard, but few students learning compiler design are proficient with Haskell.

Supported target languages:

- C++ (C++17 or newer)
- C# (tested with C# 7)
- Python (tested with python 3.6 and 3.7)
- JavaScript

Supported parser algorithms:

- Recursive descent with 1-character preview (no backtracking)
- LL(1)
- LR(0)
- LR(1)
- SLR
- LALR

```
Usage: alpaca [-l|--lang cpp|c++|c#|csharp|cs|python|py|js|javascript]
              [-p|--parser recursive|rec|ll1|lr0|lr1|slr|lalr] [--debug-lexer]
              [-n|--name NAME] [-b|--basename FILENAME] GRAMMARFILE
              [-v|--version]

Available options:
  -l,--lang cpp|c++|c#|csharp|cs|python|py|js|javascript
                           Target language, default cpp
  -p,--parser recursive|rec|ll1|lr0|lr1|slr|lalr
                           Parser method, default lalr
  --debug-lexer            Output lexer finite automata graphs in GraphViz
                           format
  -n,--name NAME           Parser class name, default "Parser"
  -b,--basename FILENAME   Parser output file base name, default "parser"
  GRAMMARFILE              Grammar input file
  -h,--help                Show this help text
  -v,--version             Show version
```

## Grammar definition syntax

Grammar definition files are text files containing both lexical and grammatical rules.

The file starts with lexical rules. Lexical rules can have two forms:
```
<name> /<regex>/ [{<semantic value>}] [:: <type>]
```
or
```
/<regex>/
```

The first form contains token name, which can be referenced in grammar, and token pattern defined by a regular expression (between slashes), and token semantic value, which is an expression in the target language (optional).

If the first character of `<regex>` is `?`, then the pattern is interpreted as non-greedy, consequently lexer will return the _shortest_ match instead of the longest one (which is the default).

Token names _must_ begin with a lowercase ASCII letter or the underscore character `_`, and contain only ASCII letters, digits and the underscore character.

Semantic value, which is enclosed in braces, is an expression in the target language. It can reference variable `text`, which contains the string matched by token pattern.

Optionally, type of the semantic value can be specified after `::`, which is a type expression in the target language. Currently this is only used for C++, for other languages it is ignored.

If semantic value isn't specified, then token value is either empty or nonexistent (i.e. empty string, `null`, `None`, etc)

The second form contains only the token pattern. These patterns lexer parse without emitting tokens, essentially skipping those. This is useful, for instance, to skip whitespace, which is delimiting, but not otherwise meaningful.

Here is a simple example of a lexer definition:

```
number /[0-9]+/ text
id /[a-z_]+/ text
add /\+/
sub /\-/
/ +/
```

Lexical rules are separated from grammar definition by a line containing only `%%`.

Grammar definition starts with optional directives `%top` and `%inherit`. Both directives must be followed by curly braces, i.e. `%top { <code> }`. Code can contain newlines.

`%top` directive will include the code verbatim at the top of the generated parser, for example:

```
%top {
from parseResult import *
}
```

`%inherit` directive will inherit the parser class from a user-defined class. It should be noted, that code passed to the inherit directive is pasted verbatim after the class name in the definition, so it has to include some additional syntax, for instance, parentheses in Python, and `: public` in C++, etc. This may be changed in a future version.

After the directives, follow the grammar rules in slightly extended Backus-Naur form:

`<head> : <body1> [{<action1>}] | <body2> [{<action2>}] | ... ;`

Equivalently, `->` or `::=` can be used instead if `:`.

`<head>` is the name of a non-terminal. The head of the first rule defined is the starting non-terminal. It must start with an uppercase ASCII letter and must contain only ASCII letters, digits and the underscore character `_`.

`<bodyN>` is one of the alternative bodies for the production rules with the head `<head>`. It must consist of terminal and non-terminal names separated by white-space. Additionally, a special token `%eof` can be used to signify end of input. Alternatives are separated by the vertical line ("pipe") character `|`. With LR-based parsers, each body can have a defined priority and associativity by starting with a `%leftN`, `%rightN` or `%nonN` directive, where `N` is an integer, corresponding to left-associative, right-associative and non-associative. Higher `N` means higher priority.

Optionally, each body can contain a semantic action, which defines the semantic value of the non-terminal a given body describes. While being optional, in the absolute majority of cases it should be defined, if the contents associated with the parsing sub-tree are needed at all (i.e. when it's a real parser, not a recognizer). Semantic actions are basically expressions in the target language, delimited by curly braces. Semantic actions can reference elements in the body by the way of variables `_1`, `_2`, etc. Such variables corresponding to terminals will contain the token semantic value (note those might generate a run-time exception if the token doesn't have a semantic value). Variables corresponding to non-terminals will contain the non-terminal semantic value, as defined by its actions.

After all alternatives are defined, before the semicolon, rule definition may optionally include `:: {<type>}`, i.e. literal `::` followed by a braced type expression in the target language. Note that the whole non-terminal is thus typed, hence all semantic actions are expected to have the same type. This is currently only used when target language is C++ and is ignored for other target languages.
