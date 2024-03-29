## 0.3.0.3

-   Try to fix negative character range bugs (it was pretty broken it turns out)

## 0.3.0.2

-   Fix CR handling in input

## 0.3.0.1

-   Throw meaningful errors on unknown non-terminals.

## 0.3.0.0

-   Fix static Linux build
-   Use iterator\/generator lexer formulation in C#, Python, JS
-   Move language implementations into separate namespaces
-   Escape non-printable characters in lexer
-   Use utf8 for file I\/O explicitly

## 0.2.1.1

-   Bump version
-   \[Fix\] Replace · with an ascii .
-   \[Example\]\[CPP\] Update jack-analyzer
-   \[Example\]\[CPP\] Update polish-notation-calc cpp example

## 0.2.1.0

-   \[CPP\]\[LL1\/LR\] Fix parse return type when start rule type specified
-   \[CPP\]\[LL1\] Fix debug default value

## 0.2.0.3

-   Fix a minor bug in DFA construction

    1-character ranges weren't handled correctly.

-   Add C++ jack-analyzer example

## 0.2.0.2

-   [Examples] Rework jack-analyzer to be LL(1)
-   [Fix] Fix LL1 parser issues for grammars with ambiguity

## 0.2.0.1

-   [Parsers][C++] cast to rvalue reference

## 0.2.0.0

-   Add jack-analyzer example
-   Optional explicit typing. Comes with a syntax change, hence the big version bump
-   Non-greedy patterns
-   Negative character groups
-   Allow lexer character escapes and escape graphviz labels
-   Allow lexer-only syntax files
-   Prefer more specific lexer rules to less specific ones. Prefer rules defined earlier in the file to rules defined later.

## 0.1.3.1

-   [Lexer] Rework ambiguity handler, use more efficient algorithm
-   [Main] Handle CR character
-   [Lexer] Detect and resolve some ambiguities and pattern intersections
-   Add orphans to make each new language immediately buildable

## 0.1.3.0

-   [CI] Updates + static Linux build
-   Stack.yaml config
-   Add JS parsers
-   Get rid of mkToken parameter in some parsers

## 0.1.2.1

-   Bump version
-   Make lexer FA graph output optional
-   Add example
-   [LL1] Track\/use rule head in debug output
-   Fix lexer EOF debug output

## 0.1.2.0

-   Bogus
-   Fix release script
-   Rename %nonassoc to %non
-   Add README
-   Fix unmatched pattern error
-   Tweak build scripts
-   Bump version; Add version output
-   Use semantic value of terminals in C# parsers
-   Use semantic value of terminals in python parsers
-   Simplify lexer tokens in CPP
-   Clean-up API; rename package; rename executable

## 0.1.1.1

-   Bump version
-   Fix Python recursive parser error reporting

## 0.1.1.0

-   Bump version
-   Add inheritance into templates
-   Parse nested braces

## 0.1.0.0

-   First release
