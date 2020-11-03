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
