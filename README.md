# tinyHaskell
A Haskell-to-C compiler for a subset of Haskell, fully written in Haskell.

The main objective of this project is to force myself to learn about or unlock many "mystery boxes" with one stone. 
These "mystery boxers" mainly are:
1. Haskell: Doing a big project in Haskell should make me better at writing Haskell code (and, maybe not immediately obvious, in other imperative languages as well) and see the differences between it and other languages from the other paradigms (like Java and C++).
2. Programming Languages: Understanding how programming languages work by implementing one myself—maybe even a Turing-complete one. Implementing a functional one adds more fun to it because implementing a functional language is rarely explained in the literature. Therefore, I have to read papers :)
3. Lexers.
4. Parsers.
5. Type-checker.
6. Code Generator.

The repo also has Obsidian-flavoured markdown files thoroughly laying down all the details (with code snippets) about how each stage of the compiler is going to be implemented.

### What subset of Haskell?
- Algebraic Data Types
- Multiple function heads with multiple patterns
- Arithmetic operations
- Let expressions
- Where expressions
- Type Synonyms
- Arithmetic operations
- Strings
- Conditional expressions
- If then else expression
- Case expressions

### TODO
1. [x] CLI Abstraction
2. [x] Lexer.
3. [x] Parser.
4. [ ] Type-checker.
5. [ ] STG Code Generator.
6. [ ] C-code Generator. 
