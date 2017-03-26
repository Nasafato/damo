# damo
Compilation process, given source code:
1. Parse source code into AST
    - Involves `ast.ml`, `scanner.ml`, and `parser.ml`
2. Check it with semantic checker - turn into SAST
    - Involves `semant.ml`
3. Generate intermediate representation (IR)
    - Is this `codegen.ml`?
4. Augment that and turn into assembly code
    - Involves `damo.ml`
5. (bunch of stuff) that turns it into an executable
    - Linking everything in the `Makefile`