# Chapter 3: The Scope Chain
The connections between scopes that are nested within other scopes is called the _scope chain_, which determines the path along which variables can be accessed. Lookup moves upwards/outwards only.

## Sections
* ["Lookup" Is (Mostly) Conceptual](#lookup-is-mostly-conceptual)

[◂ Return to Table of Contents](../README.md)

## "Lookup" Is (Mostly) Conceptual
The scope where the declaration of any given identifier access originates is _usually determined_ during the initial compilation process. This information will likely be stored or accessible from each variable's entry in the AST. That information is then used explicitly by the executable instructions that constitute the program's runtime.

This means that, in practical execution, the _Engine_ from [Chapter 2](../02/README.md#a-conversation-among-friends) does not actually look through scopes to locate where an identifier is declared. Avoiding this runtime lookup is a key optimization benefit of lexical scope.

However, there is a case where scope needs to be deferred to runtime: any variable that isn't declared in any lexically available scope in a given file may have been declared in the shared global scope in a different JavaScript file. Because of this case: any variable that is initially undeclared is left as unscoped during that file's compilation. These variables are left unscoped until other relevant files have been compiled and the application runtime commences. This deferred lookup will eventually resolve to whichever scope the variable is found to have been declared (likely the global scope). This looup is only needed once per variable at most - nothing else at runtime could later change that variable's scope.

[▲ Return to Sections](#sections)

| [Previous: Chapter 2 - Illustrating Lexical Scope](../02/README.md) | [Table of Contents](../README.md#table-of-contents) |
