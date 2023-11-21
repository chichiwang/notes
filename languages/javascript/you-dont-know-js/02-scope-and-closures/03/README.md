# Chapter 3: The Scope Chain
The connections between scopes that are nested within other scopes is called the _scope chain_, which determines the path along which variables can be accessed. Lookup moves upwards/outwards only.

## Sections
* ["Lookup" Is (Mostly) Conceptual](#lookup-is-mostly-conceptual)
* [Shadowing](#shadowing)

[◂ Return to Table of Contents](../README.md)

## "Lookup" Is (Mostly) Conceptual
The scope where the declaration of any given identifier access originates is _usually determined_ during the initial compilation process. This information will likely be stored or accessible from each variable's entry in the AST. That information is then used explicitly by the executable instructions that constitute the program's runtime.

This means that, in practical execution, the _Engine_ from [Chapter 2](../02/README.md#a-conversation-among-friends) does not actually look through scopes to locate where an identifier is declared. Avoiding this runtime lookup is a key optimization benefit of lexical scope.

However, there is a case where scope needs to be deferred to runtime: any variable that isn't declared in any lexically available scope in a given file may have been declared in the shared global scope in a different JavaScript file. Because of this case: any variable that is initially undeclared is left as unscoped during that file's compilation. These variables are left unscoped until other relevant files have been compiled and the application runtime commences. This deferred lookup will eventually resolve to whichever scope the variable is found to have been declared (likely the global scope). This looup is only needed once per variable at most - nothing else at runtime could later change that variable's scope.

[▲ Return to Sections](#sections)

## Shadowing
A program where all the variable names used are unique does not necessarily benefit from having multiple scopes. Every variable could conceivably share the same global scope bucket without conflict.

A program containing multiple variables, each in different scopes, with the same lexical names benefits greatly from lexical scope. A single scope cannot contain two or more variables with the same name - multiple references would be assumed as just one variable.

Consider:

```javascript
var studentName = "Suzy";

function printStudent(studentName) {
  studentName = studentName.toUpperCase();
  console.log(studentName);
}

printStudent("Frank");
// FRANK

printStudent(studentName);
// SUZY

console.log(studentName);
// Suzy
```

The `studentName` variable on line one occupies the global scope bucket. The `studentName` defined as parameter on line 3 belongs to the `printStudent(..)` function bucket.

The references to `studentName` on lines 4 and 5, within the `printStudent(..)` function all refer to the `studentName` defined in the parameter on line 3, scoped to the `printStudent(..)` function bucket.

This is a key aspect of lexical scope behavior known as _shadowing_. The `printStudent(..)`-scoped `studentName` variable _shadows_ the global-scoped `stuentName` defined on line 1. The parameter `studentName` is _shadowing_ the (_shadowed_) global variable `studentName`.

One side-effect of shadowing a variable from an outer scope is that it becomes lexically impossible to reference that shadowed variable from the outer scope.

[▲ Return to Sections](#sections)

| [Previous: Chapter 2 - Illustrating Lexical Scope](../02/README.md) | [Table of Contents](../README.md#table-of-contents) |
