# Chapter 2: Illustrating Lexical Scope
JavaScript scope is determined during code compilation, a model called _lexical scope_. The term "lexical" refers to the first stage of compilation (lexing/parsing). This chapter will illustrate scope with several metaphors with the goal of creating more accurate mental models of how a program is handled by the JavaScript engine.

## Sections
* [Marbles, and Buckets, and Bubbles... Oh My!](#marbles-and-buckets-and-bubbles-oh-my)
* [A Conversation Among Friends](#a-conversation-among-friends)

[◂ Return to Table of Contents](../README.md)

## Marbles, and Buckets, and Bubbles... Oh My!
An effective metaphor for understanding scope is sorting colored marbles into buckets of the corresponding color. Imagine a pile of marbles that are colored red, blue, and green. Imagine sorting the marbles into buckets of the same color: red marbles in a red bucket, blue in blue, and green in green.

In this metaphor the marbles are variables in a program, and the buckets are scopes (functions and blocks).

Annotating the example program from [Chapter 1](../01/README.md#compiler-speak) with scope color labels:

```javascript
// outer/global scope: RED

var students = [
  { id: 14, name: "Kyle" },
  { id: 73, name: "Suzy" },
  { id: 112, name: "Frank" },
  { id: 6, name: "Sarah" }
];

function getStudentName(studentID) {
// function scope: BLUE

  for (let student of students) {
    // loop scope: GREEN

    if (student.id == studentID) {
      return student.name;
    }
  }
}

var nextStudent = getStudentName(73);
console.log(nextStudent);   // Suzy
```

Three scopes have been designated with colors: RED (outermost global scope), BLUE (scope of the function `getStudentName(..)`), and GREEN (scope of/inside the `for` loop).

This is easier to visualize with color coding:

![Figure 2: Colored Scope Bubbles](./fig2.png)

**Note**: Technically, the parameter `studentID` is not exactly in the BLUE scope. This is explained in greater detail in "Implied Scopes" in Appendix A. For the purposes of this metaphor it is close enough to label `studentID` as belonging to the BLUE scope.

Scope bubbles are determined at compilation according to where the functions/blocks are written and their nesting. Each scope bubble is contained entirely within its parent scope bubble. A scope is never partially in two different outer scopes.

Each variable/identifier is colored based on the scope that it is declared in, not the scope it may be accessed from.

Scopes can nest inside each other to any depth of nesting as the program dictates. References to variables/identifiers are allowed if there's a matching declaration in either the current scope, or in any scope above/outside the current scope - but not for delcarations in lower/nested scopes.

**Note**: The JavaScript engine does not look up the scope of variable references at runtime. At compilation most or all variable references have their scope-access determined and stored to avoid avoid unnecessary lookups at runtime. This is covered in greater detail in Chapter 3.

Key take-aways:
* Variables are declared in specific scopes, which can be thought of as colored marbles from matching-color buckets.
* Any variable reference that appears either in the scope it was declared, or a deeper-nested scope, will be labeled as a marble of the same color as the scope.
  * The exception is if an intervening scope _shadows_ the variable declaration (_shadowing_ covered in Chapter 3).
* The determination of colored buckets, and the marbles in them, happens during compilation. This information is used for varaible lookups during execution.

[▲ Return to Sections](#sections)

## A Conversation Among Friends
Another useful metaphor for the process of analyzing variables and their scopes is to imagine various conversations that occur within the JavaScript engine during code processing and execution.

Members of the JavaScript engine that will converse as they process a program:
* _Engine_: responsible for the start-to-finish compilation and execution of a program.
* _Compiler_: responsilbe for parsing code and code-generation.
* _Scope Manager_: collects and maintains a lookup list of all declared variables/identifiers, and enforces a set of rules as to how these are accessible to currently executing code.

Using the scope-example program:

```javascript
var students = [
  { id: 14, name: "Kyle" },
  { id: 73, name: "Suzy" },
  { id: 112, name: "Frank" },
  { id: 6, name: "Sarah" }
];

function getStudentName(studentID) {
  for (let student of students) {
    if (student.id == studentID) {
      return student.name;
    }
  }
}

var nextStudent = getStudentName(73);

console.log(nextStudent);
// Suzy
```

Starting with the first statement: the array and its contents are JavaScript value literals and thus unaffected by scoping concerns. The assignment declaration `var students = [ .. ]` will be the focus of the statement.

The _Engine_ sees this statement as two distinct operations, one which the _Compiler_ will handle during compilation, the other the _Engine_ will handle during execution.

First the _Compiler_ will perform lexing on the program to covert it into tokens, then parse it into an AST. After generating the AST, the _Compiler_ will then convert it into byte-code:
1. Upon encountering the `var students` expression the _Compiler_ checks the _Scope Manager_ for an existing variable `students` in that particular scope bucket.
  * If `students` already exists in the current scope bucket, the _Compiler_ moves on.
  * If `students` does not exist in the current scope bucket, the _Compiler_ generates code that will (at execution) ask the _Scope Manager_ to create a variable `students` in that scope bucket.
2. _Compiler_ generates code for the _Engine_ to handle at execution to handle the `students = []` assignment. The code will first check the _Scope Manager_ if there is a variable `students` accessible in the current scope.
  * If `students` does not exist in the current scope, the _Engine_ will continue checking other scopes (see [Nested Scope](#nested-scope)).
  * Once _Engine_ finds the variable `students` it assigns the reference of the `[ .. ]` array literal to it.

In conversational form, the compilation of the program may look something like:

> **Compiler**: Hey, (Global) Scope Manager, I found a formal declaration for an identifier called `students`. Have you heard of it?

> **(Global) Scope Manager**: Nope, never heard of it - so I just created it for you.

> **Compiler**: Hey, (Global) Scope Manager, I found a formal declaration for an identifier called `getStudentName`. Have you heard of it?

> **(Global) Scope Manager**: Nope - I just created it for you.

> **Compiler**: Hey, (Global) Scope Manager, `getStudentName` points to a function. We need a new scope bucket.

> **(Function) Scope Manager**: Here's the scope bucket.

> **Compiler**: Hey, (Function) Scope Manager, I found a formal declaration for an identifier called `studentID`. Have you heard of it?

> **(Function) Scope manager**: Nope - I just created it for you in this scope.

> **Compiler**: Hey, (Function) Scope Manager, I found a `for`-loop that will require its own scope bucket.

> ...

In compilation the _Compiler_ will ask if a identifier declaration has already been encountered: if not, the _Scope Manager_ will create it. The _Compiler_ also signals when it encounters functions or block scopes which will prompt the _Scope Manager_ to create a new scope bucket.

During execution, the conversation is between the _Engine_ and the _Scope Manager_:

> **Engine**: Hey, (Global) Scope Manager, can you look up the identifier `getStudentName` so I can assign this function to it?

> **(Global) Scope Manager**: Here it is.

> **Engine**: Hey, (Global) Scope Manager, I found a target reference for `students`. Have you heard of it?

> **(Global) Scope Manager**: Yes, it was formally declared in this scope. Here it is.

> **Engine**: Thanks, I am initializing `students` to `undefined`, so it's ready to use.

> Hey, (Global) Scope Manager, I found a target reference for `nextStudent`. Have you heard of it?

> **(Global) Scope Manager**: Yes, it was formally declared in this scope. Here it is.

> **Engine**: Thanks, I am initializing `nextStudent` to `undefined`, so it's ready to use.

> Hey, (Global) Scope Manager, I found a target reference for `getStudentName`. Have you heard of it?

> **(Global) Scope Manager**: Yes, it was formally declared in this scope. Here it is.

> **Engine**: Thanks. The value of `getStudentName` is a function so I am going to execute it.

> **Engine**: Hey, (Global) Scope Manager, we need to instantiate the function `getStudentName`'s scope.

> ...

To summarize how the statement `var students = [ .. ]` is processed:
1. _Compiler_ sets up the declaration of the scope variable (it wasn't previously declared in the current scope).
2. During execution, _Engine_ asks _Scope Manager_ to look up the variable, initializes it to `undefined`, then assigns the array value to it.

[▲ Return to Sections](#sections)

| [Previous: Chapter 1 - What's The Scope?](../01/README.md) | [Table of Contents](../README.md#table-of-contents) |
