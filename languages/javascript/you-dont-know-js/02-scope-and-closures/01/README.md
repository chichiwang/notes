# Chapter 1: What's the Scope?
Creating and working with variables is one of the most foundational actions programmers perform. A set of well-defined rules called _scope_ determine how the JavaScript engine knows which variables are accessible by any given statement and how to handle two variables of the same name.

## Sections
* [About This Book](#about-this-book)
* [Compiled vs. Interpreted](#compiled-vs-interpreted)
* [Compiling Code](#compiling-code)
  * [Required: Two Phases](#required-two-phases)
* [Compiler Speak](#compiler-speak)
  * [Targets](#targets)
  * [Sources](#sources)
* [Cheating: Runtime Scope Modifications](#cheating-runtime-scope-modifications)
* [Lexical Scope](#lexical-scope)

[◂ Return to Table of Contents](../README.md)

## About This Book
The focus of this book will be on the first of the three pillars of JavaScript: [the scope system and its function closures](../../01-get-started/04/README.md#pillar-1-scope-and-closure). It will also dive into the power of the module design pattern.

Prior to execution, during parse/compile, the JavaScript engine analyzes the code's placement of variables, functions, and blocks according to the rules of scope. The resulting scope structure is generally unaffected by runtime conditions.

JavaScript functions are _first-class values_, and are assigned and passed around like any other value. Regardless of where a function is eventually executed, it retains its original scope. This is called _closure_.

_Modules_ are a code organization pattern characterized by public methods that have privileged access (via closure) to private variables and functions.

[▲ Return to Sections](#sections)

## Compiled vs. Interpreted
_Code compilation_ is a set of steps that process the text of the code and turn it into a list of instructions that the computer can understand. Typically the entire source code is processed at once and the resulting instructions are saved as output (usually a file) that can later be executed.

_Code interpretation_ is similar to compilation in that it transforms code into machine-executable instructions, but the processing model is different. Interpreted source code is transformed line-by-line (rather than all at once) and each line or statement is executed before immediately proceeding to processing the next line of the source code.

Generally the two models are mutually exclusive, however it can be more nuanced than that in reality: interpretation can take on other forms besides simply operating line-by-line on the source code. Modern JavaScript engines employ numerous variations of both compilation and interpretation.

[▲ Return to Sections](#sections)

## Compiling Code
Scope is primarily determined during compilation. Understanding how compilation and execution relate is key in mastering scope.

In classic compiler theory, a program is processed by a compiler in three basic stages:
1. **Tokenizing/Lexing**: Breaking up a string of characters into meaningful chunks (according to the language), called _tokens_. For example the statement: `var a = 2;` may be broken up into the tokens `var`, `a`, `=`, `2`, and `;`. Whitespace may or may not be persisted as a token depending on the language.

  **Note**: The difference between _tokenizing_ and _lexing_ is subtle and academic: it has to do with whether the tokens are identified in a stateful or stateless manner. If the tokenizer invokes stateful parsing rules to determine if `a` should be considered a distinct token or part of another token, then that would be _lexing_.

2. **Parsing**: taking a stream (array) of tokens and turning it into a tree of nested elements which represent the grammatical structure of a program, called an _Abstract Syntax Tree_ (AST).

  Example: The statement `var a = 2;` might be converted into a tree like:

  ```
  {
    VariableDeclaration: {
      Identifier: "a",
      AssigmentExpression: {
        NumericLiteral: 2,
      },
    },
  }
  ```

3. **Code Generation**: taking an AST and turning it into executable code. This process varies greatly depending on language, the target platform, and other factors.

  The JavaScript engine the above described AST for `var a = 2;` and converts it into machine instructions to create the variable called `a` (including memory allocation) and then storing the value.

The JavaScript engine is more complex than just the above three stages: in the process of parsing and code generation there are steps to optimize execution performance (ie: collapsing redudant elements). Code can even be re-compiled and re-optimized during the progression of execution.

JavaScript engines do not have the luxury of time to perform work and optimizations because there is no build step ahead of time. It only has microseconds (or less) before the code is executed. To ensure the fastest performance under these constraints the JavaScript engine uses all kinds of tricks (such as JITs which lazy-compile and hot re-compile).

#### Required: Two Phases
The processing of JavaScript programs occurs in (at least) two phases:
1. Parsing/compilation
2. Execution

The separation of a parsing/compilation phase from the subsequent execution phase is observable fact. While the JavaScript specification does not specifically require "compilation", it does require behavior that is only practical with a compile-then-execute approach. There are three observable program characteristics that can prove this: syntax errors, early errors, and hoisting.

**Syntax Errors From The Start**

Taking this example:

```javascript
var greeting = "Hello";

console.log(greeting);

greeting = ."Hi";
// SyntaxError: unexpected token .
```

The above program produces no output: `"Hello"` is never printed. Instead it throws a `SyntaxError` due to the unexpected `.` token before the `"Hi"` string. If JavaScript was executing top-down line-by-line one would expect the syntax error to occur after the `console.log(..)`. The only way the JavaScript engine could throw a syntax error before printing the `console.log(..)` the engine would have had to parse the program prior to execution.

**Early Errors**

Taking this example:

```javascript
console.log("Howdy");

saySomething("Hello","Hi");
// Uncaught SyntaxError: Duplicate parameter name not
// allowed in this context

function saySomething(greeting,greeting) {
  "use strict";
  console.log(greeting);
}
```

Much like the previous example, here `"Howdy"` is never printed - the `SyntaxError` is thrown before program execution. This happens because strict-mode, opted into in the function `saySomething(..)`, forbids functions having duplicate parameter names (which is allowed in non-strict mode). The specification requires that safe-mode throws this error before any execution begins.

The JavaScript engine must parse the code prior to any execution to know that the duplicate parameters exist, or that the function is even in strict-mode.

**Hoisting**

Taking this example:

```javascript
function saySomething() {
  var greeting = "Hello";
  {
    greeting = "Howdy";  // error comes from here
    let greeting = "Hi";
    console.log(greeting);
  }
}

saySomething();
// ReferenceError: Cannot access 'greeting' before
// initialization
```

The `ReferenceError` occurs on the statement `greeting = "Howdy";`. The `greeting` variable for the statement is the block-scoped `let greeting="Hi";`, not the `var greeting="Hello";` from the outer scope. The JavaScript engine knows the next statement declares a block-scoped variable with the name `greeting` because it has already processed all of the code in an earlier pass and set up all of the scopes and their variable associations. The processing of scopes and declarations can only be accurately accomplished by parsing the program before execution.

The `ReferenceError` technically comes from `greeting = "Howdy";` accessing the `greeting` variable before declaration - a conflict referred to as the Temporal Dead Zone (TDZ).

These examples prove JavaScript is parsed before execution, but how does that show that compilation occurs?

A JavaScript program could parse a program, then execute it by _interpreting_ operations represented in the AST without any compilation - but it is unlikely since this would be highly inefficient. It is difficult to imagine a JavaScript engine would parse a program into AST and not converting that AST into the most efficient (binary) representation for the engine to execute.

There may be nuances and technicalities brought up in debate of this topic, but **the processing of JavaScript languages has more alike with compilation than not**.

The purpose of classifying JavaScript as a compiled language is creating a clear mental model about the phase where JavaScript code is processed and analyzed (before code execution). It is less about concerns of the distribution model (binary/byte-code executable representations). A clear mental model of how the JavaScript engine treats code is important to understanding JavaScript and scope effectively.

[▲ Return to Sections](#sections)

## Compiler Speak
Take the following as example:

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

Other than declarations, all occurrences of variables/identifiers in a program serve in one of two roles:
1. The _target_ of an assigment
2. The _source_ of a value

If a variable has a value that is being assigned to it - it is a _target_. Otherwise it is a _source_. The JavaScript engine must label each occurrence of a variable as either a target or source in order to properly handle a program's variables.

#### Targets
The statement `students = [..];` from the above example is clearly an assignment operation. The `var students` expression is handled as a declaration at compile time and is irrelevant during execution. The same goes for the statement `nextStudent = getStudentName(72)`.

There are three other _target_ assignment operations in the above program that are less obvious:

```javascript
for (let student of students) {
```

This statement assigns a value to `student` for each iteration of the loop.

```javascript
getStudentName(73)
```

The value `73` is assigned to the parameter `studentID`.

```javascript
function getStudentName(studentID) {
```

A `function` declaration is a special case of _target_ reference. The identifier `getStudentName` is declared (at compile time) and the association made to the function value (the definition) is also made at compile time. The association between `getStudentName` and the function is automatically set up at the beginning of the scope rather than when the execution reaches the lines of code. This automatic association is known as _function hoisting_.

#### Sources
All variable references in the above example program besides the ones identified in the [last section](#targets) as _targets_ must be _source_ references.

In the statement `for (let student of students)` the variable `student` was identified as a _target_ reference. The variable `students` is a _source_ reference in this case.

In the statement `if (student.id === studentID)` both `student` and `studentID` are _source_ references.

In `getStudentName(73)`, `getStudentName` is a _source_ reference. In `console.log(nextStudent)` both `console` and `nextStudent` are both source references.

**Note**: `id`, `name`, and `log` are all properties, not variable references.

[Chapter 2](../02/README.md) will cover how a variable's role impacts its lookup.

[▲ Return to Sections](#sections)

## Cheating: Runtime Scope Modifications
It should now be clear that scope is determined at compilation-time and generally not affected by runtime conditions. However, in non-strict-mode, there are two ways to cheat this rule and modify a program's scope during runtime.

While it is important to be aware of these two scenarios, it is never advisable to use them. Code written today should be run in strict-mode where these techniques are disallowed.

The `eval(..)` function receives a string of code to compile and execute during the program's runtime. If the string being evaluated contains a `var` or `function` declaration, these declarations will modify the scope that `eval(..)` is executed in:

```javascript
function badIdea() {
  eval("var oops = 'Ugh!';");
  console.log(oops);
}
badIdea();   // Ugh!
```

If `eval(..)` had not been present, the `oops` variable would not exist and the statement `console.log(oops);` would throw a `ReferenceError`. `eval(..)` modifies the scope of the `badIdea()` function at runtime. This is bad for many reasons, including the performance hit of modifying the already-compiled and optimized scope every time `badIdea()` is invoked.

The second cheat is the `with` keyword, which dynamically turns an object into a local scope at runtime (its properties are treated as identifiers in that new scope's block):

```javascript
var badIdea = { oops: "Ugh!" };

with (badIdea) {
  console.log(oops);   // Ugh!
}
```
`badIdea` is turned into a scope at runtime rather than compile-time, and the property `oops` becomes a variable in that scope. This is a bad idea for performance and readability reasons.

Avoid `eval(..)` (at least creating declarations within `eval(..)`) and `with`. Neither of these are avaialable in strict-mode.

[▲ Return to Sections](#sections)

## Lexical Scope
The term _lexical scope_ describes scope that is determined at compile time, as is done in JavaScript. "Lexical" refers to the "lexing" stage of compilation.

The key idea of _lexical scope_ is that its controlled by the placement of functions, blocks, and variable declarations in relation to one another. The compiler handles a variable declared within a function as it's parsing the function and associates said variable with the function's scope. A variable that is block-scoped delcared with `let` or `const` is associated with the nearest enclosing `{..}` block rather than with the enclosing function.

A reference for a variable must be resolved as coming from a scope that is lexically available to it, otherwise the variable is considered _undecleared_ which usually results in an error. If a variable referenced is not declared in the current scope, the outer scope is consulted, and so forth.

Compilation does not actually reserve any memory for scopes and variables - no program execution has begun at this point. Compilation, instead, creates a map of all the lexical scopes that the program uses at runtime - defining all of the scopes ("lexical environments") and registers all of the identifiers (variables) for each scope. While scopes are identified at compile-time, they are not created until runtime, each time a scope needs to be run.

[▲ Return to Sections](#sections)

| [Table of Contents](../README.md#table-of-contents) | [Next: Chapter 2 - Illustrating Lexical Scope](../02/README.md) |
