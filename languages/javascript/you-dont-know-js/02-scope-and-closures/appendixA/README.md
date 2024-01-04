# Appendix A: Exploring Further
This appendix is optional, supporting material.

Disclaimer: The discussions contained within are more heavily influenced by the author's opinions than the main text is.

## Sections
* [Implied Scopes](#implied-scopes)
  * [Parameter Scope](#parameter-scope)
  * [Function Name Scope](#function-name-scope)
* [Anonymous vs. Named Functions](#anonymous-vs-named-functions)
  * [Explicit or Inferred Names?](#explicit-or-inferred-names)
  * [Missing Names?](#missing-names)
  * [Who Am I?](#who-am-i)
  * [Names are Descriptors](#names-are-descriptors)
  * [Arrow Functions](#arrow-functions)
  * [IIFE Variations](#iife-variations)
* [Hoisting: Functions and Variables](#hoisting-functions-and-variables)
  * [Function Hoisting](#function-hoisting)
  * [Variable Hoisting](#variable-hoisting)
* [The Case for `var`](#the-case-for-var)
  * [Don't Throw Out `var`](#dont-throw-out-var)
  * [`const`-antly Confused](#const-antly-confused)
  * [`var` and `let`](#var-and-let)
* [What's the Deal with TDZ?](#whats-the-deal-with-tdz)
  * [Where It All Started](#where-it-all-started)
  * [Who `let` the TDZ Out?](#who-let-the-tdz-out)

[◂ Return to Table of Contents](../README.md)

## Implied Scopes
Scopes are occasionally created in non-obvious places. These implied scopes do not often impact program behavior, but it is still useful to know about them:
* Parameter scope
* Function name scope

#### Parameter Scope

[Chapter 2](../02/README.md#a-conversation-among-friends) implies that function parameters are the same as locally declared variables in the function scope. This is not always true.

```javascript
// outer/global scope: RED(1)

function getStudentName(studentID) {
  // function scope: BLUE(2)

  // ..
}
```

In the above example, `studentID` is considered a _simple_ parameter, and therefore does behave as a member of the BLUE(2) function scope. This is not technically the case with non-simple parameters.

Non-simple parameters include:
* Parameters with default values
* Rest parameters (using the spread operator `...`)
* Destructured parameters

```javascript
// outer/global scope: RED(1)

function getStudentName(/*BLUE(2)*/ studentID = 0) {
  // function scope: GREEN(3)

  // ..
}
```

In this example, `studentID` is given a default value of `0`. The parameter list, in this case, essentially becomes its own scope and the function scope is nested inside of it.

Why does this matter? Consider:

```javascript
function getStudentName(studentID = maxID, maxID) {
  // ..
}
```

The code above produces a [TDZ error](../05/README.md#unitialized-variables-aka-tdz) because, assuming left-to-right operations, the default assignment `= maxID` to `studentID` requires a `maxID` variable to already exist. `maxID` is declared within the parameter scope, but it has not yet been initialized due to the order of the paramters. If the parameter list is flipped, no TDZ error occurs:

```javascript
function getStudentName(maxID,studentID = maxID) {
  // ..
}
```

It gets even trickier when the default parameter is assigned a function expression, which can then create its own closure within this implied parameter scope:

```javascript
function whatsTheDealHere(id,defaultID = () => id) {
  id = 5;
  console.log( defaultID() );
}

whatsTheDealHere(3);
// 5
```

In the above code, `defaultID`'s default function behavior holds closure over the `id` variable in the parameter, which is then reassigned to `5`.

The implied parameter scope can be revealed with shadowing thusly:

```javascript
function whatsTheDealHere(id,defaultID = () => id) {
  var id = 5;
  console.log( defaultID() );
}

whatsTheDealHere(3);
// 3
```

Using a `var` declaration to assign `id` to `5` creates a variable `id` that shadows the `id` in the parameter list. The closure of `defaultID` is over the `id` within the parameter list, not the `id` in the function scope, proving there's a scope bubble around the parameter list.

Strange, unexpected behaviors can occur with this implied parameter scope, though:

```javascript
function whatsTheDealHere(id,defaultID = () => id) {
  var id;

  console.log(`local variable 'id': ${ id }`);
  console.log(
    `parameter 'id' (closure): ${ defaultID() }`
  );

  console.log("reassigning 'id' to 5");
  id = 5;

  console.log(`local variable 'id': ${ id }`);
  console.log(
    `parameter 'id' (closure): ${ defaultID() }`
  );
}

whatsTheDealHere(3);
// local variable 'id': 3   <--- Huh!? Weird!
// parameter 'id' (closure): 3
// reassigning 'id' to 5
// local variable 'id': 5
// parameter 'id' (closure): 3
```

The first console message should reference the function-scoped `id`, which at the point of execution should be `undefined` (assuming the variable declaration `var id` auto-initialized `id` to `undefined`).

In this corner case, JavaScript does not auto-initialize `id` to `undefined`, but rather to the value of the `id` parameter (`3`). The reason for this behavior is for legacy compatibility. However, it is clear there are still 2 `id` variables in different scopes, as observed after reassigning the function-scoped `id` to `5`.

The author's advice, in order to avoid these strange behaviors:
* Never shadow parameters with local variables.
* Avoid using a default parameter that closes over any of the parameters.

#### Function Name Scope
[Chapter 3](../03/README.md#function-name-scope) asserts that a function's name is added to the function's own scope:

```javascript
var askQuestion = function ofTheTeacher(){
  // ..
};
```

While `ofTheTeacher` is not added to the enclosing scope, it is also not technically added to the scope of the function. It is, instead, added to an implied scope between the outer scope and the main inner function scope.

If `ofTheTeacher` was in the function's scope, this would result in an error:

```javascript
var askQuestion = function ofTheTeacher(){
  // why is this not a duplicate declaration error?
  let ofTheTeacher = "Confused, yet?";
};
```

The `let` declaration form [does not allow re-declaration](../05/README.md#re-declaration), however, the above example is legal shadowing (and not re-declaration) because the two `ofTheTeacher` identifiers are in different scopes.

There are hardly any cases where the scope of a function's name identifier matters, but it is good to know how these mechanisms work. To avoid issues, never shadow function name identifiers.

[▲ Return to Sections](#sections)

## Anonymous vs. Named Functions
Functions can be expressed in named or anonymous form. When contemplating naming functions, consider:
* Name inference is incomplete
* Lexical names allow self-reference
* Names are useful descriptions
* Arrow functions have no lexical names
* IIFEs also need names

#### Explicit or Inferred Names?
Every function in a program should have a purpose, and if it has a purpose there is a name for that purpose.

The author of this text believes every function in the code of a program should be named.

It is unhelpful in debugging when "anonymous" shows up in a stack trace:

```javascript
btn.addEventListener("click",function(){
  setTimeout(function(){
    ["a",42].map(function(v){
      console.log(v.toUpperCase());
    });
  },100);
});

// Uncaught TypeError: v.toUpperCase is not a function
//     at myProgram.js:4
//     at Array.map (<anonymous>)
//     at myProgram.js:3
```

When the functions are named, the stack trace is easier to grok:

```javascript
btn.addEventListener("click",function onClick(){
  setTimeout(function waitAMoment(){
    ["a",42].map(function allUpper(v){
      console.log(v.toUpperCase());
    });
  },100);
});

// Uncaught TypeError: v.toUpperCase is not a function
//     at allUpper (myProgram.js:4)
//     at Array.map (<anonymous>)
//     at waitAMoment (myProgram.js:3)
```

With function names, `allUpper` and `waitAMoment` appear in the stack trace providing useful context when debugging this error. Function names make a program more debuggable.

**NOTE**: The `<anonymous>` that still appears in the stack trace refers to the fact that the implementation of `Array.map(..)` is not present in the code, but is instead built into the JavaScript engine.

To be clear about which function declaration/assignment syntaxes result in a _named function_:

```javascript
function thisIsNamed() {
  // ..
}

ajax("some.url",function thisIsAlsoNamed(){
  // ..
});

var notNamed = function(){
  // ..
};

makeRequest({
  data: 42,
  cb /* also not a name */: function(){
    // ..
  }
});

var stillNotNamed = function butThisIs(){
  // ..
};
```

The following syntaxes from the above list result in functions with  _inferred names_, and may still cause issues:

```javascript
var notNamed = function(){
  // ..
};

var config = {
  cb: function(){
    // ..
  }
};

notNamed.name;
// notNamed

config.cb.name;
// cb
```

#### Missing Names?
Inferred names might show up in stack traces, which is better than getting "anonymous" in a stack trace, however:

```javascript
function ajax(url,cb) {
  console.log(cb.name);
}

ajax("some.url",function(){
  // ..
});
// ""
```

Anonymous function expressions passed as callbacks are incapable of receiving an inferred name (`cb.name` holds an empty string `""`). The vast majority of anonymous function expressions in a program are used as callback arguments, therefore relying on name inference is incomplete.

Inference also fails in the following cases:

```javascript
var config = {};

config.cb = function(){
  // ..
};

config.cb.name;
// ""

var [ noName ] = [ function(){} ];
noName.name
// ""
```

Any assinment of a function expression beyond a simple assignment will also fail to name inference. Unless deliberate about it, almost all _function expressions_ in a program will not be named.

Even if a function expression does receive an inferred name, it still does not count as a full _named function_.

#### Who Am I?
Without a lexical name identifier, a function does not have the ability to self-reference. Self-reference is important for strategies like recursion or event handling:

```javascript
// broken
runOperation(function(num){
  if (num <= 1) return 1;
  return num * oopsNoNameToCall(num - 1);
});

// also broken
btn.addEventListener("click",function(){
  console.log("should only respond to one click!");
  btn.removeEventListener("click",oopsNoNameHere);
});
```

#### Names are Descriptors
Not naming a function makes the overall program more difficult for a developer to read. It will often be unclear the intended purpose of a function at a glance, having to read the code around and within the function to figure it out:

```javascript
[ 1, 2, 3, 4, 5 ].filter(function(v){
  return v % 2 == 1;
});
// [ 1, 3, 5 ]

[ 1, 2, 3, 4, 5 ].filter(function keepOnlyOdds(v){
  return v % 2 == 1;
});
// [ 1, 3, 5 ]
```

There is no reasonable argument to be made for omitting the name `keepOnlyOdds` from the `Array.filter(..)` callback. 13 chraacters are saved and readability is lost. The name `keepOnlyOdds` tells the reader very clearly what the intent of the function is.

A developer can figure out what `v % 2 == 1` is doing, but this takes mental processing time. And if there is a bug in the code, figuring out the intent becomes even more difficult. A _good_ descriptive name greatly improves the development process.

No matter the length or complexity of the function, the function's author should figure out a good descriptive name and add it to the code. Even one-liner functions should be named:

```javascript
lookupTheRecords(someData)
.then(function extractSalesRecords(resp){
  return resp.allSales;
})
.then(storeRecords);
```

The name `extractSalesRecords` tells the reader very quickly and clearly what the intent is, where `return resp.allSales` may take a bit more mental processing time.

The only excuses for not naming a function are laziness or the lack of clarity of intent for the function. If it is difficult to determine a good name for a function it is likely that the function is poorly designed, does too many things, and should be refactored/re-designed. What to name a well-designed, single-purpose function should be evident.

All functions need names. Every single one. Any unnamed function makes a program more difficult to read, debug, extend, and maintain.

#### Arrow Functions
Arrow functions are **always anonymous**, even if they're assigned in a way that gives them an inferred name, and it is the author of this text's opinion that they should never be used.

Do not use arrow functions as a general replacement for regular functions. While they may be more concise, that brevity comes at the cost of readability.

Arrow functions have a purpose, but that purpose is not the more concise syntax. The purpose of arrow functions is that they force a _lexical this_ behavior on the function. Arrow functions treat `this` like any other lexical variable: an arrow function does not define its own `this` variable, but instead the scope chain is consulted to find the function scope where the arrow function is defined and it uses the `this` from that scope.

Arrow functions are designed to avoid hacks such as `self = this` or calling `.bind(this)` on inner function expressions to force them to inherit a `this` from the outer function.

Using arrow functions means accepting the downsides of an anonymous function and accepting strange un-function-like behaviors (the inability to later invoke the function with an explicitly provided `this` via `.bind(..)`, `.call(..)`, `.apply(..)`, etc.

#### IIFE Variations
IIFEs should also have names:

```javascript
(function(){
  // don't do this!
})();

(function doThisInstead(){
  // ..
})();
```

To determine the name to provide an IIFE, identify the purpose of using an IIFE: why is a scope necessary?

```javascript
var getStudents = (function StoreStudentRecords(){
  var studentRecords = [];

  return function getStudents() {
  // ..
  }
})();
```

In the above example the IIFE is named `StoreStudentRecords` because that is what it is doing: creating a scope around a private variable `studentRecords`.

Wrapping a function declaration in parentheses `( .. )` is so that the `function` keyword is not in a position to qualify as a function declaration to the JavaScript engine. There are other ways to avoid a `function` keyword being parsed as a declaration:

```javascript
!function thisIsAnIIFE(){
  // ..
}();

+function soIsThisOne(){
  // ..
}();

~function andThisOneToo(){
  // ..
}();
```

`!`, `+`, `~`, and several other _unary operators_ (operators with a single operand) can be placed in front of the `function` keyword to turn it into an expression. This makes the final `()` function call valid, which is what makes it an IIFE.

Using `void` unary operator in front of the `function` keyword is a great way to denote a standalone IIFE:

```javascript
void function yepItsAnIIFE() {
  // ..
}();
```

`void` clearly communicates that the IIFE will not be returning any value.

[▲ Return to Sections](#sections)

## Hoisting: Functions and Variables
Chapter 5 articulates both _[function hoisting](../05/README.md#hoisting-declaration-vs-expression)_ and _[variable hoisting](../05/README.md#variable-hoisting)_. Since hoisting is often cited as a mistake in the design of JavaScript, this section articulates why both forms of hoisting can be beneficial and should be considered:
* Executable code first, function declarations last
* Semantic placement of variable declarations

#### Function Hoisting

```javascript
getStudents();

// ..

function getStudents() {
  // ..
}
```

The `function` declaration is hoisted during compilation making the identifier `getStudents` available to the entire scope. Then `getStudents` is auto-initialized with the function reference at the beginning of the scope.

This makes it possible to place code execution at the top of any given scope, with definitions placed at the bottom. It is the author's opinion this is preferable since it is easier to locate the start of code execution in any given scope, at all levels of scope:

```javascript
getStudents();

// *************

function getStudents() {
  var whatever = doSomething();

  // other stuff

  return whatever;

  // *************

  function doSomething() {
    // ..
  }
}
```

The author prefers this organization of code, finding it to be more readable.

#### Variable Hoisting
Although `let` and `const` declared variables are hoisted, [they cannot be accessed prior to declaration](../05/README.md#unitialized-variables-aka-tdz). For this reason this section will focus on `var` declared variables.

The author believes that, in almost all cases, leveraging variable hoisting is a bad idea:

```javascript
pleaseDontDoThis = "bad idea";

// much later
var pleaseDontDoThis;
```

The author believes this code is more difficult to reason about. However, for CommonJS modules, he does make a small exception.

Typically, The author will structure his module definitions in Node like the following:

```javascript
// dependencies
var aModuleINeed = require("very-helpful");
var anotherModule = require("kinda-helpful");

// public API
var publicAPI = Object.assign(module.exports,{
  getStudents,
  addStudents,
  // ..
});

// ********************************
// private implementation

var cache = { };
var otherData = [ ];

function getStudents() {
  // ..
}

function addStudents() {
  // ..
}
```

In the above example: external module imports are placed at the top of file, followed by the module export. Module-global-scoped variables like `cache` and `otherData` sit below that with the rest of the module's private definitions.

However, in cases where the private variables need to be used in the module exports assignment, such as:

```javascript
// public API
var publicAPI = Object.assign(module.exports,{
  getStudents,
  addStudents,
  refreshData: refreshData.bind(null,cache)
});
```

The author will make an exception in cases like this, assigning the variable (`cache`) above the export assignment and declaring it below:

```javascript
cache = {};   // used here, but declared below

// public API
var publicAPI = Object.assign(module.exports,{
  getStudents,
  addStudents,
  refreshData: refreshData.bind(null,cache)
});

// ********************************
// private implementation

var cache /* = {}*/;
```

This is the only case the author has found for leveraging variable hoisting to assign a variable earlier in its scope than its declaration.

[▲ Return to Sections](#sections)

## The Case for `var`
Many developers blame declarations using the `var` keyword for many woes of development. A few points:
* `var` was never broken
* `let` is useful
* `const` has limited utility
* The best of both worlds: `var` _and_ `let`

#### Don't Throw Out `var`
Claims that `var` is broken, deprecated, outdated, dangerous, or ill-designed are incorrect.

While `var` is not always the correct declarator to use for every declaration in a program, it has its uses.

`let` is great for block-scoped declarations (it is the author's opinion that implementing a TDZ was a mistake), and should be used often.

#### `const`-antly Confused
The author believes that the historical confusion around `const` outweighs the benefit that the `const` declarator provides. _Assignment_ immutability is often confused for _value_ immutability:

```javascript
const studentIDs = [ 14, 73, 112 ];

// later

studentIDs.push(6);   // whoa, wait... what!?
```

The author only ever uses `const` to assign values that are already immutable (numbers, strings, etc). It is his opinion that variable re-assignment is not a major source of bugs or confusion in code, making the usage of `const` less important - especially since `const` is block-scoped and the fact that blocks are meant to be short.

It is the author's opinion that `const` is rarely useful.

#### `var` and `let`
The author asserts that `var` and `let` are both useful in different contexts and both declarators should be used.

Kyle Simpson uses `var` in the top-level scope of any function, as well as in the global scope, when a variable is used in nested scopes. He reserves `let` for any varaible used only within a single scope. It is his assertion that this convention makes it clearer to a reader which variables are used across scope levels and which are used only within a single scope level:

```javascript
function getStudents(data) {
  var studentRecords = [];

  for (let record of data.records) {
    let id = `student-${ record.id }`;
    studentRecords.push({
      id,
      record.name
    });
  }

  return studentRecords;
}
```

`studentRecords` is declared with `var` since it is used in a lower-level scope, but `record` and `id` are only used within the narrower block-scope of the `for`-loop and therefore declared with `let` instead.

There are also a limited few circumstances where `var` will work and `let` will not. One circumstance is a loop whose conditional clause cannot see block-scoped declarations inside the iteration:

```javascript
function commitAction() {
  do {
    let result = commit();
    var done = result && result.code == 1;
  } while (!done);
}
```

`result` is only used inside the iteration block, and therefore declared with `let`. However, the `while` clause cannot access block-scoped variables from within the iteration block, so `done` is declared with `var` to function-scope it instead.

`var` declarations are also useful within unintednded blocks: where syntax requires a block, but the developer does not actually intend to create a localized scope. An example of an unintended scope is within a `try..catch` statement:

```javascript
function getStudents() {
  try {
    // not really a block scope
    var records = fromCache("students");
  }
  catch (err) {
    // oops, fall back to a default
    var records = [];
  }
  // ..
}
```

Kyle prefers that the assignment of a variable be on the same line as the declaration, so does not want to declare `records` in the parent function-scope. He uses `var` in both the `try` and the `catch` blocks despite hoisting ensuring the identifier already exists in the scope if only declared in one block: he uses this to signal to readers that `records` is always declared regardless of which path of execution the code takes. This is possible because `var` can be used on the same identifier multiple times within a single scope (to no effect), unlike `let`.

Kyle finds this ability to repeat `var` declarations to no effect useful:

```javascript
function getStudents() {
  var data = [];

  // do something with data
  // .. 50 more lines of code ..

  // purely an annotation to remind us
  var data;

  // use data again
  // ..
}
```

While the second `var data;` statement does not functionally do anything, Kyle uses it to remind readers that `data` is a function-wide declaration - saving the reader from jumping around the long `getStudents` function to find the declaration. This is something that cannot be done with `let`.

[▲ Return to Sections](#sections)

## What's the Deal with TDZ?
Some motivations of TDZ and hints to its origin:
* `const` should never change
* It's all about time
* Should `let` behave more like `const` or `var`?

#### Where It All Started
TDZ comes from the `const` declarator. During the development of ES6, TC39 had to decide if `const` (and `let`) would hoist to the top of their blocks. They ultimately decided these declarations would hoist, avoiding issues such as confusion of mid-block shadowing:

```javascript
let greeting = "Hi!";

{
  // what should print here?
  console.log(greeting);

  // .. a bunch of lines of code ..

  // now shadowing the `greeting` variable
  let greeting = "Hello, friends!";

  // ..
}
```

It would be unintuitive if the `console.log(..)` statement above printed `"Hi!"`.

As to why variables declared with `const` (and `let`) do not auto-initialize to `undefined` like variables declared with `var` do, it is to avoid situations like the following:

```javascript
{
  // what should print here?
  console.log(studentName);

  // later

  const studentName = "Frank";

  // ..
}
```

If auto-initialization of `const` occurred, the `console.log(..)` statement above would print `undefined`. However, it would be strange for a constant to hold two different values (`undefined` and `"Frank"` at different times in the program execution. It was therefore determined that any access to the variable prior to assignment would result in a TDZ error.

#### Who `let` the TDZ Out?
Why does TDZ apply to `let` in the same way it applies to `const`? TC39 made the decision in order to maintain consistency with `const` as well as leveraging it as a way to discourage developers from leveraging variable hoisting.

The author of this book disagrees with this decision and argues that `let` is much more like `var` as a declarator and should behave more like `var`. Kyle sees it as more reasonable that `const` be unique in that it is enforced to be defined at the top of a scope.

[▲ Return to Sections](#sections)

| [Previous: Chapter 8 - The Module Pattern](../08/README.md) | [Table of Contents](../README.md#table-of-contents) |
