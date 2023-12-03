# Chapter 6: Limiting Scope Exposure
This chapter looks at how and why different levels of scope (functions and blocks) should be used to organize a program's variables, specifically to reduce scope over-exposure.

## Sections
* [Least Exposure](#least-exposure)
* [Hiding in Plain (Function) Scope](#hiding-in-plain-function-scope)
  * [Invoking Function Expressions Immediately](#invoking-function-expressions-immediately)
  * [Function Boundaries](#function-boundaries)
* [Scoping With Blocks](#scoping-with-blocks)
  * [`var` and `let`](#var-and-let)
  * [Where to `let`?](#where-to-let)
  * [What's the Catch?](#whats-the-catch)
* [Function Declarations in Blocks (FiB)](#function-declarations-in-blocks-fib)

[◂ Return to Table of Contents](../README.md)

## Least Exposure
Software engineering articulates a fundamental discipline, typically applied to software security, called [The Principle of Least Privilege](https://en.wikipedia.org/wiki/Principle_of_least_privilege) (PoLP). A variation of this principle applies to this discussion, typically labeled as _Least Exposure_ (PoLE).

PoLP expresses a defensive posture to software architecture: components of a system should be designed to function with least privilege, least access, least exposure. If each piece of a system is connected with minimum-necessary capabilities, the overall system is stronger (from a security standpoint) - compromise or failure of one component has minimized impact on the rest of the system.

PoLP focuses on system-level component design, PoLE focuses on a lower level: this chapter applies it to how scopes interact with each other.

Placing all of a program's variables in a global scope, where they are exposed to all parts of the program via scope, introduces three main hazards:
* **Naming Collisions**: Common names for variables will likely be re-used throughout various parts of a program - storing them in the global scope will lead to naming collisions that can introduce bugs as different parts of the program use the same variable without awareness of each other.
* **Unexpected Behavior**: Exposing variables/functions whose usage is otherwise _private_ allows developers to use these values in ways that were never intended. This can violate expected behaviors and cause bugs. It can also allow malicious actors to circumvent designed limitations or do things never intended possible with the software.
* **Unintended Dependency**: Exposing variables/functions unnecessary invites developers to use and depend on these otherwise _private_ pieces. This creates a refactoring hazard in the future since it couples these global values to a number of difficult-to-trace components within the program.

PoLE, as applied to variable/function scoping, says to default to exposing the bare minimum necessary, keeping everything else as private as possible. Declare variables in as small and as deeply nested scopes as required.

Consider the following:

```javascript
function diff(x,y) {
  if (x > y) {
    let tmp = x;
    x = y;
    y = tmp;
  }

  return y - x;
}

diff(3,7);      // 4
diff(7,5);      // 2
```

In this simple example it doesn't seem to matter whether `tmp` belongs to the function-scope. It definitely should not be a global variable. Following the PoLE principle `tmp` should be as hidden in scope as possible so it is block-scoped to the `if`-block using a `let` declaration.

[▲ Return to Sections](#sections)

## Hiding in Plain (Function) Scope
It can be useful to hide `var`/`function`-declared variables within function scopes.

Consider a program that calculates factorials, maintaining a cache of previously calculated factorials (trading memory for speed):

```javascript
var cache = {};

function factorial(x) {
  if (x < 2) return 1;
  if (!(x in cache)) {
    cache[x] = x * factorial(x - 1);
  }
  return cache[x];
}

factorial(6);
// 720

cache;
// {
//     "2": 2,
//     "3": 6,
//     "4": 24,
//     "5": 120,
//     "6": 720
// }

factorial(7);
// 5040
```

In this example, the `cache` variable should be _private_  to `factorial(..)` since it is a mechanism for how the function works. It should not be exposed to the outer/global-scope.

In this case, a middle-scope between the global scope and `factorial(..)`'s function-scope can be defined to hide `cache`:

```javascript
// outer/global scope

function hideTheCache() {
  // "middle scope", where we hide `cache`
  var cache = {};

  return factorial;

  // **********************

  function factorial(x) {
    // inner scope
    if (x < 2) return 1;
    if (!(x in cache)) {
      cache[x] = x * factorial(x - 1);
    }
    return cache[x];
  }
}

var factorial = hideTheCache();

factorial(6);
// 720

factorial(7);
// 5040
```

The only purpose of `hideTheCache()` is to create a scope for `cache` to persist in across multiple calls to `factorial(..)`. For `factorial(..)` to have access to `cache` it must be defined within the same scope.

**NOTE**: Caching a function's computed output to optimize performance when repeated calls of the same input are expected is quite common in the Functional Programming world. Canonically referred to as _memoization_, this caching relies on closure. There are memory-usage concerns associated with this strategy and FP libraries will usually provide an optimized/vetted utility for the memoization of functions.

It will become tedious to define and name a `hideTheCache()` function scope everywhere a factorial needs to be calculated. A better approach may be to use a function expression:

```javascript
var factorial = (function hideTheCache() {
  var cache = {};

  function factorial(x) {
    if (x < 2) return 1;
    if (!(x in cache)) {
      cache[x] = x * factorial(x - 1);
    }
    return cache[x];
  }

  return factorial;
})();

factorial(6);
// 720

factorial(7);
// 5040
```

[Recall](../03/README.md#function-name-scope) that a function expression's identifier exists in it's own scope. The approach illustrated in the above example ensures that each function expression used wrap variables within its scope can be semantically named without polluting the scope it is defined in.

#### Invoking Function Expressions Immediately
In the example at the end of the previous section the entire function expression was wrapped in `( .. )();`. The wrapping parentheses `( .. )` is not strictly necessary but improves readability.

The second set of parentheses is actually calling the function expression preceding it. This common pattern is called an _Immediately Invoked Function Expression_ (IIFE).

An IIFE is useful for creating scope to hide variables/functions. Being an expression it can be used **any** place in a program JavaScript allows expressions. An IIFE can be named or anonymous. It can be a standalone statement or part of another statement.

Here is an example of a standalone IIFE:

```javascript
// outer scope

(function(){
  // inner hidden scope
})();

// more outer scope
```

Unlike the earlier `hideTheCache()` example, the wrapping `( .. )` are not optional for a standalone IIFE - they are required (technically there are other syntactic methods to ensure that an IIFE is treated as a function expression by the JavaScript parser).

#### Function Boundaries
Using an IIFE to define scope can have unintended consequences, depending on the code around it. An IIFE's function boundary alters the behavior of certain statements/constructs.

A `return` statement would change its meaning if an IIFE was wrapped around it. Non-arrow function IIFEs also change the binding of a `this` keyword. Statements like `break` and `continue` won't operate across an IIFE's boundary to control an outer loop or block.

If scope needs to be wrapped around code that contains `return`, `this`, `break`, or `continue` an IIFE is not the best approach - in these cases it may be better to create scope with a block rather than a function.

[▲ Return to Sections](#sections)

## Scoping With Blocks
In general, any curly brace pair `{ .. }` which is a statement will act as a block, but **not necessarily** as a scope.

A block only becomes a scope if necessary to contain its block-scoped declarations (`let`/`const`):

```javascript
{
  // not necessarily a scope (yet)

  // ..

  // now we know the block needs to be a scope
  let thisIsNowAScope = true;

  for (let i = 0; i < 5; i++) {
    // this is also a scope, activated each
    // iteration
    if (i % 2 == 0) {
      // this is just a block, not a scope
      console.log(i);
    }
  }
}
// 0 2 4
```

Not all `{ .. }` create blocks (and thus are eligible to create scopes):
* Object literals use curly-brace pairs to delimit their key-value lists, but these object values are not blocks.
* `class` uses curly-brace pairs around its body definition, but this is not a block or a scope.
* A `function` uses curly-brace pairs around its body definition, but this technically isn't a block - it's a single statement for the function body. It _is_, however, a (function) scope.
* A `switch` statement uses curly-brace pairs around its set of `case` clauses, but this does not definie a block or a scope.

A curly brace pair can define a block attached to a statement (`if`, `for`, etc) or stand alone. An explicit block like this is not actually a scope if it contains no declarations, and serves no operational purpose.

In most languages that support block-scoping, an explicit block scope is a common pattern for creating a narrow slice of scope for one or more variables.

An explicit block scope can be useful even inside of another block:

```javascript
if (somethingHappened) {
  // this is a block, but not a scope

  {
    // this is both a block and an
    // explicit scope
    let msg = somethingHappened.message();
    notifyOthers(msg);
  }

  // ..

  recoverFromSomething();
}
```

The curly-brace pair inside the `if` statement is an even smaller explicit block scope for `msg`, since that variable is not needed for the entire `if`-block.

If following PoLE, always define the smallest block for each variable within reason. It is recommended to use the extra explicit block demonstrated in the above example.

Another example using an explicit block scope:

```javascript
function getNextMonthStart(dateStr) {
  var nextMonth, year;

  {
    let curMonth;
    [ , year, curMonth ] = dateStr.match(
      /(\d{4})-(\d{2})-\d{2}/
    ) || [];
    nextMonth = (Number(curMonth) % 12) + 1;
  }

  if (nextMonth == 1) {
    year++;
  }

  return `${ year }-${
    String(nextMonth).padStart(2,"0")
    }-01`;
}
getNextMonthStart("2019-12-25");   // 2020-01-01
```

The reason to place `curMonth` in an explicit block scope instead of at the function scope level alongside `nextMonth` and `year` is that `curMonth` is only needed for the first two statements - at the function level it's over-exposed. This example is small, so the hazards of over-exposing `curMonth` are limited.

A more substantial example:

```javascript
function sortNamesByLength(names) {
  var buckets = [];

  for (let firstName of names) {
    if (buckets[firstName.length] == null) {
      buckets[firstName.length] = [];
    }
    buckets[firstName.length].push(firstName);
  }

  // a block to narrow the scope
  {
  let sortedNames = [];

  for (let bucket of buckets) {
    if (bucket) {
      // sort each bucket alphanumerically
      bucket.sort();

      // append the sorted names to our
      // running list
      sortedNames = [
        ...sortedNames,
        ...bucket
      ];
    }
  }

  return sortedNames;
  }
}

sortNamesByLength([
  "Sally",
  "Suzy",
  "Frank",
  "John",
  "Jennifer",
  "Scott"
]);
// [ "John", "Suzy", "Frank", "Sally",
//   "Scott", "Jennifer" ]
```

This example contains six identifiers across five different scopes. Each variable is defined at the innermost scope possible for the program to operate as desired.

`sortedNames` could have been defined in the top-level function scope, but it is only required in the second half of this function. Following PoLE, in an effort to avoid over-exposing `sortedNames`, it is block-scoped in the inner explict block scope.

#### `var` and `let`
In the above example, the variable `buckets` is declared with the `var` keyword because it is used across the entire function (except in the final `return` statement). Any variable that is needed across all (or most) of a function should be declared so that such usage is obvious.

**NOTE**: The parameter `names` isn't used across the entire function, but there is no way to limit the scope of a parameter.

There is both a semantic and technical reason to use `var` to declar `buckets`.

Stylistically `var` has always signaled a variable that belongs to the entire function. `var`-declared variables [attach themselves to the nearest enclosing function scope](../01/README.md#lexical-scope) no matter where they are placed. This is true even if the declaration occurs within a block:

```javascript
function diff(x,y) {
  if (x > y) {
    var tmp = x;    // `tmp` is function-scoped
    x = y;
    y = tmp;
  }

  return y - x;
}
```

While a `var` variable can be declared inside a block and still belong to the function-scope, with the exception of a few specific cases this is advised against. Generally `var` declarations should be reserved for use in the top-level scope of a function.

As a stylistic convention it is better to allow `var` to communicate function-scoped variables, and `let` to indicate block-scoped variables. As long as a program needs both function-scoped and block-scoped variables, the most readable approach is to use both `var` and `let`, each for their own best purpose.

**WARNING**: The recommendation to use both `var` and `let` is controversial and contradicts the majority. It is far more common to hear assertions that `var` is broken and to just use `let`. These opinions are valid, as is the one promoted by this book. `var` is neither broken nor deprecated.

#### Where to `let`?
The way to determine where each declaration in a program belongs is to determine what the minimal scope exposure that is sufficient for a given variable is. If a declaration belongs in a block scope, use `let`. If it belongs in the function scope, use `var`.

In `for`-loops the iterator is only ever used inside the loop and therefore should always be declared using `let`:

```javascript
for (let i = 0; i < 5; i++) {
  // do something
}
```

Virtually the only case where an iterator in a `for`-loop should be declared with `var` is if the iterator needs to be accessed outside of the loop:

```javascript
for (var i = 0; i < 5; i++) {
  if (checkValue(i)) {
    break;
  }
}

if (i < 5) {
  console.log("The loop stopped early!");
}
```

The above approach, however, smells of poor code structure and a preferable approach is to use another outer-scoped variable for that purpose:

```javascript
var lastI;

for (let i = 0; i < 5; i++) {
  lastI = i;
  if (checkValue(i)) {
    break;
  }
}

if (lastI < 5) {
  console.log("The loop stopped early!");
}
```

#### What's the Catch?
Since the introduction of `try..catch` in ES3 (in 1999), the `catch` clause has used an additional (little known) block-scoping declaration capability:

```javascript
try {
  doesntExist();
}
catch (err) {
  console.log(err);
  // ReferenceError: 'doesntExist' is not defined
  // ^^^^ message printed from the caught exception

  let onlyHere = true;
  var outerVariable = true;
}

console.log(outerVariable);     // true

console.log(err);
// ReferenceError: 'err' is not defined
// ^^^^ this is another thrown (uncaught) exception
```

The `err` variable declared by the `catch` clause is block-scoped to that block. Other block-scoped variables can be declared within the block using `let` but a `var` declaration within the block attaches to the outer function/global scope.

ES2019 changed the `catch` clause so that the error value declaration is optional. If omitted the `catch` block is no longer (by default) a scope - however, it is still a block.

If the error object is unnecessary, it can be omitted both to simplify the syntax and to remove an unnecessary scope:

```javascript
try {
  doOptionOne();
}
catch {   // catch-declaration omitted
  doOptionTwoInstead();
}
```

[▲ Return to Sections](#sections)

## Function Declarations in Blocks (FiB)
Are `function` declarations function-scoped like `var` declarations? Yes and no. Consider the following example:

```javascript
if (false) {
  function ask() {
    console.log("Does this run?");
  }
}
ask();
```

There are three reasonable outcomes to what might happen running this code:
1. The `ask()` call could fail with a `ReferenceError` exception because the `ask` identifier is block-scoped to the `if`-block and is therefore not available to the outer/global scope.
2. The `ask()` call could fail with a `TypeError` exception because the `ask` identifier exists but is `undefined` (since the `if` statement does not run).
3. The `ask()` call could run correctly, printing out the message.

Depending on what JavaScript environment this code runs in, the results could be different. This is an area where existing legacy behavior betrays a predictable outcome.

The JavaScript specification states that `function` declarations made inside of blocks are block-scoped, so the correct standards-specified behavior should be (1). However, most browser-based JavaScript engines (including V8, used in Chrome, Edge, Node, etc.) will behave as (2) - the identifier is scoped outside of the `if`-block but the function value is not auto-initialized.

The reason these engines contradict the standard is because they already had these behaviors around FiB defined prior to ES6's introduction of block scoping. There was concern that changing their behaviors to adhere to the specification might break existing website code.

One of the most common use cases for declaring a `function` within a block is to provide a conditional function declaration:

```javascript
if (typeof Array.isArray != "undefined") {
  function isArray(a) {
    return Array.isArray(a);
  }
}
else {
  function isArray(a) {
    return Object.prototype.toString.call(a)
    == "[object Array]";
  }
}
```

**WARNING**: In addition to risk of inconsistent FiB behavior across runtime environments, another problem with conditional function definitions is it makes a program harder to debug. Defining multiple versions of a function always causes a program to be harder to reason about and maintain.

There are several FiB corner cases whose behavior will likely vary across runtime environments:

```javascript
if (true) {
  function ask() {
    console.log("Am I called?");
  }
}

if (true) {
  function ask() {
    console.log("Or what about me?");
  }
}

for (let i = 0; i < 5; i++) {
  function ask() {
    console.log("Or is it one of these?");
  }
}

ask();

function ask() {
  console.log("Wait, maybe, it's this one?");
}
```

The only practical advice for avoiding the vagaries of FiB is to avoid FiB entirely. Never place a `function` declaration within any block - always place them within the top-level of a function or in the global scope.

While it is advised to never place function **declarations** inside of a block, it is perfectly fine to place function **expressions** inside of blocks:

```javascript
var isArray = function isArray(a) {
  return Array.isArray(a);
};

// override the definition, if you must
if (typeof Array.isArray == "undefined") {
  isArray = function isArray(a) {
    return Object.prototype.toString.call(a)
    == "[object Array]";
  };
}
```

[▲ Return to Sections](#sections)

| [Previous: Chapter 5 - The (Not So) Secret Lifecycle of Variables](../05/README.md) | [Table of Contents](../README.md#table-of-contents) |
