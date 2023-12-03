# Chapter 6: Limiting Scope Exposure
This chapter looks at how and why different levels of scope (functions and blocks) should be used to organize a program's variables, specifically to reduce scope over-exposure.

## Sections
* [Least Exposure](#least-exposure)
* [Hiding in Plain (Function) Scope](#hiding-in-plain-function-scope)
  * [Invoking Function Expressions Immediately](#invoking-function-expressions-immediately)

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

[▲ Return to Sections](#sections)

| [Previous: Chapter 5 - The (Not So) Secret Lifecycle of Variables](../05/README.md) | [Table of Contents](../README.md#table-of-contents) |
