# Chapter 5: The (Not So) Secret Lifecycle of Variables
JavaScript's particular flavor of lexical scope is rich in nuance in how and when variables come into existence and become available in the program.

## Sections
* [When Can I Use a Variable?](#when-can-i-use-a-variable)
  * [Hoisting: Declaration vs. Expression](#hoisting-declaration-vs-expression)

[◂ Return to Table of Contents](../README.md)

## When Can I Use a Variable?
Consider:

```javascript
greeting();
// Hello!

function greeting() {
  console.log("Hello!");
}
```

The identifier `greeting` is accessed on line 1 even though the `greeting()` function declaration doesn't occur until line 4. This works because all identifiers are registered to their scopes at compile time and every identifier is created at the beginning of the scope it belongs to **every time that scope is entered**.

_Hoisting_ is the term commonly used to refer to a variable being visible from the beginning of its enclosing scope, even if its declaration appears further down in the code.

With formal function declarations, _function hoisting_ ensures that in addition to the identifier being registered, it is auto-initialized with the function's reference. This is unique to formal function declarations.

Both _function hoisting_ and `var`-flavored _variable hoisting_ attach the identifiers to the nearest enclosing **function scope**, and not a block scope.

**NOTE**: Variables declared with `let` and `const` also hoist, but these declaration forms attach their identifiers to the enclosing block, rather than the enclosing function.

#### Hoisting: Declaration vs. Expression
_Function hoisting_ only applies to formal function declarations and not to function expressions:

```javascript
greeting();
// TypeError

var greeting = function greeting() {
  console.log("Hello!");
};
```

The invocation of `greeting()` on line 1 throws a `TypeError` meaning the requested operation on the particular value is not allowed. Depending on the JavaScript environment the error message may say something like `'undefined' is not a function` or `'greeting' is not a function`.

The error is not a `ReferenceError` - the identifier `greeting` was found but at the time (line 1) does not hold a function. Variables declared with the `var` keyword are hoisted but also initialized with the value `undefined`. Once initialized a variable is available for use throughout the entire scope.

A `function` declaration is hoisted and initialized to its function reference.

A `var` declaration is hoisted and initialized to `undefined`.

[▲ Return to Sections](#sections)

| [Previous: Chapter 4 - Around The Global Scope](../04/README.md) | [Table of Contents](../README.md#table-of-contents) |
