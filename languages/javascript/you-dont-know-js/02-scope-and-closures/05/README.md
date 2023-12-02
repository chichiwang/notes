# Chapter 5: The (Not So) Secret Lifecycle of Variables
JavaScript's particular flavor of lexical scope is rich in nuance in how and when variables come into existence and become available in the program.

## Sections
* [When Can I Use a Variable?](#when-can-i-use-a-variable)
  * [Hoisting: Declaration vs. Expression](#hoisting-declaration-vs-expression)
  * [Variable Hoisting](#variable-hoisting)
* [Hoisting: Yet Another Metaphor](#hoisting-yet-another-metaphor)
* [Re-declaration?](#re-declaration)

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

#### Variable Hoisting
Consider the following:

```javascript
greeting = "Hello!";
console.log(greeting);
// Hello!

var greeting = "Howdy!";
```

`greeting` is available for assignment on line 1 even though it is not formally declared until line 5:
* The identifier `greeting` is hoisted to the top of scope
* `greeting` is automatically initialized to the value `undefined` at the top of the scope

**NOTE**: Using _variable hoisting_ in this way feels unnatural and many developers would be right to avoid relying on this sort of usage in their programs.

[▲ Return to Sections](#sections)

## Hoisting: Yet Another Metaphor
It is useful to think of hoisting as a set of actions JavaScript takes before execution. The explanation often asserted for how hoisting works is that the JavaScript engine will actually rewrite the program to move the declarations to the top of scope.

Taking the above example:

```javascript
greeting = "Hello!";
console.log(greeting);
// Hello!

var greeting = "Howdy!";
```

One could imagine that the JavaScript engine rewrites it to look like the following prior to execution:

```javascript
var greeting;           // hoisted declaration
greeting = "Hello!";    // the original line 1
console.log(greeting);  // Hello!
greeting = "Howdy!";    // `var` is gone!
```

The metaphor also asserts that `function` declarations are, in their entirety, moved to top of their scope so that:

```javascript
studentName = "Suzy";
greeting();
// Hello Suzy!

function greeting() {
  console.log(`Hello ${ studentName }!`);
}
var studentName;
```

Becomes (prior to program execution):

```javascript
function greeting() {
  console.log(`Hello ${ studentName }!`);
}
var studentName;

studentName = "Suzy";
greeting();
// Hello Suzy!
```

This hoisting metaphor is convenient, thinking about a program as if the JavaScript engine executes it in a single pass, top-down. This simplification of re-ordering the code is attractive and easy to understand, but it is not accurate. The JavaScript engine does not actually re-arrange the code nor does it look ahead to find declarations, it instead parses the code ahead of execution.

Parsing is the first phase of the two-phase processing.

**WARNING**: Incorrect or incomplete mental models often seem sufficient because they can occasionally lead to accidental correct answers, but in the long run it is harder to accurately analyze and predict outcomes without aligning mental models with how the JavaScript engine actually works.

Hoising _should_ be used to refer to the **compile-time operation** of generating runtime instructions for the automatic registration of a variable at the beginning of its scope, each time that scope is entered. This is an important distinction: hoisting as a compile-time task rather than a runtime behavior.

[▲ Return to Sections](#sections)

## Re-declaration?
Consider the following:

```javascript
var studentName = "Frank";
console.log(studentName);
// Frank

var studentName;
console.log(studentName);   // ???
```

Many would consider that `studentName` has been re-declared and thus expect the second console log to print `undefined`. However, there is no such thing as re-declaring a variable.

Applying the hoising metaphor to this example, the code would be re-arranged like this for execution:

```javascript
var studentName;
var studentName;    // clearly a pointless no-op!

studentName = "Frank";
console.log(studentName);
// Frank

console.log(studentName);
// Frank
```

Since hoisting is about registering a variable to a scope, the second `var studentName` statement is a no-op. It is important to note that `var studentName;` does not equate to `var studentName = undefined;` as some may assume.

Looking at another example:

```javascript
var studentName = "Frank";
console.log(studentName);   // Frank

var studentName;
console.log(studentName);   // Frank <--- still!

// let's add the initialization explicitly
var studentName = undefined;
console.log(studentName);   // undefined <--- see!?
```

The explicit `= undefined` initializations produces a different result than when it is omitted.

A repeated `var` declaration of the same identifier name in a scope is effectively a no-op. Here's another illustration using a function declaration:

```javascript
var greeting;

function greeting() {
  console.log("Hello!");
}

// basically, a no-op
var greeting;

typeof greeting;        // "function"

var greeting = "Hello!";

typeof greeting;        // "string"
```

The first `greeting` declaration registers the identifier to the scope and auto-initializes it to `undefined`. The `function` declaration does not re-register the identifier, but _function hoisting_ overrides the auto-initialization to use the function reference. The second `var greeting;` statement doesn't do anything since `greeting` is already an identifier and _function hoisting_ took precedence for auto-initialization.

Assinging `"Hello!"` to `greeting` changes its value to a string, the `var` keyword is a no-op in that statement.

`let` and `const` behave differently:

```javascript
let studentName = "Frank";

console.log(studentName);

let studentName = "Suzy";
```

The above program will not execute and will instead immediately throw a `SyntaxError`. The error message will indicate something like `studentName has already been declared.` `let` and `const` explicitly disallow re-declaration.

If any of a repeated declaration of an identifier uses `let` or `const`, the same error will occur:

```javascript
var studentName = "Frank";

let studentName = "Suzy";
```

and

```javascript
let studentName = "Frank";

var studentName = "Suzy";
```

Both of these programs will throw a `SyntaxError` on the second declaration.

This is because re-declaration of variables is seen by some, including many on the TC39 body, as a bad habit that can lead to program bugs. When ES6 introduced `let` they decided to ban re-declaration with an error. It is allowed with `var` and `function` for forwards compatibility reasons.

**NOTE**: This is a stylistic opinion and not a technical argument. A reasonable case could be made that maintaining consistency with `var`'s precedent would have been prudent.

[▲ Return to Sections](#sections)

| [Previous: Chapter 4 - Around The Global Scope](../04/README.md) | [Table of Contents](../README.md#table-of-contents) |
