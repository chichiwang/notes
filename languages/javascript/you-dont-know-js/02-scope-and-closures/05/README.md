# Chapter 5: The (Not So) Secret Lifecycle of Variables
JavaScript's particular flavor of lexical scope is rich in nuance in how and when variables come into existence and become available in the program.

## Sections
* [When Can I Use a Variable?](#when-can-i-use-a-variable)
  * [Hoisting: Declaration vs. Expression](#hoisting-declaration-vs-expression)
  * [Variable Hoisting](#variable-hoisting)
* [Hoisting: Yet Another Metaphor](#hoisting-yet-another-metaphor)
* [Re-declaration?](#re-declaration)
  * [Constants?](#constants)
  * [Loops](#loops)
* [Unitialized Variables (aka, TDZ)](#unitialized-variables-aka-tdz)
* [Finally Initialized](#finally-initialized)

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

The above program will not execute and will instead immediately throw a `SyntaxError`. The error message will indicate something like `studentName has already been declared.` `let` explicitly disallows re-declaration.

If any of a repeated declaration of an identifier uses `let`, the same error will occur:

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

#### Constants?
The `const` keyword is even more constrained than `let`. `const` disallows re-declaration of a repeated identifier within the same scope, like `let`, but there is an overriding technical reason for doing so (unlike `let`).

The `const` keyword requires a variable to be initialized so using `const` without assignment results in a `SyntaxError`:

```javascript
const empty;   // SyntaxError
```

`const`-declared variables cannot be re-assigned:

```javascript
const studentName = "Frank";
console.log(studentName);
// Frank

studentName = "Suzy";   // TypeError
```

**WARNING**: The error thrown when attempting to reassign `studentName` is a `TypeError` and not a `SyntaxError`. This is an important distinction: Syntax errors represent faults in the program that stop it from starting execution. A `TypeError`, by contrast, occurs during runtime. In the above example the console will print out `"Frank"` before processing the re-assignment of `studentName` and throwing an error.

The clear technical reason the `const` keyword must disallow re-assignment:
* `const` declarations cannot be re-assigned.
* `const` declarations always require an assignment.

Since `const` disallows re-assignment on these technical grounds, TC39 felt that `let` must similarly disallow re-assignment for consistency. It is debatable if this was the right decision.

#### Loops
Consider the following:

```javascript
var keepGoing = true;
while (keepGoing) {
  let value = Math.random();
  if (value > 0.5) {
    keepGoing = false;
  }
}
```

All the rules of scope, including re-declaration of `let`-created variables, are applied _per scope instance_. Each time a scope is entered during execution, everything resets.

In the above example, each loop iteration is its own scope instance. Within each scope instance, `value` is only being declared once.

If `value` were declared using `var` instead of `let`:

```javascript
var keepGoing = true;
while (keepGoing) {
  var value = Math.random();
  if (value > 0.5) {
    keepGoing = false;
  }
}
```

The `var`-declared `value` identifier is not block-scoped, and lives at the same scope as the variable `keepGoing`. In this instance there is only one variable `value` that is being re-assigned. There is no re-declaration because each subsequent `var` declaration is a no-op and ignored (only the assignment is excuted).

`var`, `let`, and `const` keywords are effectively removed from the code by the time execution begins - they are all handled by the compiler.

In the case of loop forms like the `for`-loop:

```javascript
for (let i = 0; i < 3; i++) {
  let value = i * 10;
  console.log(`${ i }: ${ value }`);
}
// 0: 0
// 1: 10
// 2: 20
```

The `i` declaration occurs in the `for`-loop body, in the same scope as `value`. `i` and `value` are both declared exactly once **per scope instance**.

The same is true of `for..in` and `for..of` loop forms:

```javascript
for (let index in students) {
  // this is fine
}

for (let student of students) {
  // so is this
}
```

Looking at how `const` impacts looping constructs, consider:

```javascript
var keepGoing = true;
while (keepGoing) {
  // ooo, a shiny constant!
  const value = Math.random();
  if (value > 0.5) {
    keepGoing = false;
  }
}
```

As with the `let` example, the `const`-declaration is occurring once per scope instance so no error will occur.

`for..in` and `for..of` loops also behave fine with `const`-declarations for the same reason:

```javascript
for (const index in students) {
  // this is fine
}

for (const student of students) {
  // this is also fine
}
```

But the general `for`-loop can fail:

```javascript
for (const i = 0; i < 3; i++) {
  // oops, this is going to fail with
  // a Type Error after the first iteration
}
```

The reason is the iteration command `i++` is attempting to re-assign the value of `i`. Absent attempts at re-assignment, the usage of `const` within a general `for`-loop is fine:

```javascript
var keepGoing = true;

for (const i = 0; keepGoing; /* nothing here */ ) {
  keepGoing = (Math.random() > 0.5);
  // ..
}
```

While the above example works it is pointless - the purpose of declaring the index `i` is to use it to count iterations. Declaring it with a `const` does not make sense - it is better to declare it with `let` or to just use a `while`-loop.

[▲ Return to Sections](#sections)

## Unitialized Variables (aka, TDZ)
Variables declared with `var` are hoisted to the top of their function-scope and initialized with `undefined`. `let` and `const` declared variables do not behave quite the same.

Consider the following code:

```javascript
console.log(studentName);
// ReferenceError

let studentName = "Suzy";
```

A `ReferenceError` is thrown on line 1 at runtime. The error message may say something like `Cannot access studentName before initialization`. `studentName` exists on line 1 but has not been initialized and therefore cannot be accessed.

```javascript
studentName = "Suzy";   // let's try to initialize it!
// ReferenceError

console.log(studentName);

let studentName;
```

`studentName` is unavailabe for both _read_ and _write_ prior to initialization. The only way for `let`/`const`-declared variables to be initialized is by use of a declaration statement:

```javascript
let studentName = "Suzy";
console.log(studentName);   // Suzy
```

The `let`-declaration statement in the above example initializes `studentName` to the string `"Suzy"`. Alternatively:

```javascript
// ..

let studentName;
// or:
// let studentName = undefined;

// ..

studentName = "Suzy";

console.log(studentName);
// Suzy
```

**NOTE**: It was previously noted that `var studentName;` is equivalent to `var studentName = undefined` - but rather that `studentName` is auto-initialized to `undefined` at the top of the scope. In the above example it appears `let studentName;` is equivalent to `let studentName = undefined;`. The distinction here is that while `var`-declared variables to auto-initialized to `undefined` at the top of the scope, `let`-declared variables do not initialize until execution of the declaration statement.

While the compiler registers identifiers at the top of the scope they are declared in, `let` and `const`-declared variables are not initialized until the point in the program where they are declared. Prior to this point, these identfiers cannot be accessed.

The term coined by TC39 to refer to the _period of time_ from entering the scope until where the initialization of the variable occurs is _Temporal Dead Zone_ (_TDZ_). Only after a variable is initialized can it be accessed.

Only `let` and `const`-declared variables have an observable TDZ.

Temporal Dead Zone refers to _time_ and not _position in the code_:

```javascript
askQuestion();
// ReferenceError

let studentName = "Suzy";

function askQuestion() {
  console.log(`${ studentName }, do you know?`);
}
```

Although the `console.log` statement that references `studentName` comes after the `let`-delcaration, `askQuestion` is invoked before the `let`-statment is encountered and while `studentName` is still in the TDZ.

A common misconception is that because the TDZ exists, `let` and `const` variables do not hoist. They do. However, they do not automatically initialize at the top of the scope the way `var`-declared variables do. Auto-initialization and hoisting are two distinct operations that should not be lumped together under the term "hoisting".

[Shadowing](../03/README.md#shadowing) can be used to prove that `let`/`const`-declared variables hoist:

```javascript
var studentName = "Kyle";

{
  console.log(studentName);
  // ???

  // ..

  let studentName = "Suzy";

  console.log(studentName);
  // Suzy
}
```

If the `let`-declared `studentName` variable did not hoist, the first `console.log(..)` statement should print `"Kyle"`. However, running this program will result in a runtime TDZ error at the first `console.log(..)` statement because the inner-scope `studentName` was hoisted to the top of that block-scope. Initialzation of the `let`-declared `studentName` variable does not occur until after the `console.log(..)` statement.

It is a good idea to place all `let`/`const` declarations at the top of any scope to avoid TDZ errors.

[▲ Return to Sections](#sections)

## Finally Initialized
_Hoisitng_, _(re)declaration_, and the _Temporal Dead Zone_ are common sources of confusion for developers (especially those who have worked in other languages prior to JavaScript).

_Hoisting_ is generally cited as an explicit mechanism of the JavaScript engine, but it's really more of a metaphor to describe the various ways JavaScript handles variable declarations during compilation.

Declaration and re-declaration of variables tend to cause confusion when thought of as runtime operations, but shifting to compile-time thinking on these topics clears up the behaviors.

[▲ Return to Sections](#sections)

| [Previous: Chapter 4 - Around The Global Scope](../04/README.md) | [Table of Contents](../README.md#table-of-contents) |
