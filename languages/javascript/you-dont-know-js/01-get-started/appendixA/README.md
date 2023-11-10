# Appendix A: Exploring Further
This appendix will explore some of the topics from the main chapter text in greater detail.

## Sections
* [Values vs. References](#values-vs-references)
* [So Many Function Forms](#so-many-function-forms)

[◂ Return to Table of Contents](../README.md)

## Values vs. References
In many languages a developer chooses between assigning/passing a value as either the value itself or as a reference to the value. In JavaScript this decision is entirely determined by the kind of value.

Assigning or passing the value itself will make a copy of that value:

```javascript
var myName = "Kyle";

var yourName = myName;
```

In the above example `yourName` references a copy of the string `"Kyle"` separate from the string that the `myName` variable contains. This is because the value is a primitive (a string in this case) and primitive values are always assigned or passed as _value copies_.

The proof that primitives are assigned/passed as values:

```javascript
var myName = "Kyle";

var yourName = myName;

myName = "Frank";

console.log(myName);
// Frank

console.log(yourName);
// Kyle
```

In the above example `yourName` is not affected by the reassignment of `myName`. This is because each variable holds its own copy of the value.

Reference would mean that variables point to the same shared value - changes to the shared value would be reflected across all variables that reference it. In JavaScript object values (arrays, objects, functions, etc) are treated as references:

```javascript
var myAddress = {
  street: "123 JS Blvd",
  city: "Austin",
  state: "TX",
};

var yourAddress = myAddress;

// I've got to move to a new house!
myAddress.street = "456 TS Ave";

console.log(yourAddress.street);
// 456 TS Ave
```

Both `myAddress` and `yourAddress` contain references to the same object, so the property reassignment made to `myAddress.street` is reflected by access to `yourAddress.street`.

JavaScript chooses value-copy vs. reference-copy based on the value type: primitives are held by value, objects are held by reference.

[▲ Return to Sections](#sections)

## So Many Function Forms
Taking this snippet from [Chapter 2](../02/README.md#functions) as example:

```javascript
var awesomeFunction = function(coolThings) {
  // ..
  return amazingStuff;
};
```

The function expression above is referred to as an _anonymous function expression_ because it has no name identifier between the `function` keyword and the `(..)` parameter list. This may be confusing because as of ES6 JavaScript performs a "name inference" on anonymous functions:

```javascript
awesomeFunction.name;
// "awesomeFunction"
```

The `name` property of a function will return either its directly given name (if the function was defined in a declaration) or its inferred name in the case of an assignment expression. This value is mostly used by developer tools when inspecting a function value or when reporting an error stack trace.

_Name inference_ only occurs in limited cases such as assignment using the `=` operator. If a function expression is passed to a function call, however, the `name` property will be an empty string and the developer console will usually report `(anonymous function)`.

Even when a name is inferred an anonymous function remains anonymous. The inferred name is a metadata string value, not an available identifier to refer to the function. An anonymous function is unable to refer to itself from within its own scope (for recursion, event unbinding, etc).

Compare this to:

```javascript
// let awesomeFunction = ..
// const awesomeFunction = ..
var awesomeFunction = function someName(coolThings) {
  // ..
  return amazingStuff;
};

awesomeFunction.name;
// "someName"
```

This is, by contrast, a _named function expression_ because the identifier `someName` is directly associated with the function expression at compile-time. The association with the variable `awesomeFunction` doesn't occur until runtime.

The explicit function name `someName` takes precendence for the `name` property.

Opinions vary on if function expressions should have be named or anonymous. For the sake of clarity and readability it is better to prefer named function expressions over anonymous ones.

There are many other function definition forms in JavaScript as of early 2020:

```javascript
// generator function declaration
function *two() { .. }

// async function declaration
async function three() { .. }

// async generator function declaration
async function *four() { .. }

// named function export declaration (ES6 modules)
export function five() { .. }

// IIFE
(function(){ .. })();
(function namedIIFE(){ .. })();

// asynchronous IIFE
(async function(){ .. })();
(async function namedAIIFE(){ .. })();

// arrow function expressions
var f;
f = () => 42;
f = x => x * 2;
f = (x) => x * 2;
f = (x,y) => x * y;
f = x => ({ x: x * 2 });
f = x => { return x * 2; };
f = async x => {
  var y = await doSomethingAsync(x);
  return y * 2;
};
someOperation( x => x * 2 );
// ..
```

Arrow function expressions are _syntactically anonymous_ - the syntax does not provide a way to provide a direct name identifier to the function. Arrow functions may get inferred names but only in assignment expressions, not when passed as argument.

It is not a good idea to use anonymous functions frequently so it is best to avoid using the arrow function form. The arrow function serves the purpose of creating a function that handles the `this` keyword lexically, but that is not a good reason to use it everywhere. Use the appropriate tool for the job.

Functions can also be specified in class and object literal definitions. In these contexts they are often referred to as _methods_. JavaScript does not have much obsrevable difference in these definition forms over a regular function:

```javascript
class SomethingKindaGreat {
  // class methods
  coolMethod() { .. }   // no commas!
  boringMethod() { .. }
}

var EntirelyDifferent = {
  // object methods
  coolMethod() { .. },   // commas!
  boringMethod() { .. },

  // (anonymous) function expression property
  oldSchool: function() { .. }
};
```

Build familiarity with all function forms to recognize them in existing code and to use them appropriately as context demands.

[▲ Return to Sections](#sections)

| [Previous: Chapter 4 - The Bigger Picture](../04/README.md) | [Table of Contents](../README.md#table-of-contents) |
