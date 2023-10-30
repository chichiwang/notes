# Chapter 2: Surveying JS
The best way to learn JS is to start writing JS.

## Sections
* [Each File is a Program](#each-file-is-a-program)
* [Values](#values)
* [Arrays and Objects](#arrays-and-objects)
* [Value Type Determination](#value-type-determination)
* [Declaring and Using Variables](#delaring-and-using-variables)
* [Functions](#functions)

[◂ Return to Table of Contents](../README.md)

## Each File is a Program
Almost every web site/application is comprised of many different JS files (typically with the `.js` extension). JS does not see the whole application as a single program: each standalone file is its own separate program.

The reason it is important to recognize this is mainly because of how error handling works. If one file fails (during parse/compile or execution) that will not necessarily prevent the next file from being processed. It is important to ensure that each file works properly and handles failures in other files as gracefully as possible. The only way multiple standalone JS files act as a single program is by sharing state and access to public functionality via the "global scope".

Many projects use build process tools to combine many separate project files into a single output file to be delivered to a web page. In these cases JS treats this single combined file as the entire program.

Since ES6 JavaScript has also supported a file-based module format. If a file is loaded via a module-loading mechanism (`import` statement or `<script type="module">` tag) all of its code is treated as a single module. JS still treats each module separtely: similar to how "global" scope allows standalone files to interoperate at runtime, importing one module into another allows runtime interoperation.

Regardless of code organization pattern and loading mechanisms, each file should be thought of as its own (mini) program that cooperate with other (mini) programs to perform the functions of the overall application.

[▲ Return to Sections](#sections)

## Values
The most fundamental unit of information in a program is a _value_. Values are data and they are how a program maintains state. Values come in two forms in JavaScript: **primitive** and **object**.

Values are embedded in programs as literals:
```javascript
greeting("My name is Kyle.");
```

The value `"my name is Kyle."` is a primitive string literal (ordered collections of characters usually used to represent words and sentences). A double quote `"` or single quote `'` are used to _delimit_ (surround, separate, define) a string value. Which quote character to use is entirely stylistic but it is important to code readability to choose one and use it consistently throughout the program.

A backtick character `` ` `` can also be used to delimit a string, however there is a behavioral difference from using the single or double quotes:
```javascript
let firstName = "Kyle";

console.log("My name is ${ firstName }.");
// My name is ${ firstName }.

console.log('My name is ${ firstName }.');
// My name is ${ firstName }.

console.log(`My name is ${ firstName }.`);
// My name is Kyle.
```

The backtick-delimited string resolves the variable expression indicated with `${ .. }` to its current value. This delimiter is used to denote a [template literal](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals) utilizing a strategy called **string interpolation**. The backtick-delimiter can be used without including interpolated expressions but that defeats the purpose of using this syntax. It is better to use `"` or `'` and reserve `` ` `` only for strings that include interpolated expressions.

Other primitive literal values include **booleans** and **numbers**:
```javascript
while (false) {
  console.log(3.141592);
}
```

[while](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/while) represents a loop type that repeats operations _while_ it's condition is true. In the above example the loop will never run because the loop conditional supplied is the boolean value `false`. `true` would have resulted in the loop running infinitely.

The number `3.141592` is an approximation of mathematical PI to the first six digits. Typically the predefined constant `Math.PI` would be used instead. Another type of number is the [BigInt](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt) (big integer) primitive used for storing arbitrarily large numbers. Numbers are often used in programs for counting steps (ex: loop iterations) and accessing data in numeric positions (ex: array index).

Two other _primitive values_ in JavaScript (in addition to strings, numbers, and booleans) are `null` and `undefined`. While there are differences between them they mostly serve the same purpose for indicating _emptiness_ or absence of a value. Many assume/treat these values as interchangable (and they can be) but it's safest to use only `undefined` as the single empty value:

```javascript
while (value != undefined) {
  console.log("Still got something!");
}
```

The final primitive value to be aware of is a symbol: a special-purpose value that behaves as a hidden unguessable value. Symbols are almost exclusively used as special keys on objects:

```javascript
hitchhikersGuide[ Symbol("meaning of life") ];
// 42
```

Direct usage of symbols are uncommon in typical JS programs - they are most widely used in low-level code such as libraries and frameworks.

[▲ Return to Sections](#sections)

## Arrays and Objects
Objects are the other value type used in JS besides primitives.

Arrays are a special type of object that is comprised of an ordered, numerically-indexed list of data:

```javascript
var names = [ "Frank", "Kyle", "Peter", "Susan" ];

names.length;
// 4

names[0];
// Frank

names[1];
// Kyle
```

Arrays can hold any value type, either primitive or object (including other arrays). Even functions are values that can be held in arrays or objects. Functions are a special sub-type of object, like arrays.

Objects are more general: unordered, keyed-collections of any various values. Elements of an object are accessed by a string location name (aka: key or property) rather than by numeric position:

```javascript
var me = {
  first: "Kyle",
  last: "Simpson",
  age: 39,
  specialties: [ "JS", "Table Tennis" ]
};

console.log(`My name is ${ me.first }.`);
```

`me` is assigned an object, `first` represents the name of a location of information in that object. `me.first` is a syntax used to access the value stored at `first` in the object referenced by `me`. Another syntax to access this information is by using square brackets `[]` (`me["first"]`).

[▲ Return to Sections](#sections)

## Value Type Determination
The `typeof` operator is used to determine a value's built-in types:

```javascript
typeof 42;                  // "number"
typeof "abc";               // "string"
typeof true;                // "boolean"
typeof undefined;           // "undefined"
typeof null;                // "object" -- oops, bug!
typeof { "a": 1 };          // "object"
typeof [1,2,3];             // "object"
typeof function hello(){};  // "function"
```

`typeof null;` returns `"object"` instead of `"null"`. Similarly `typeof` does not return `"array"` for an array.

Converting from one value type to another (ex: string to number) in JavaScript is referred to as **coercion**. Primitive values and object values behave differently when they're assigned or passed around.

[▲ Return to Sections](#sections)

## Declaring and Using Variables
In JavaScript programs values can either appear as literal values or they can be held in variables. Variables can be thought of as containers for values.

Variables have to be declared (created) to be used. There are various syntax forms that declare variables - each form has different implied behaviors.

Consider the `var` statement:

```javascript
var myName = "Kyle";
var age;
```

`var` declares a variable to be used in that part of the program, and optionally allows an initial assignment of value.

Another similar keyword is `let`:

```javascript
let myName = "Kyle";
let age;
```

`let` has some differences to `var`, the most obvious being that `let` allows a more limited access to the variable than `var` (_block scoping_ as opposed to regular or function scoping):

```javascript
var adult = true;

if (adult) {
  var myName = "Kyle";
  let age = 39;
  console.log("Shhh, this is a secret!");
}

console.log(myName);
// Kyle

console.log(age);
// Error!
```

The attempt to access `age` outside of the `if` block results in an error because `age` was block-scoped whereas `myName` wasn't. Block-scoping is useful for preventing accidental overlap of variable names. `var` is more useful when the variable will be used by a wider scope within the function.

**Note**: It's a common suggestion that `var` should be avoided in favor of `let` and `const` because of the perceived confusion of how the scoping behavior of `var` has worked. This is overly restrictive and unhelpful advice. It assumes the inability to learn a feature properly in combination with other features. Instead learn any features available and use them where appropriate.

A third declaration form `const` is similar to `let` in scoping, but it contains an additional limitation that it must be given a value the moment it is declared and cannot be reassigned to another value.

```javascript
const myBirthday = true;
let age = 39;

if (myBirthday) {
  age = age + 1;    // OK!
  myBirthday = false;  // Error!
}
```

Attempting to reassign the `myBirthday` constant results in an exception.

`const` declared variables are not "unchangeable", they just cannot be reassigned. Avoid using `const` with object values because the contents of these values can be changed which can lead to confusion:

```javascript
const actors = [
  "Morgan Freeman", "Jennifer Aniston"
];

actors[2] = "Tom Cruise";   // OK :(
actors = [];                // Error!
```

Using `const` only to assign primitive values to variables avoids the confusion of re-assigment (not allowed) with mutation (allowed).

Function declarations provide another syntactic form of declaring identifiers (variables) in various scopes:

```javascript
function hello(myName) {
  console.log(`Hello, ${ myName }.`);
}

hello("Kyle");
// Hello, Kyle.
```

The variable `hello` is created in the outer scope and automatically assigned the function. The named parameter `myName` is created inside the function and only accessible in the function's scope. Both `hello` and `myName` generally behave as `var`-declared.

Another syntax that declares a variable is a `catch`-clause:

```javascript
try {
  someError();
}
catch (err) {
  console.log(err);
}
```

`err` is a block-scoped variable only accessible inside the `catch` block. It behaves as if it had been declared with `let`.

[▲ Return to Sections](#sections)

## Functions
In the world of Functional Programming "function" has a precise mathematical definition and implies a strict set of rules to abide by. In JavaScript "function" should be taken to take a broader meaning of a related term: "procedure." A procedure is a collection of statements that can be invoked one or more times, may be provided some inputs, and may give back one or more outputs.

A _function definition_ looks like:

```javascript
function awesomeFunction(coolThings) {
  // ..
  return amazingStuff;
}
```

It is called a function definition because it appears as a statement by itself and not an expression in another statement. The association between the identifier `awesomeFunction` and the function value is made during the compile phase of the code before the code is executed.

A function defined in an expression looks like:

```javascript
// let awesomeFunction = ..
// const awesomeFunction = ..
var awesomeFunction = function(coolThings) {
  // ..
  return amazingStuff;
};
```

Unlike a function definition, this _function expression_ is not associated with its identifier until the statement is executed at runtime. In JavaScript functions are values that can be assigned and passed around as values. Functions are a special object value type. Not all languages treat functions as values, but it is important for languages that support the functional programming pattern to.

JavaScript functions can receive parameter input:

```javascript
function greeting(myName) {
  console.log(`Hello, ${ myName }!`);
}

greeting("Kyle");   // Hello, Kyle!
```

In the above example `myName` is a parameter and acts like a local variable within the function. Functions can be defined to receive any number of parameters (0+). At runtime each parameter is assigned the value passed to it, in the function call, in the corresponding position (in this case `"Kyle"`).

Functions can return values using the `return` keyword:

```javascript
function greeting(myName) {
  return `Hello, ${ myName }!`;
}

var msg = greeting("Kyle");

console.log(msg);   // Hello, Kyle!
```

Only a single value can be returned, but that value can be a single object/array containing multiple values.

Since functions are are values they can be assigned as properties on objects:

```javascript
var whatToSay = {
  greeting() {
    console.log("Hello!");
  },
  question() {
    console.log("What's your name?");
  },
  answer() {
    console.log("My name is Kyle.");
  }
};

whatToSay.greeting();
// Hello!
```

The object definition `whatToSay` defines three functions `greeting()`, `question()`, and `answer()`. Each function is called by accessing the property to retrieve the function reference value.

[▲ Return to Sections](#sections)

| [Previous: Chapter 1 - What is JavaScript?](../01/README.md) | [Table of Contents](../README.md#table-of-contents) |
