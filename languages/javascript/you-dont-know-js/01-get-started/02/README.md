# Chapter 2: Surveying JS
The best way to learn JS is to start writing JS.

## Sections
* [Each File is a Program](#each-file-is-a-program)
* [Values](#values)
* [Arrays and Objects](#arrays-and-objects)
* [Value Type Determination](#value-type-determination)
* [Declaring and Using Variables](#delaring-and-using-variables)
* [Functions](#functions)
* [Comparisons](#comparisons)
  * [Equal...ish](#equalish)
  * [Coercive Comparisons](#coercive-comparisons)
* [How We Organize in JS](#how-we-organize-in-js)
  * [Classes](#classes)
  * [Class Inheritance](#class-inheritance)
  * [Modules](#modules)
  * [Classic Modules](#classic-modules)
  * [ES Modules](#es-modules)

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

## Comparisons
Creating branching conditional logic in programs requires comparing values to determine their identity and relationship with each other. JavaScript has several mechanisms to enable value comparison.

#### Equal...ish
Sometimes an equality comparison intends _exact_ matching but other times the desired comparison is _closely similar_ or _interchangeable_ matching. It is important to be aware of the difference between an **equality** comparison and an **equivalence** comparison.

In JavaScript the triple-equals `===` operator is described as the "strict equality" operator. Most values compared with an `===` equality comparison will behave as expected to determine if the values are exactly the same:

```javascript
3 === 3.0;              // true
"yes" === "yes";        // true
null === null;          // true
false === false;        // true

42 === "42";            // false
"hello" === "Hello";    // false
true === 1;             // false
0 === null;             // false
"" === null;            // false
null === undefined;     // false
```

`===` is often described as comparing both the value and type. In several of the examples above, such ash `42 === "42"` the _type_ of the values (number and string) does seem to be the distinguishing factor. However, in JavaScript **all** comparisons take into account both the type and values being compared. `===` specifically disallows any sort of type conversion (coercion) in its comparison while other JS comparisons do allow coercion.

There are two instances where the `===` operator is designed to lie:

```javascript
NaN === NaN;            // false
0 === -0;               // true
```

For `-0` and `NaN` it is best to avoid using `===` to check for equality. Instead `Number.isNaN(..)` can be used to check for `NaN` and `Object.is(..)` can be used for anything including `-0`. `Object.is(..)` is the strictest tool for comparison in JavaScript.

`===` also gets tricky when comparing non-primitive values (objects):

```javascript
[ 1, 2, 3 ] === [ 1, 2, 3 ];    // false
{ a: 42 } === { a: 42 }         // false
(x => x * 2) === (x => x * 2)   // false
```

What is happening in these examples is that JavaScript does not check for _structural equality_ of objects using the `===` operator. It instead checks for _identity equality_ on objects.

In JavaScript all object values are held by reference, are assigned and passed by reference-copy, and are compared by reference (identity) equality:

```javascript
var x = [ 1, 2, 3 ];

// assignment is by reference-copy, so
// y references the *same* array as x,
// not another copy of it.
var y = x;

y === x;              // true
y === [ 1, 2, 3 ];    // false
x === [ 1, 2, 3 ];    // false
```

In this example `y === x` returns `true` because both variables hold reference to the same initial array. Both `=== [1, 2, 3]` comparisons return `false` because `y` and `x` are being compared to a newly created array `[1, 2, 3]`. The array structure and contents do not matter for comparison, only the **reference identity** does.

JavaScript does not provide a mechanism for structural equality comparison of object values. Structural equivalency is a difficult problem (for example: how can it be determined that two functions are structurally equivalent?). It is too difficult to account for all possible corner cases.

#### Coercive Comparisons
Coercion is when a value of one type is coverted to its respective representation in another type to whatever extent possible.

Few JS features draw more ire from the JS community than the `==` operator, referred to as the "loose eqaulity" operator. Most writing and discourse condemns this operator as poorly designed, dangerous, and bug-ridden. Even the creator of JS, Brendan Eich, has lamented how it was designed as a big mistake.

Most of this frustration comes from a short list of corner cases and a widespread misconception that it performs comparisons without considering the types of its compared values. In fact `==` and `===` boh consider the type of the values being compared, and when the comparison is between values of the same type both operators do **do exactly the same thing**. If the values are of different types, `==` differs from `===` in that it coerces the values to be the same type before performing the comparison. Instead of "loose equality" the `==` operator should be described as the "coercive equality" operator.

```javascript
42 == "42";             // true
1 == true;              // true
```

In both of the above comparisons the value types are different so `==` causes non-number values (`"42"` and `true`) to be converted to numbers (`42` and `1` respectively) before the comparison is made. `==` prefers numeric comparisons.

Similarly, the relational comparison operators like `<`, `>`, `<=`, and `>=` will allow coercion (generally, to numbers) if the types of values being compared differ.

```javascript
var arr = [ "1", "10", "100", "1000" ];
for (let i = 0; i < arr.length && arr[i] < 500; i++) {
  // will run 3 times
}
```

`i < arr.length` will not deal with coercion because both values will always be numbers. `arr[i] < 500` will invoke coercion, however, because all of the elements of `arr` are string values.

Relational comparisons typically use numeric comparisons except in cases where both values in the comparison are already strings - in this case they use alphabetical (dictionary-like) comparison of strings:

```javascript
var x = "10";
var y = "9";

x < y;      // true, watch out!
```

There is no way to get relational operators to avoid coercion besides never using mismatched types in comparisons. It is still very likely these cases will happen. Rather than avoiding coercive comparisons it is better to embrace them and learn the nuances of their behaviors. Coercive comparisons will show up in other operations in JavaScript (such as conditionals like `if`).

[▲ Return to Sections](#sections)

## How We Organize in JS
Two major patterns of organizing code (data and behavior) are used broadly across JS: classes and modules. These patterns are not mutually exclusive - programs can use one, both, or neither.

In some respects these patterns are very different but in other ways they are just different sides of the same coin. Being proficient in JavaScript requires understanding both patterns and where to use each.

#### Classes
The terms "object-oriented", "class-oriented", and "classes" are full of nuance - they are not universal in definition. This discussion will use a somewhat traditional definition: the one familiar to those to those with backgrounds in "object-oriented" languages like C++ and Java.

A class is a definition of a "type" of custom data structure that includes both data and the behaviors that operate on the data. Classes define how this data structure works but the classes are not themselves concrete values. To get a concrete value that can be used in a program a class must be _instantiated_ with the `new` keyword.

```javascript
class Page {
  constructor(text) {
    this.text = text;
  }

  print() {
    console.log(this.text);
  }
}

class Notebook {
  constructor() {
    this.pages = [];
  }

  addPage(text) {
    var page = new Page(text);
    this.pages.push(page);
  }

  print() {
    for (let page of this.pages) {
      page.print();
    }
  }
}

var mathNotes = new Notebook();
mathNotes.addPage("Arithmetic: + - * / ...");
mathNotes.addPage("Trigonometry: sin cos tan ...");

mathNotes.print();
// ..
```

The `this.text` member property of the `Page` class is where the string data is stored. The behavior is the class method `Page.print()` that dumps the string data to the console. The data in the `Notebook` class is an array of `Page` instances. The `Notebook.addPage(..)` method instantiates a new `Page` instance and adds it to the list. The `Notebook.print()` methods logs all of the pages of the Notebook instance to the console.

The statement `mathNotes = new Notebook()` creates a new instance of the `Notebook` class and `page = new Page(text)` is where instances of the `Page` class are created.

Methods of a class can only be called on through instances of the class, and not the classes themselves: `mathNotes.addPage(..)`, `page.print()`.

The `class` mechanism allows for data to be organized together with behaviors associated with that data. This same program could have been built without any `class` definitions but it would likely have been less organized and more difficult to reason about.

#### Class Inheritance
An aspect of traditional "class-oriented" design less commonly used in JavaScript is [inheritance](https://en.wikipedia.org/wiki/Inheritance_(object-oriented_programming) (and [polymorphism](https://en.wikipedia.org/wiki/Polymorphism_(computer_science)).

Considering the following class:

```javascript
class Publication {
  constructor(title,author,pubDate) {
    this.title = title;
    this.author = author;
    this.pubDate = pubDate;
  }

  print() {
    console.log(`
      Title: ${ this.title }
      By: ${ this.author }
      ${ this.pubDate }
    `);
  }
}
```

The above `Publication` class defines a set of common behavior that any publication might need.

Now consider more specific types of publications, such as `Book` or `BlogPost`:

```javascript
class Book extends Publication {
  constructor(bookDetails) {
    super(
      bookDetails.title,
      bookDetails.author,
      bookDetails.publishedOn
    );
    this.publisher = bookDetails.publisher;
    this.ISBN = bookDetails.ISBN;
  }

  print() {
    super.print();
    console.log(`
      Publisher: ${ this.publisher }
      ISBN: ${ this.ISBN }
    `);
  }
}

class BlogPost extends Publication {
  constructor(title,author,pubDate,URL) {
    super(title,author,pubDate);
    this.URL = URL;
  }

  print() {
    super.print();
    console.log(this.URL);
  }
}
```

Both the `Book` and `BlogPost` classes use the `extends` clause to extend the general definition of `Publication` to include additional behavior. The `super(..)` call in each constructor delegates to the parent `Publication` class's constructor for initialization before doing more initialization specific to their own sub-class/child class.

To use these child classes:

```javascript
var YDKJS = new Book({
  title: "You Don't Know JS",
  author: "Kyle Simpson",
  publishedOn: "June 2014",
  publisher: "O'Reilly",
  ISBN: "123456-789"
});

YDKJS.print();
// Title: You Don't Know JS
// By: Kyle Simpson
// June 2014
// Publisher: O'Reilly
// ISBN: 123456-789

var forAgainstLet = new BlogPost(
  "For and against let",
  "Kyle Simpson",
  "October 27, 2014",
  "https://davidwalsh.name/for-and-against-let"
);

forAgainstLet.print();
// Title: For and against let
// By: Kyle Simpson
// October 27, 2014
// https://davidwalsh.name/for-and-against-let
```

Both of the child classes have a `print()` method which is an override of the inherited `print()` method from the parent `Publication` class. Each of these `print()` methods call `super.print()` to invoke the inherited version of the `print()` method. That both the inherited and overriden methods can have the same name is called _polymorphism_.

Inheritance is a powerful pattern for organizing data/behavior into separate logical units (classes) while allowing child classes to utilize the parent class by accessing/using its behavior/data.

#### Modules
The module pattern has essentially the same goal as the class pattern: group data and behavior together into logical units. Like classes, modules can "include" and "access" the data and behaviors of other modules.

Modules do have some differences from classes, most notably syntax.

#### Classic Modules
ES6 added a native module syntax to JavaScript, but from the early days of JavaScript modules have been an important and common pattern leveraged across countless programs, even without a dedicated syntax.

The key hallmarks of a _classic module_ are an outer function that returns an "instance" of the module with one or more functions that can operate on the module instance's internal (hidden) data. Because a _classic module_ is just a function and calling it produces an "instance" of the module another term used to describe these functions is _module factories_.

The classic module form of the earlier class examples of `Publication`, `Book`, and `BlogPost`:

```javascript
function Publication(title,author,pubDate) {
  var publicAPI = {
    print() {
      console.log(`
        Title: ${ title }
        By: ${ author }
        ${ pubDate }
      `);
    }
  };

  return publicAPI;
}

function Book(bookDetails) {
  var pub = Publication(
    bookDetails.title,
    bookDetails.author,
    bookDetails.publishedOn
  );

  var publicAPI = {
    print() {
      pub.print();
      console.log(`
        Publisher: ${ bookDetails.publisher }
        ISBN: ${ bookDetails.ISBN }
      `);
    }
  };

  return publicAPI;
}

function BlogPost(title,author,pubDate,URL) {
  var pub = Publication(title,author,pubDate);

  var publicAPI = {
    print() {
      pub.print();
      console.log(URL);
    }
  };

  return publicAPI;
}
```

These modules share more similarities with their class counterparts than differences.

The class forms store methods and data on an object instance which are accessed using through the `this` keyword. With modules the data and methods are accessed as identifier variables in scope without the `this.` prefix.

With a class the API of an instance is implicit in the class definition (all of the data and methods are public). With a module factory function an object with publicly exposed data/methods is explicitly returned from the function, while any data or unreferenced methods remain private.

There are variations to this factory function pattern across JavaScript: AMD (Asynchronous Module Definition), UMD (Universal Module Definition), and CommonJS (classic Node.js-style modules) among them. All of these variations rely on the same basic principles.

Consider the usage ("instantiation") of the above module factory functions:

```javascript
var YDKJS = Book({
  title: "You Don't Know JS",
  author: "Kyle Simpson",
  publishedOn: "June 2014",
  publisher: "O'Reilly",
  ISBN: "123456-789"
});

YDKJS.print();
// Title: You Don't Know JS
// By: Kyle Simpson
// June 2014
// Publisher: O'Reilly
// ISBN: 123456-789

var forAgainstLet = BlogPost(
  "For and against let",
  "Kyle Simpson",
  "October 27, 2014",
  "https://davidwalsh.name/for-and-against-let"
);

forAgainstLet.print();
// Title: For and against let
// By: Kyle Simpson
// October 27, 2014
// https://davidwalsh.name/for-and-against-let
```

The only observable difference between these instantiations and the ones used for the classes is the lack of the `new` keyword. Instead the module factories are invoked as regular functions.

#### ES Modules
ES Modules were introduced to JavaScript with ES6. They were meant to serve the same spirit and purpose as the _classic modules_, taking into account important variations and use cases from AMD, UMD, and CommonJS. The implementation, however, differs significantly with classic modules.

First the wrapping context for an ES module is a file. ESMs are file-based: one file, one module. Secondly ES module APIs are not interacted with explicitly - the `export` keyword is used to add a variable or method to a module's public API. If something is defined in a module but not `export`ed, then it remains private to the module. Third: ES modules are not "instantiated", they are `import`ed to use their single instance. ES modules are, in effect, "singletons" - only one instance is ever created at the point of first `import` in the program runtime. Subsequent `import` statements simply receive a reference to that same single instance. If multiple instantiations are necessary a classic module factory function inside an ESM needs to be provided.

The following snippets mix both ESM and classic modules, since multiple instantiation is necessary:

```javascript
// file: publication.js

function printDetails(title,author,pubDate) {
  console.log(`
    Title: ${ title }
    By: ${ author }
    ${ pubDate }
  `);
}

export function create(title,author,pubDate) {
  var publicAPI = {
    print() {
      printDetails(title,author,pubDate);
    }
  };

  return publicAPI;
}
```

To import and use the above module from a different module:

```javascript
// file: blogpost.js
import { create as createPub } from "publication.js";

function printDetails(pub,URL) {
  pub.print();
  console.log(URL);
}

export function create(title,author,pubDate,URL) {
  var pub = createPub(title,author,pubDate);

  var publicAPI = {
    print() {
      printDetails(pub,URL);
    }
  };

  return publicAPI;
}
```

To use the above module `blogpost.js`, import it into another module like `main.js`:

```javascript
// file: main.js
import { create as newBlogPost } from "blogpost.js";

var forAgainstLet = newBlogPost(
  "For and against let",
  "Kyle Simpson",
  "October 27, 2014",
  "https://davidwalsh.name/for-and-against-let"
);

forAgainstLet.print();
// Title: For and against let
// By: Kyle Simpson
// October 27, 2014
// https://davidwalsh.name/for-and-against-let
```

**Note**: `as newBlogPost` clause in the `import` statement is optional. If omitted the function will just be named `create` in this scope. Renaming a generic method like like `create` to contain more context as to its function can greatly improve readability.

ES modules can use classic modules internally if they need to support multiple-instantiation. Alternatively a `class` defintion could have been exposed instead. If a module only needs a single instance, its methods/properties can be `export`ed directly instead.

[▲ Return to Sections](#sections)

| [Previous: Chapter 1 - What is JavaScript?](../01/README.md) | [Table of Contents](../README.md#table-of-contents) | [Next: Chapter 3 - Digging to the Roots of JS](../03/README.md) |
