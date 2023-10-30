# Chapter 2: Surveying JS
The best way to learn JS is to start writing JS.

## Sections
* [Each File is a Program](#each-file-is-a-program)
* [Values](#values)
* [Arrays and Objects](#arrays-and-objects)

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

| [Previous: Chapter 1 - What is JavaScript?](../01/README.md) | [Table of Contents](../README.md#table-of-contents) |
