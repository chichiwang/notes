# Chapter 3: Digging to the Roots of JS
This chapter covers some of the lower-level root characteristics of the JavaScript language.

## Sections
* [Iteration](#iteration)
  * [Consuming Iterators](#consuming-iterators)
  * [Iterables](#iterables)
* [Closures](#closures)
* [this Keyword](#this-keyword)
* [Prototypes](#prototypes)
  * [Object Linkage](#object-linkage)
  * [this Revisited](#this-revisited)

[◂ Return to Table of Contents](../README.md)

## Iteration
The patterns used to step through data have a big impact on a program's readability. The iterator pattern has been around for decades - it suggests a standardized approach to consuming data from a source one chunk at a time.

The iterator pattern defines a data structure called an _iterator_, that has reference to an underlying data source, which exposes a method like `next()`. Calling `next()` returns the next piece of data from that source. The pattern typically indicates completion of the data set by some special value or exception once it has reached or passed the end of the set.

Standardizing the pattern of proccessing data iteratively is important: it creates easier to understand code (as opposed to have every data structure/source define its own custom approach to handling data). ES6 standardized a specific protocol for the iterator pattern directly in the language. The protocol defines a `next()` method whose return object has `value` and `done` properties. `done` is a boolean whose value is `false` until the iteration over the underlying data source is complete.

#### Consuming Iterators
Although ES6 has standardized iterators, ES6 has also included several mechanisms (syntax and APIs) for standardized consumption of these iterators.

One of these mechanisms is the `for .. of` loop:

```javascript
// given an iterator of some data source:
var it = /* .. */;

// loop over its results one at a time
for (let val of it) {
  console.log(`Iterator value: ${ val }`);
}
// Iterator value: ..
// Iterator value: ..
// ..
```

The manual loop equivalent (not demonstrated here) is less readable than this `for .. of` loop.

Another mechanism used for consuming iterators is the `...` operator. This operator has two symmetrical forms: _spread_ and _rest_ (or _gather_).

The _spread_ operator is an iterator-consumer. There are two places to spread a data collection into: an array or an argument list for a function call:

```javascript
// spread an iterator into an array,
// with each iterated value occupying
// an array element position.
var vals = [ ...it ];

// spread an iterator into a function,
// call with each iterated value
// occupying an argument position.
doSomethingUseful( ...it );
```

In both of these cases the iterator-spread form of `...` follows the iterator-consumption protocol (the same as the `for .. of` loop) to retrieve all available values from and iterator and place them into the receiving context.

#### Iterables
The iterator-consumption protocol is defined for consuming _iterables_: values that can be iterated over. The protocol automatically creates an instance from an iterable and consumes just that _iterator instance_ to completion. A single iterable can be consumed more than once - a new iterator instance is created from it each time.

ES6 defined the basic data structure/collection types in JS as iterables, including strings, arrays, maps, sets, and others.

Taking an array as an example of an iterable:

```javascript
// an array is an iterable
var arr = [ 10, 20, 30 ];

for (let val of arr) {
  console.log(`Array value: ${ val }`);
}
// Array value: 10
// Array value: 20
// Array value: 30
```

Since arrays are iterables, they can be shallow-copied with the spread operator:

```javascript
var arrCopy = [ ...arr ];
```

The characters of a string can also be iterated over:

```javascript
var greeting = "Hello world!";
var chars = [ ...greeting ];

chars;
// [ "H", "e", "l", "l", "o", " ",
//   "w", "o", "r", "l", "d", "!" ]
```

A [map](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map) data structure uses objects as keys, associating a value of any type with that key. Maps have a different default iteration: the iteration runs over the map's _entries_ in the form of a tuple (2-element array) containing both the key and value.

```javascript
// given two DOM elements, `btn1` and `btn2`

var buttonNames = new Map();
buttonNames.set(btn1,"Button 1");
buttonNames.set(btn2,"Button 2");

for (let [btn,btnName] of buttonNames) {
  btn.addEventListener("click",function onClick(){
    console.log(`Clicked ${ btnName }`);
  });
}
```

In the `for .. of` loop over `buttonNames`, `let [btn, btnName]` is used to assign the entry values to `btn` and `btnName` (called _array destructuring_).

Each built-in iterable in JavaScript exposes a default iteration. A more specific iteration can be chosen if necessary. For example, to consume only the values of a map, the `.values()` method can be called on it:

```javascript
for (let btnName of buttonNames.values()) {
  console.log(btnName);
}
// Button 1
// Button 2
```

To consume both the index and value of array entries, the `.entries()` method can be called:

```javascript
var arr = [ 10, 20, 30 ];

for (let [idx,val] of arr.entries()) {
  console.log(`[${ idx }]: ${ val }`);
}
// [0]: 10
// [1]: 20
// [2]: 30
```

For the most part all built-in iterables in JavaScript have three iterator forms available:
* `keys()`: keys only
* `values()`: values only
* `entries()`: entries in the form of a tuple

Custom data structures can also be made in a way they adhere to the iteration protocol: this allows them to used by iterator consumers (such as `for .. of` and `...`).

**Note**: A nuance of _iterators_ is that they are in and of themselves _iterables_. The iterable-consumption protocol expects an _iterable_ but an _iterator_ is just an iterable of itself. When creating an iterator instance from an iterator, the iterator itself is returned.

[▲ Return to Sections](#sections)

## Closures
_Closure_ is one of the most pervasive programming functionalities across a majority of programming languages. It is a concept as fundamental as variables or loops.

A formal definition:
> Closure is when a function remembers and continues to access variables outside of its scope, even when the function is executed in a different scope.

Two characteristics found in this definition:
1. Closure is part of the nature of a function. Objects do not get closures, functions do.
2. To observe a closure you must execute a function in a different scope than where that function was originally defined.

Taking the following as example:

```javascript
function greeting(msg) {
  return function who(name) {
    console.log(`${ msg }, ${ name }!`);
  };
}

var hello = greeting("Hello");
var howdy = greeting("Howdy");

hello("Kyle");
// Hello, Kyle!

hello("Sarah");
// Hello, Sarah!

howdy("Grant");
// Howdy, Grant!
```

When the outer function `greeting(..)` is executed it returns an instance of the inner function `who(..)`. The instance of `who(..)` returned closes over the variable `msg`, the parameter from the outer function.

When `greeting(..)` is finished running, normally all of its variables would be garbage collected (removed from memory). However, `msg` persists due to closure, and will remain in memory until the instance of `who(..)` hanging onto that closure (`hello(..)` or `howdy(..)`) are no longer in memory.

Closures are not a snapshot of values, they are a direct link and preservation of the varibles themselves. This means closures can actually observe and make changes to the variables over time:

```javascript
function counter(step = 1) {
  var count = 0;
  return function increaseCount(){
    count = count + step;
    return count;
  };
}

var incBy1 = counter(1);
var incBy3 = counter(3);

incBy1();       // 1
incBy1();       // 2

incBy3();       // 3
incBy3();       // 6
incBy3();       // 9
```

Each instance of `increaseCount()` is closed over both the `step` and `count` variables from the outer function's scope. `count` is updated on each invocation of that returned function.

Closure is most common when working with asynchronous code:

```javascript
function getSomeData(url) {
  ajax(url,function onResponse(resp){
    console.log(
      `Response (from ${ url }): ${ resp }`
    );
  });
}

getSomeData("https://some.url/wherever");
// Response (from https://some.url/wherever): ...
```

The inner function `onResponse(..)` is closed over `url` and preserves that variable until the Ajax call returns and executes the callback.

It isn't necessary for the outer scope to be a function, just that there be one variable in an outer scope is accessed from an inner function:

```javascript
for (let [idx,btn] of buttons.entries()) {
  btn.addEventListener("click",function onClick(){
    console.log(`Clicked on button (${ idx })!`);
  });
}
```

Because the `for` loop is using `let` declarations, each iteration gets new block-scoped `idx` and `btn` variables, as well as an new `onClick()` function. The inner `onClick()` function in each iteration closes over `idx`.

It is important to remember that the inner functions close over the variables and not the values they contain.

[▲ Return to Sections](#sections)

## `this` Keyword
One of JavaScript's most powerful mechanisms is also its most misunderstood: the `this` keyword.

> Scope is the set of rules that controls how references to variables are resolved.

Functions have another characteristic besides scope that influences what they can access: the _execution context_, which is exposed to the function via the `this` keyword.

Scope is static and contains a fixed set of variables available at the moment and location where a function is defined. A function's _execution context_ is dynamic and depedent on how it is called. `this` is a dynamic characteristic determined each time a function is called.

One way to think about execution context is that it is a tangible object whose properties are made available to a function while it executes. Scope can also be thought about as an object, but it is hidden within the JS engine, whose properties are identifier variables available inside the function.

```javascript
function classroom(teacher) {
  return function study() {
    console.log(
      `${ teacher } says to study ${ this.topic }`
    );
  };
}

var assignment = classroom("Kyle");
```

The inner function `study()` refers to a `this` keyword which makes it a `this`-aware function, or a function dependent on its execution context. If `assignment()` is called normally:

```javascript
assignment();
// Kyle says to study undefined  -- Oops :(
```

Because the program is not running in [strict mode](../01/README.md#strictly-speaking), context-aware functions called without a context specified default the context to the global object (`window` in the browser). Given there is no global variable `topic`, `this.topic` resolves to `undefined`.

To provide a context, a function can be assigned as a property of an object:

```javascript
var homework = {
  topic: "JS",
  assignment: assignment
};

homework.assignment();
// Kyle says to study JS
```

This time `assignment()` is assigned as a property of the `homework` object and invoked as `homework.assignment()` - `this` in the function execution now references the `homework` object and `this.topic` resolves as `"JS"`.

A context-aware function can also be provided a context explicitly in its invocation:

```javascript
var otherHomework = {
  topic: "Math"
};

assignment.call(otherHomework);
// Kyle says to study Math
```

This time the function `assignment()` was invoked with its `call(..)` method, which takes a context object as argument. In this example the context object is `otherHomework` and `this.topic` resolves to `"Math"`.

The benefit of context-aware functions is the ability to more flexibly re-use a single function with data from different objects. A function that closes over a scope can never reference a different scope, but a function with dynamic `this` context awareness can be useful in certain situations.

[▲ Return to Sections](#sections)

## Prototypes
The same way that the `this` keyword is a characteristic of function execution, a prototype is a characteristic of an object (specifically: resolution of property access).

A prototype can be thought of as a linkage between two objects. This linkage is created when an object is created and links the new object to another object that already exists. A series of objects linked together via prototypes is known as a "prototype chain".

The purpose of this prototype chain is to delegate access of properties and methods that an object does not have up the chain. Consider this object literal definition:

```javascript
var homework = {
  topic: "JS",
};
```

While `homework` is only defined with a single property `topic`, the default prototype linkage connects it to the `Object.prototype` object which contains common built-in methods like `toString()`, `valueOf()`, etc. This prototype delegation can be observed by accessing a property from an object's prototype:

```javascript
homework.toString();    // [object Object]
```

`homework.toString()` is a valid property access even though the `homework` object does not have a defined `toString()` method, the access delegates invocation to `Object.prototype.toString()` instead.

#### Object Linkage

To explicitly define an object prototype linkage an object can be created using the `Object.create(..)` utility:

```javascript
var homework = {
  topic: "JS"
};

var otherHomework = Object.create(homework);

otherHomework.topic;   // "JS"
```

`Object.create(..)` takes as argument an object to link to and returns an object linked to the specified object. Passing in `null` creates a standalone object with no prototype link.

Delegation through the prototype chain only applies for accesses to look up the value of a property - it does not delegate property assignments:

```javascript
homework.topic;
// "JS"

otherHomework.topic;
// "JS"

otherHomework.topic = "Math";
otherHomework.topic;
// "Math"

homework.topic;
// "JS" -- not "Math"
```

`otherHomework.topic = "Math"` assigns the value `"Math"` to a new property `topic` directly on the `otherHomework` object, not to any objects further up in the chain. `homework.topic` remains unaffected by the assignment. The `topic` property on `otherHomework` is _shadowing_ the property of the same name on `homework` in the prototype chain.

#### `this` Revisited
The true importance of the `this` keyword is seen in how it powers prototype-delegated function calls. One of the main reasons `this` supports dynamic context is so that method calls on objects which delegate methods maintain the expected context for method execution.

```javascript
var homework = {
  study() {
    console.log(`Please study ${ this.topic }`);
  }
};

var jsHomework = Object.create(homework);
jsHomework.topic = "JS";
jsHomework.study();
// Please study JS

var mathHomework = Object.create(homework);
mathHomework.topic = "Math";
mathHomework.study();
// Please study Math
```

Both objects `jsHomework` and `mathHomework` delegate the `study()` property to the `homework` object. Both objects `jsHomework` and `mathHomework` have their own `topic` property - when they delegate the invocation of the `study()` method, it is invoked with the context of the object it was called from.

JavaScript's dynamic `this` is a critical component of the functioning of prototype delegation and of `class`.

[▲ Return to Sections](#sections)

| [Previous: Chapter 2 - Surveying JS](../02/README.md) | [Table of Contents](../README.md#table-of-contents) |
