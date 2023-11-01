# Chapter 3: Digging to the Roots of JS
This chapter covers some of the lower-level root characteristics of the JavaScript language.

## Sections
* [Iteration](#iteration)
  * [Consuming Iterators](#consuming-iterators)
  * [Iterables](#iterables)

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

```javscript
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

For the most part all built-in iterables in JavaScript have three iterator forms avaiable:
* `keys()`: keys only
* `values()`: values only
* `entries()`: entries in the form of a tuple

Custom data structures can also be made in a way they adhere to the iteration protocol: this allows them to used by iterator consumers (such as `for .. of` and `...`).

**Note**: A nuance of _iterators_ is that they are in and of themselves _iterables_. The iterable-consumption protocol expects an _iterable_ but an _iterator_ is just an iterable of itself. When creating an iterator instance from an iterator, the iterator itself is returned.

[▲ Return to Sections](#sections)

| [Previous: Chapter 2 - Surveying JS](../02/README.md) | [Table of Contents](../README.md#table-of-contents) |
