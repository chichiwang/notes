# Chapter 3: Digging to the Roots of JS
This chapter covers some of the lower-level root characteristics of the JavaScript language.

## Sections
* [Iteration](#iteration)
  * [Consuming Iterators](#consuming-iterators)

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

[▲ Return to Sections](#sections)

| [Previous: Chapter 2 - Surveying JS](../02/README.md) | [Table of Contents](../README.md#table-of-contents) |
