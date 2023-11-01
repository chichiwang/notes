# Chapter 3: Digging to the Roots of JS
This chapter covers some of the lower-level root characteristics of the JavaScript language.

## Sections
* [Iteration](#iteration)

[◂ Return to Table of Contents](../README.md)

## Iteration
The patterns used to step through data have a big impact on a program's readability. The iterator pattern has been around for decades - it suggests a standardized approach to consuming data from a source one chunk at a time.

The iterator pattern defines a data structure called an _iterator_, that has reference to an underlying data source, which exposes a method like `next()`. Calling `next()` returns the next piece of data from that source. The pattern typically indicates completion of the data set by some special value or exception once it has reached or passed the end of the set.

Standardizing the pattern of proccessing data iteratively is important: it creates easier to understand code (as opposed to have every data structure/source define its own custom approach to handling data). ES6 standardized a specific protocol for the iterator pattern directly in the language. The protocol defines a `next()` method whose return object has `value` and `done` properties. `done` is a boolean whose value is `false` until the iteration over the underlying data source is complete.

[▲ Return to Sections](#sections)

| [Previous: Chapter 2 - Surveying JS](../02/README.md) | [Table of Contents](../README.md#table-of-contents) |
