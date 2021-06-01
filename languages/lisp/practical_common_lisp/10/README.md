# Numbers, Characters, and Strings
[Chapter Link](https://gigamonkeys.com/book/numbers-characters-and-strings.html)

[Fred Brooks](https://en.wikipedia.org/wiki/Fred_Brooks) observed in _The Mythical Man-Month_ "Representation _is_ the essence of programming."

Common Lisp provides built-in support for most of the data types typically found in modern languages: numbers (integer, floating point, and complex), characters, strings, arrays, lists, hash tables, input and output streams, and an abstraction for portably representing filenames. Functions are also a first-class data type in Lisp.

From the language user point of view the built-in data types are defined by the functions that operate on them.

In this chapter the built-in "scalar" types will be covered: numbers, characters, and strings. Technically strings aren't scalars (individual characters can be accessed and functions that operate on sequences can be applied to strings), but they will be discussed because most of the string-specific functions manipulate them as single values.

## Sections
* [Numbers](#numbers)

[◂ Return to Table of Contents](../README.md)

## Numbers

[▲ Return to Sections](#sections)

| [Previous: Building a Unit Test Framework](../09/README.md) | [Table of Contents](../README.md#notes) | Next |
