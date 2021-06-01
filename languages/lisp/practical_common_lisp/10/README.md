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
Lisp was originally designed by a mathematician as a tool for studying mathematical functions. One of the reasons Lisp is a nice language for math is its numbers behave more like true mathematical numbers than the approximations of numbers that are easy to implement in finite computer software.

Integers in Common Lisp can be almost arbitrarily large rather than being limited by the size of a machine word. Dividing by integers results in an exact ratio, not a truncated value. Since ratios are represented as pairs of arbitrarily sized integers, rations can represent arbitrarily precise fractions.

Common Lisp also offers several types of floating-point numbers, which are mapped by the implementation to the appropriate hardware-supported floating-point representations. Floats are also used to represent the results of computations whose mathematical value would be an irrational number.

Common Lisp supports complex numbers as well - the numbers that result from operations such as taking square roots and logarithms of negative numbers. The Common Lisp standard even specifies the principal values and branch cuts for irrational and transcendental functions on the complex domain.

[▲ Return to Sections](#sections)

| [Previous: Building a Unit Test Framework](../09/README.md) | [Table of Contents](../README.md#notes) | Next |
