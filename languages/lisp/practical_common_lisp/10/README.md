# Numbers, Characters, and Strings
[Chapter Link](https://gigamonkeys.com/book/numbers-characters-and-strings.html)

[Fred Brooks](https://en.wikipedia.org/wiki/Fred_Brooks) observed in _The Mythical Man-Month_ "Representation _is_ the essence of programming."

Common Lisp provides built-in support for most of the data types typically found in modern languages: numbers (integer, floating point, and complex), characters, strings, arrays, lists, hash tables, input and output streams, and an abstraction for portably representing filenames. Functions are also a first-class data type in Lisp.

From the language user point of view the built-in data types are defined by the functions that operate on them.

In this chapter the built-in "scalar" types will be covered: numbers, characters, and strings. Technically strings aren't scalars (individual characters can be accessed and functions that operate on sequences can be applied to strings), but they will be discussed because most of the string-specific functions manipulate them as single values.

## Sections
* [Numbers](#numbers)
  * [Numeric Literals](#numeric-literals)
    * [Rational Numbers](#rational-numers)

[◂ Return to Table of Contents](../README.md)

## Numbers
Lisp was originally designed by a mathematician as a tool for studying mathematical functions. One of the reasons Lisp is a nice language for math is its numbers behave more like true mathematical numbers than the approximations of numbers that are easy to implement in finite computer software.

Integers in Common Lisp can be almost arbitrarily large rather than being limited by the size of a machine word. Dividing by integers results in an exact ratio, not a truncated value. Since ratios are represented as pairs of arbitrarily sized integers, rations can represent arbitrarily precise fractions.

Common Lisp also offers several types of floating-point numbers, which are mapped by the implementation to the appropriate hardware-supported floating-point representations. Floats are also used to represent the results of computations whose mathematical value would be an irrational number.

Common Lisp supports complex numbers as well - the numbers that result from operations such as taking square roots and logarithms of negative numbers. The Common Lisp standard even specifies the principal values and branch cuts for irrational and transcendental functions on the complex domain.

[▲ Return to Sections](#sections)

### Numeric Literals
There is a division of labor between the Lisp reader and the Lisp evaluator in dealing with numeric literals: the reader is responsible for translating text into Lisp objects, the evaluator then deals only with those objects. For a given number type there can be many different textual representations, all of which will be translated into the same object representation by the Lisp reader. A given integer `10` can be written as `10`, `20/2`, `#xA`, or any number of other ways but the reader will translate all of these text representations to the same object. When numbers are printed back out (say in the REPL) they're printed in a canonical textual syntax:

```console
CL-USER> 10
10
CL-USER> 20/2
10
CL-USER> #xa
10
CL-USER>
```

#### Rational Numbers

The syntax for for integer values is an optional sign (`+` or `-`) followed by one or more digits. Ratios are written as an optional sign and a sequence of numbers (representing the numerator), a slash (`/`), and another sequence of numbers representing the denonminator. All rational numbers are _canonicalized_ as they're read (`10` and `20/2` are the same number, so are `3/4` and `6/8`). Rational numbers are printed in _reduced_ form, integers are printed in integer syntax, ratios are printed with the numerator and denonminator reduced to lowest terms.

If preceded by a `#B` or `#b`, a rational literal is read as a binary number with `0` and `1` being the only legal digits. A `#O` or `#o` prefix denotes an octal value (legal digits 0-7). `#X` or `#x` denotes a hexadecimal value (legal digits 0-F or 0-f).

Rationals can be written in other bases using the prefix `#nR` where `n` is the base (always written in decimal). Additional digits beyond 9 are taken from the letters A-Z or a-z. These radix indicators apply to the whole rational - it is not possible to write a ratio where the numerator is in one base and the denonminator in another.

Integer values, but not ratios, can also be terminated with a decimal point.

**Some examples of rationals**, along with their canonical decimal values:

| Textual syntax                 | Canonical representation                 |
| ------------------------------ | ---------------------------------------- |
| 123                            | 123                                      |
| +123                           | 123                                      |
| -123                           | -123                                     |
| 123.                           | 123                                      |
| 2/3                            | 2/3                                      |
| -2/3                           | -2/3                                     |
| 4/6                            | 2/3                                      |
| 6/3                            | 2                                        |
| #b10101                        | 21                                       |
| #b1010/1011                    | 10/11                                    |
| #o777                          | 511                                      |
| #xDADA                         | 56026                                    |
| #36rABCDEFGHIJKLMNOPQRSTUVWXYZ | 8337503854730415241050377135811259267835 |

[▲ Return to Sections](#sections)

| [Previous: Building a Unit Test Framework](../09/README.md) | [Table of Contents](../README.md#notes) | Next |
