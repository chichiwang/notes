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
    * [Floating-Point Numbers](#floating-point-numbers)
    * [Complex Numbers](#complex-numbers)
  * [Basic Math](#basic-math)

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

An optional prefix can be applied to rational numbers to denote the numeral base of the digits:
* `#b`/`#B`: Binary (the only legal digits being 0 and 1)
* `#o`/`#O`: Octal (legal digits 0-7)
* `#x`/`#X`: Hexadecimal (legal digits 0-F or 0-f)
* `#nr`/`#nR`: Custom base where `n` is the base (always written in decimal). Additional digits beyond 9 are taken from the letters A-Z, a-z. These radix indicators apply to the whole rational - it is not possible to write a ratio where the numerator is in one base and the denonminator in another.

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


#### Floating-Point Numbers

Unlike rational numbers, the syntax used to denote a floating-point number can affect the actual type of number read. Common Lisp denotes four subtypes of floating-point number: _short_, _single_, _double_, and _long_. Each subtype can use a different number of bits in its representation, meaning each subtype can represent values spanning a different range and with different precision.

The basic format for floating-point numbers is an optional sign followed by a nonempty sequence of decimal digits possibly with an embedded decimal point. This sequence can be followed by an exponent marker for _computerized scientific notation_. The exponent marker consists of a single letter followed by an optional sign and a sequence of digits (the power of ten by which the number before the exponent marker should be multiplied). The exponent marker marks the beginning of the exponent and indicates what floating-point representation should be used for the number.

The exponent markers are:
* `s`/`S`: Short
* `f`/`F`: Single
* `d`/`D`: Double
* `l`/`L`: Long
* `e`/`E`: Default (initially single-float)

Numbers with no exponent marker are read in the default representation and must contain a decimal point followed by at least one digit to distinguish them from inteers. The digits in a floating-point number are always treated as base-10 digits. The `#B`, `#X`, `#O`, `#R` prefixes work only with rational values.

**Some examples of floating-point numbers**, along with their canonical represenation:

| Textual syntax | Canonical representation |
| -------------- | ------------------------ |
| 1.0            | 1.0                      |
| 1e0            | 1.0                      |
| 1d0            | 1.0d0                    |
| 123.0          | 123.0                    |
| 123e0          | 123.0                    |
| 0.123          | 0.123                    |
| .123           | 0.123                    |
| 123e-3         | 0.123                    |
| 123E-3         | 0.123                    |
| 0.123e20       | 1.23e+19                 |
| 123d23         | 1.23d+25                 |

#### Complex Numbers
Complex numbers are written their own syntax: `#c`/`#C` followed by a list of two real numbers representing the real and imaginary part of the complex number.

There are five kinds of complex numbers. The real and imaginary parts must either be both rational or both be the same kind of floating point number. Complex numbers can be written a number of ways: if written with one rational and one floating-point part, the rational is converted to a float of the appropriate representation. If the real and imaginary parts are both floats of different representations, the one in the smaller representation is converted to match the larger representation.

No complex numbers have a rational real component and a zero imaginary part. Such values are, mathematically speaking, rational and they are represented by the appropriate rational value. However, a complex number with a floating-point component and a zero imaginary part is represented as a different object than the floating-point number representing the real component.

**Some examples of complex numbers**, along with their canonical representations:

| Textual syntax | Canonical representation |
| -------------- | ------------------------ |
| #c(2        1) | #c(2 1)                  |
| #c(2/3    3/4) | #c(2/3 3/4)              |
| #c(2      1.0) | #c(2.0 1.0)              |
| #c(2.0  1.0d0) | #c(2.0d0 1.0d0)          |
| #c(1/2    1.0) | #c(0.5 1.0)              |
| #c(3        0) | 3                        |
| #c(3.0    0.0) | #c(3.0 0.0)              |
| #c(1/2      0) | 1/2                      |
| #c(-6/3     0) | -2                       |

[▲ Return to Sections](#sections)

### Basic Math
Basic arithmetic operations are supported for all of the different types of numbers in Lisp. These functions can be called with any number of arguments.

* `+`: Addition
* `-`: Subtraction
* `*`: Multiplication
* `/`: Division

| Expression         | Result  |
| ------------------ | ------- |
| (+ 5)              | 5       |
| (+ 1 2)            | 3       |
| (+ 1 2 3)          | 6       |
| (+ 10.0 3.0)       | 13.0    |
| (+ #c(1 2) #c(3 4) | #c(4 6) |
| (- 2)              | -2      |
| (- 5 4)            | 1       |
| (- 10 3 5)         | 2       |
| (* 2)              | 2       |
| (* 2 3)            | 6       |
| (* 2 3 4)          | 24      |
| (/ 4)              | 1/4     |
| (/ 10 5)           | 2       |
| (/ 10 5 2)         | 1       |
| (/ 2 3)            | 2/3     |

If all of the arguments are the same type of number (rational, floating-point, complex) then the result will be the same type, except in the case where the result of an operation on complex numbers with rational components yields a number with a zero imaginary part (in which case the result will be rational).

Floating-point and complex numbers are _contiguous_ meaning if all of the arguments are reals but one or more are floating-point values, the other arguments are converted to the nearest floating-point representation of the actual floating-point arguments. Floating-point numbers of a smaller representation are converted to the larger representation. If any of the arguments are complex, any real numbers are converted to the complex equivalents.

| Expression           | Result    |
| -------------------- | --------- |
| (+ 1 2.0)            | 3.0       |
| (/ 2 3.0)            | 0.6666667 |
| (+ #c(1 2) 3)        | #c(4 2)   |
| (+ #c(1 2) 3/2)      | #c(5/2 2) |
| (+ #c(1 1) #c(2 -1)) | 3         |

Because `/` doesn't truncate, Common Lisp provides four flavors of truncating and rounding for converting a real number (rational or floating-point) to an integer:
* `FLOOR`: Truncates towards negative infinity, returning the largest integer less than or equal to the argument.
* `CEILING`: Truncates towards positive infinity, returning the smallest integer greater than or equal to the argument.
* `TRUNCATE`: Truncates towards zero, behaving like `FLOOR` for positive arguments and `CEILING` for negative arguments.
* `ROUND`: Rounds to the nearest integer. If the argument is exactly halfway between two integers it rounds to the nearest even integer.

Two additional functions that are related:
* `MOD`: Returns the modulus of a truncating division on real numbers.
* `REM`: Returns the remainder of a truncating division on real numbers.

For positive quotients `MOD` and `REM` return the same results, but for negagive quotients they return different results.

Shorthand functions:
* `1+`: Add by one and return the result.
* `1-`: Subtract by one and return the result.
* `INCF`: Increment a value stored in a variable by a specified amount (default 1).
* `DECF`: Decrement a value stored in a variable by a specified amount (default 1).

```lisp
(incf x)    === (setf x (1+ x)) === (setf x (+ x 1))
(decf x)    === (setf x (1- x)) === (setf x (- x 1))
(incf x 10) === (setf x (+ x 10))
(decf x 10) === (setf x (- x 10))
```

[▲ Return to Sections](#sections)

| [Previous: Building a Unit Test Framework](../09/README.md) | [Table of Contents](../README.md#notes) | Next |