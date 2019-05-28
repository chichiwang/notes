# Python Fundamentals
Course: [PluralSight](https://app.pluralsight.com/library/courses/python-fundamentals/table-of-contents)

## Table of Contents
* [Overview](#overview)
* [Getting Started](#getting-started)
  * [The Read-Eval-Print-Loop](#the-read-eval-print-loop)
  * [Significant Whitespace](#significant-whitespace)
  * [Python Culture and The Zen of Python](#python-culture-and-the-zen-of-python)
  * [Standard Library](#standard-library)
  * [Scalar Types and Values](#scalar-types-and-values)
    * [Integers](#integers)
    * [Floats](#floats)
    * [None](#none)

## Overview
Python is a programming language developed by Guido van Rossum in the late 1980's in the Netherlands. It is open-source with a very active community. Today it is maintained by the Python Software Foundation.

Attributes of the language:
* Strongly typed
  * Types are fixed for variables
* Dynamically typed
  * Type-checking is done at runtime
  * Duck-typed
* General purpose programming language
* Interpreted language
  * Actually compiled down to byte-code before execution
  * Does not have a compilation phase, so the exerperience is that of an interpreted language
* Syntax is designed to be: clear, readable, expressive
  * White-space aware
* Python implementations
  * CPython
    * Written in C
    * Most common distribution
    * Runs on the native machine
  * Jython
    * Written in JAVA
    * Runs on the JVM
  * Iron Python
    * Written in C#
    * Runs on .NET Runtime
  * Pypy
    * Written in RPython
    * Runs on the native machine and others

Python comes with a broad standard library. An aspect of the Python philosophy is that `Batteries Come Included`. The [Standard Library documentation](https://docs.python.org/3/) is available at [python.org/](https://www.python.org/).

For many people Python represents a philosophy of writing code. Principles of clarity and readability are part of what it means to write correct or "pythonic" code.

The [Zen of Python](https://www.python.org/dev/peps/pep-0020/) are a set of principles and guidelines to keep in mind when coding.

## Getting Started

### The Read-Eval-Print-Loop
To start the REPL enter into the command line:
```bash
$ python3
```

or for a specific version of Python:
```bash
$ python3.7
```

You will see the command prompt:
```bash
>>> 
```

There is a special variable in the REPL: The underscore refers to the last output:
```bash
>>> 2 + 2
4
>>> _
4
>>> _ * 10
40
```
This special variable only works in the REPL, it does not behave this way in a normal Python program.

Some statements do not have a return value:
```bash
>>> x = 5
>>>
```

Some statements have side-effects and no return value:
```bash
>>> print('Hello, Python!')
Hello, Python!
>>>
```
The output here is not a returned value, but a side-effect of the command.

To exit the REPL:
* On Windows press `CTRL + z` followed by `ENTER`
* On Mac or Linux press `CTRL + d`
* On any system you can just run the function `exit()` to exit the REPL

### Significant Whitespace
In Python leading whitespace is syntactically significant. Python uses indentation levels to demarcate code blocks. By convention contemporary Python code is indented by four spaces for each level:
```python
for i in range(5):
    x = i * 10
    print(x)

```
Terminating a code block is done by printing a newline.

Advantages of significant whitespace:
* Makes code more readable
* No clutter from braces or debates about where the braces should go
* Human and computer can't get out of sync

Conventions:
* Spaces are preferable to tabs
* Four spaces has become standard in the Python community
* **NEVER** mix spaces and tabs
  * Python interpreter will complain
* You are allowed to use different levels of indentation if you wish
  * General rule: be consistent with the indentation levels on consecutive lines
  * Consecutive lines at the same indentation level are considered to be part of the same block
  * Only deviate from this rule to improve readability

### Python Culture and The Zen of Python
The development of the Python language is managed through a series of documents called _Python Enhancement Proposals_ or PEPs. One of the PEPs, called PEP 8, explains how you should format your code.

It is PEP 8 that informs you should use 4 spaces for indentation in your Python code.

PEP 20 is called The Zen of Python. It refers to 20 aphorisms describing the guiding principals of Python, only 19 of which have been written down.

The Zen of Python can always be accessed through the REPL by typing `import this`.

### Standard Library
Python comes with an extensive standard library, an aspect of the language often referred to as _Batteries Included_.

You access the standard library modules by importing them:
```python
import math

math.sqrt(81)
```

To discover all attributes made available by a module in the REPL use the function `help()`. Make sure to import the module first:
```bash
>>> import math
>>> help(math)
```

You can also use `help` for help with a specific function within a module:
```bash
>>> import math
>>> help(math.factorial)
```

You can reduce the namespace needs by importing a single function from a module using the `from` operator:
```python
from math import factorial

factorial(5)
```

You can rename imported functions from a module using the `as` operator:
```python
from math import factorial as fac

fac(5)
```
This can be useful for purposes of readability or to avoid a namespace clash. It is recommended that `as` is used infrequently and judiciously.

Python's `/` operator is the floating point division operator:
```python
10 / 5 # 2.0
```

To do integer-only division, use the integer division operator `//`:
```python
10 // 5 # 2
```

Python, unlike many other languages, can compute with arbitrarily large integers, only limited by the memory in your computer.

### Scalar Types and Values
Python comes with a number of built-in data types. This includes primitive scalar types like _integers_ as well as collection types like _dictionaries_.

Scalar types available:
* `int`: arbitrary (unlimited) precision signed integer
* `float`: 64-bit floating point numbers
* `None`: the null object
* `bool`: boolean logic values

#### Integers
Integers can be created a variety of ways:
```python
10             # 10 - literal
0b10           # 2 - binary with the 0b prefix
0o10           #  8 - octal with the 0o prefix
0x10           # 16 - hexadecimal with the 0x prefix
int(3.5)       # 3 - convert from other numberical types with the int constructor
int(-3.5)      # -3 - rounding is always towards 0
int("42")      # 42 - strings can also be converted to int
int("100", 2)  # 4 - can provide a second argument to give the number base
```

#### Floats
Floating point numbers are supported by the `float` type. Python supports IEEE-754 double precision (64-bit) floating point numbers. It has 53 bits of binary precision and 15 to 16 bits of decimal precision.

Any number containing a decimal point `.` or the letter `e` is interpreted as a floating point number by python:
```python
3.125          # 3.125 - literal
3e8            # 300000000.0 - scientific notation
1.616e-35      # 1.616e-15 - scientific notation
float(7)       # 7.0 - convert from other numerical types with the float constructor
float("1.618") # 1.618 0 strings can be converted to float
float("nan")   # nan - special floating point value: not-a-number
float("inf")   # inf - special floating point value: positive infinity
float("-inf")  # -inf - special floating point value: negative infinity
3.0 + 1        # 4.0 - any operation involving both int and float results in a float
```
Python automatically switches the display representation to the most readable form (scientific vs literal).

#### None
`None` is the sole value of `NoneType`. It is used to represent the absence of a value. The Python REPL never prints `None` results.

```python
a = None  # None can be bound to variable names
a is None # true - can test if a value is None using the is-operator
```
