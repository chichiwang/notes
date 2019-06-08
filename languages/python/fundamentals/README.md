# Python Fundamentals
**Author**: [Chi-chi Wang](https://github.com/chichiwang)

Date: June 2019

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
    * [Booleans](#booleans)
  * [Relational Operators](#relational-operators)
  * [Conditional Statements](#conditional-statements)
* [Strings and Collections](#strings-and-collections)
  * [Strings](#strings)
  * [Bytes](#bytes)
  * [Lists](#lists)
  * [Dictionaries](#dictionaries)
* [Loops](#loops)
  * [While Loops](#while-loops)
  * [For Loops](#for-loops)
* [Namespaces, Scope, and The LEGB Rule](#namespaces-scope-and-the-legb-rule)
  * [Namespaces](#namespaces)
  * [Scope](#scope)
  * [The LEGB Rule](#the-legb-rule)
* [Modularity](#modularity)
  * [Special Attributes](#special-attributes)
  * [Modules, Scripts, and Programs](#modules-scripts-and-programs)
  * [Main Functions](#main-functions)
  * [Docstrings](#docstrings)
  * [Shebang](#shebang)
* [Objects](#objects)
  * [Arguments](#arguments)

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
```python
>>> 
```

There is a special variable in the REPL: The underscore refers to the last output:
```python
>>> 2 + 2
4
>>> _
4
>>> _ * 10
40
```
This special variable only works in the REPL, it does not behave this way in a normal Python program.

Some statements do not have a return value:
```python
>>> x = 5
>>>
```

Some statements have side-effects and no return value:
```python
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
```python
>>> import math
>>> help(math)
```

You can also use `help` for help with a specific function within a module:
```python
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

#### Booleans
Booleans are of type `bool`, representing logical states. They are used in control-flow structures and can either be of value `True` or `False`. There is a constructor function `bool` used to cast values to booleans.

```python
# Integers
bool(0)            # False
bool(42)           # True
bool(-1)           # True

# Floating point numbers
bool(0.0)          # False
bool(0.207)        # True
bool(-1.117)       # True

# Binary numbers
bool(0b0)          # False
bool(0b001)        # True

# Complex numbers
bool(0j)           # False
bool(0+0j)         # False
bool(0+1j)         # True
```

When converting from collections or lists, only empty collections are considered falsey:
```python
# Lists
bool([])           # False
bool([0])          # True

# Strings
bool("")           # False
bool("False")      # True

# Dictionaries
bool({})           # False
bool({ "f": "oo" } # True
```

### Relational Operators
Relational operators are used for comparing objects:
* `==` value equality / equivalence
* `!=` value inequality / inequivalence
* `<` less-than
* `>` greather-than
* `<=` less-than or equal to
* `>=` greater than or equal to

Relational operators can be chained:
```python
g = 20
10 < g < 30 # True
```

### Conditional Statements
```python
if expr:
    print("expr is True")

```
`expr` is converted to `bool` as if by the `bool()` constructor. Newline is used to terminate the conditional statement.

The `else` keyword is used to catch all other conditions:
```python
h = 42
if h > 50:
    print("Greater than 50")
else:
    print("Less than or equal to 50")

```

The `elif` keyword is used to provide additional conditionals to prevent unnecessary nesting of conditions:
```python
if h > 50:
    print("Greater than 50")
elif h < 20:
    print("Less than 20")
else:
    print("Between 20 and 50")
```

## Strings and Collections

### Strings
**String Literals**

Strings are of type `str`: immutable sequences of Unicode codepoints. String literals are created with (consistent) quotes, single or double:
```python
"This is a string"
'This is also a string'
```

The reason Python supports both double and single quotes is to allow strings to encapsulate quotes without escape characters:
```python
'He told me "This is how Python nests quotes in string literals."'
"That's the most sense he's made all day."
```

Adjacent literal strings are concatenated by the Python interpreter into a single string:
```python
>>> "first" "second"
'firstsecond'
>>> 
```
This can be useful for nicely formatting code.

**Multiline Strings**

To create strings with newlines you can use multiline strings or escape sequences:
```python
>>> "This is
... a multiline
... string"""
'This is\a multiline\nstring'
>>> m = "This string\nspans multiple\nlines."
'This string\nspans multiple\nlines.'
>>> print(m)
This string
spans multiple
lines.
>>> 
```

**Escape Sequences**

Python has a feature called _universal newlines_ which translates `\n` to the platform's native newline sequence (`\r\n` on Windows, `\n` on Linux).

You can also escape quotes within a string literal:
```python
>>> "This is a \" in a string."
"This is a \" in a string."
>>> 'I\'ll just escape the single quotes there.'
'I\'ll just escape the single quotes there.'
>>> "If you escape both \" and \' in a string, Python will strip the unnecessary backslash out."
"If you escape both \" and ' in a string, Python will strip the unnecessary backslash out."
```

You will also need to escape backslashes:
```python
escaped_backslash = 'This is what is required to encode a \\ into a string'
print(escaped_backslash) # This is what is required to encode a \ into a string
```

More information on escape sequences in string literals can be found in the [Python documentation](https://docs.python.org/2.0/ref/strings.html).

**Raw Strings**

Python provides a method of creating raw strings for things like RegEx where you will need to constantly escape characters:
```python
path = r'C:\USERS\chichiwang\Documents'
```

Raw strings preserve the backslashes used in the string.

**String Constructor**
You can use the `str` constructor to create strings:
```python
>>> str(496)
496
>>> str(6.02e23)
6.02e+23
```

**Sequence Types**

Strings are considered [sequence types](https://docs.python.org/3/library/stdtypes.html#sequence-types-list-tuple-range). This means they support common sequence operations such as accessing indivual characters using `[]`:
```python
>>> s = "Hello World"
>>> s[1]
e
>>> 
```

There is no separate _character_ type, only a `str` type. A single character string is still a `str`.

**Built-in Methods**

`str` types have a number of [built-in methods](https://docs.python.org/3/library/stdtypes.html#string-methods). Type `help(str)` into the REPL to learn more.

**Unicode**

Strings are fully unicode capable so you can utilize them in a literal string, or use escape sequences to enter to character codes:
```python
>>> 'Vi er s5 glad for å høre og lære om Python!'
Vi er s5 glad for å høre og lære om Python!
>>> 'Vi er s\u0035 glad for \u00e5 h\u00f8re og l\u00e6re om Python!'
Vi er s5 glad for å høre og lære om Python!
>>> '\xe5'
å
>>> '\345'
å
>>> 
```

### Bytes
[Bytes](https://docs.python.org/3/library/stdtypes.html#bytes) are immutable sequences of bytes. They are used for all binary data and fixed-witdth single-character encodings, such as ASCII.

Bytes can be declared literally using quotes and a prefix `b`:
```python
b'data'
b"data"
```
Only ASCII characters are permitted in bytes literals. Any binary values over 127 must be entered into bytes literals using the appropriate escape sequence.

Bytes are useful to operate with when in multi-lingual environemnts. Bytes strings can be decoded and encoded using many of Python's standard encoding and decoding functions.

### Lists
Lists are mutable sequences of objects. List literals are denoted by square brackets `[]`:
```python
['a', 'b', 'c', 'd']
[1, 2, 3, 4]

fruit = ['apple', 'orange', 'pear']

fruit[1] # orange - access elements of a list
fruit[1] = 'watermelon' # replace an element in a list
```

The `list()` constructor can be used to create lists from other collections, such as strings:
```python
>>> list("foobar")
['f', 'o', 'o', 'b', 'a', 'r']
```

### Dictionaries
Dictionaries are mutable mappings of keys to values (also known as an associative array). They are of type `dict`.

Dictionary literals are constructed with curly braces and key-value pair definitions:
```python
hello_world = {
    "message": "Hello",
    "subject": "World",
    "punctuation": "!",
}
```

Read/write to the values can be done through the use of square brackets `[]`:
```python
print(hello_world[message]) # Hello
hello_world["punctuation"] = "."
```

The entries of a dictionary cannot be relied upon to be in any particular order.

## Loops

### While Loops
Syntax:
```python
while expr:
    print("Loop while expr is True")
```

Explicit is better than implicit, according to the Zen of Python so:
```python
c = 5

# Prefer this condition:
while c != 0:
    c -= 1

# Over this condition:
while c:
    c -= 1
```

The `break` keyword terminates the innermost loop, transferring execution to the first statement after the loop:
```python
while True:
    if expr:
        break

print("Loop broken")
```

### For Loops
Syntax:
```
for item in iterable:
  ...body...
```

Example:
```python
cities = ["London", "New York", "Paris", "Oslo", "Helsinki"]

for city in cities:
    print(city)
```

When you iterate over a dictionary, you receive the keys which can be used to access the values:
```python
colors = {
  "crimson": 0xdc143c,
  "coral": 0xff7f50,
  "teal": 0x008080,
}

for color in colors:
    print(color, colors[color])
```

## Namespaces, Scope, and The LEGB Rule
From [an article](https://sebastianraschka.com/Articles/2014_python_scope_and_namespaces.html) by Sebastian Raschka.

### Namespaces
Namespaces are containers for mapping names to objects. Everything in Python is an object.

Namespaces can be pictured as a Python dictionary structure:
```
a_namespace = { 'variable_a': object_1, 'variable_b': object_2, ... }
```

There are multiple independent namespaces and names can be reused for different namespaces:
```
a_namespace = { 'variable_a': object_1, 'variable_b': object_2, ... }
b_namespace = { 'variable_a': object_3, 'variable_b': object_4, ... }
```

Variables are scoped to the innermost function, class, or module in which they are assigned.

### Scope
Scope defines the hierarchy level in which `namespaces` are searched for name-to-object mappings.

Example:
```python
i = 1

def foo():
  i = 5
  print(i, 'in foo()')

print(i, 'in global')

foo()

"""
Output:
1 in global
5 in foo()
"""
```

**Tip**
To print out a dictionary mapping of the global and local variables use the functions `global()` and `local()`:
```python
glob = 1

def foo():
  loc = 5
  print('loc in foo():', 'loc' in locals())

foo()
print('loc in global:', 'loc' in globals())
print('glob in global:', 'glob' in globals())

"""
Output:
loc in foo(): True
loc in global: False
glob in global: True
"""
```

### The LEGB Rule
The LEGB Rule stands for:

**Local -> Enclosed -> Global -> Built-In**

This denotes the order in which Python searches namespaces for a variable:
* *Local* can be inside a function or class method
* *Enclosed* is the the direct parent enclosing scope
* *Global* is the uppermost level of the executing script itself
* *Built-Ins* are special variable names that Python reserves

Namespaces can be further nested inside of modules or new classes:
```python
import numpy
import math
import scipy

print(math.pi, 'from the math module')
print(numpy.pi, 'from the numpy package')
print(scipy.pi, 'from the scipy package')

"""
Output:
3.141592653589793 from the math module
3.141592653589793 from the numpy package
3.141592653589793 from the scipy package
"""
```

**Note**
Be careful when importing from a module via the syntax `from a_module import *` since this loads variable names into the global namespace for the module, potentially overwriting variable names.

**Examples**
Examples of scope access rules can be found in the [exercises directory](./exercises/00%20-%20Scope).

It is possible to modify a global value from inside a local scope by using the `global` keyword as seen in [exercises/00 - Scope/example_02.py](./exercises/00%20-%20Scope/example_02.py).

## Modularity
Python programs are organized into modules. Modules can be imported into other modules, but you must take care not to introduce circular dependencies.

For the purposes of this section I will be following some exercises in the [Pluralsight Course](https://app.pluralsight.com/library/courses/python-fundamentals/table-of-contents).

Modules I create for this will live in the [/exercises](./exercises) directory and can be executed using `python3.7`.

### Special Attributes
The Python runtime system defines special variables and attributes whose names are delimited by double underscores.

One such variables is `__name__`: evaluates to `__main__` or the actual module name depending on how the enclosing module is being used.

If the module is imported, `__name__` evaluates to the module name (file name), but if the module is passed directly to the Python command (`python3.7 my_module.py`) it evaluates to `__main__`. This can be [used in conditionals](./exercises/03%20-%20conditionalized%20module/words.py#L22-L23) to run the module as a script as well as use it as a module both.

### Modules, Scripts, and Programs

* `module`: Convenient import namespace with an API
* `script`: Convenient execution from the command line (module-global expressions and statements)
* `program`: Perhaps composed of many modules

It is recommended to makes `scripts` importable for convenient execution from the Python REPL.

Even `modules`, only meant to be imported in production settings, benefit from having executable test code.

It is a good practice to have all modules created with defined executable functions with a [post-script](./exercises/03%20-%20conditionalized%20module/words.py#L22-L23) to facilitate execution.

### Main Functions
Main functions called from postscripts can define default execution behavior for a module when run as a script. The example in [exercise 4](./exercises/04%20-%20main%20function/words.py) shows one such approach to the pattern. While `main()` is a common function name for the main execution, the function can have any name that encapsulates the default script-execution logic.

### Docstrings
[PEP 257](https://www.python.org/dev/peps/pep-0257/) outlines conventions for using docstrings. This is not widely accepted at this time.

The [Google Python Style Guide](https://github.com/google/styleguide/blob/gh-pages/pyguide.md#38-comments-and-docstrings) also presents recommendations for docstrings, recommending listing arguments in a conventionalized manner. This makes the docstrings ammenable to machine parsing while still being readable as comments to humans.

Module level docstrings and function level docstrings will appear when you request `help()` on the module or module methods respectively.

### Shebang
In unix-like systems, a special comment (called a shebang) is often used on the first line of a program or script. The shebang starts with `#!` and allows the program loader to identify which interpreter should be used to run the program.

For Python the shebang is useful to identify whether the code within a file is Python 2 or Python 3.

More details, and an example can be found in [exercise 07](./exercises/07%20-%20shebang/words.py#L1-L11).

**Note:** The Python shebang works on Windows for v3.3 and above. This is because Windows now runs Python files using the [Python Launcher](https://docs.python.org/3/using/windows.html#launcher).

## Objects
Python assigns variables by pointing them to underlying objects in memory.
```python
# Creates an object in memory containing the value 100 and points var x to it
x = 100

# Value is not mutated, cannot be mutated
# New object with new value created in memory
# x variable reassigned to point to that instead
x = 500


# Assigning a var to another var just points one
# variable to the same object reference as the other
y = x
```

**Note**: Garbage collector will only clean up objects not reachable by live references.

The built in `id()` function returns an integer identifier, which is unique and constant for the lifetime of an object. The built in `is` will compare the object references of two variables:
```python
>>> a = 5
>>> id(a)
140155613686816
>>> b = a
>>> id(b)
140155613686816
>>> a is b
True
```

The assignment operator `=` only ever binds to names, it never binds objects by value. In Python variables are really named references to objects. References behave like labels that allow us to retrieve objects.

The `==` operator tests for value equality. The `is` operator tests for identity.

### Arguments
Arguments are passed by reference. When that reference is to a mutable object, and that object is modified within the function, the source object is modified.

If a function is meant to modify a copy of a passed-in object, it is the responsibility of the function to do the copying.

It is possible to rebind function arguments to new object references.

Function arguments are `pass by object reference`. The value of the reference is copied, not the value of the object.

The `return` operator also passes by object reference.
```python
>>> def f(d):
...   return d
...
>>> c = [6, 10, 16]
>>> e = f(c)
>>> c is e
True
```

---

**WIP** Coure not yet completed
