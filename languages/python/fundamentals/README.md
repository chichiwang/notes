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
    * [Default Arguments](#default-arguments)
  * [Type System](#type-system)
  * [Variable Scoping](#variable-scoping)
  * [Everything Is An Object](#everything-is-an-object)
* [Collections](#collections)
  * [Tuple](#tuple)
  * [Str](#str)
  * [Range](#range)
  * [List](#list)
  * [Dict](#dict)
  * [Set](#set)
  * [Collection Protocols](#collection-protocols)
* [Exceptions](#exceptions)
  * [Programmer Errors](#programmer-errors)
  * [Re-raising Exceptions](#re-raising-excpetions)
  * [Exceptions As API](#exceptions-as-api)
  * [Exception Protocols](#exception-protocols)
  * [EAFP vs LBYL](#eafp-vs-lbyl)
  * [Finally](#finally)
* [Iterables](#iterables)
  * [Iteration Protocols](#iteration-protocols)
  * [Comprehensions](#comprehensions)
    * [List Comprehensions](#list-comprehensions)
    * [Set Comprehensions](#set-comprehensions)
    * [Dictionary Comprehensions](#dictionary-comprehensions)
    * [Generator Comprehensions](#generator-comprehensions)
    * [Filtering Predicates](#filtering-predicates)
  * [Generators](#generators)
    * [Stateful Generator Functions](#stateful-generator-functions)
* [Classes](#classes)
* [Files And Resource Management](#files-and-resource-management)
  * [Reading And Writing Text Files](#reading-and-writing-text-files)
  * [Files As Iterators](#files-as-iterators)
  * [Context Managers](#context-managers)
  * [File Like Objects](#file-like-objects)
* [Unit Tests](#unit-tests)
* [Python Debugger](#python-debugger)
* [Virtual Environemnts](#virtual-environments)
* [Distributing Programs](#distributing-programs)
* [Installing Third-Party Modules](#installing-third-party-modules)

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
```console
$ python3
```

or for a specific version of Python:
```console
$ python3.7
```

You will see the command prompt:
```console
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
The development of the Python language is managed through a series of documents called [_Python Enhancement Proposals_](https://www.python.org/dev/peps/) or PEPs. One of the PEPs, called PEP 8, explains how you should format your code.

It is PEP 8 that informs you should use 4 spaces for indentation in your Python code.

PEP 20 is called The Zen of Python. It refers to 20 aphorisms describing the guiding principals of Python, only 19 of which have been written down.

The Zen of Python can always be accessed through the REPL by typing `import this`.

### Standard Library
Python comes with an [extensive standard library](https://docs.python.org/3/library/), an aspect of the language often referred to as _Batteries Included_.

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

#### Default Arguments
```python
def fn(a, b=default_value):
  # ...
```

When defining a function with default arguments, the arguments with defaults must come after arguments without defaults, otherwise a syntax error is thrown.

Keyword arguments can be used for explicitness:
```python
def banner(message, border='-'):
  line = border * len(message)
  print(line)
  print(message)
  print(line)


banner("Norwegian Blue")
"""
--------------
Norwegian Blue
--------------
"""

banner("Norwegian Blue", bordre="*")
"""
**************
Norwegian Blue
**************
"""
```

Keyword arguments must come after positional arguments at the call sites. If we provide keywords for all arguments, we can supply them in any order:
```python
banner(border='.', message='Hello From Earth')
"""
................
Hello From Earth
................
"""
```

Default argument assignments are only evaluated once, when the function definition is evaluated:
```python
>>> import time
>>> def show_default(arg=time.ctime()):
...     print(arg)
...
>>> show_default()
Fri Jun  7 22:21:09 2019
>>> show_default()
Fri Jun  7 22:21:09 2019
```
The time doesn't change because the default value assignment only occurs at evaluation time of the function definition.

Default arguments can be modified like any other mutable value:
```python
>>> def add_spam(menu=[]):
...     menu.append("spam")
...     return menu
...
>>> add_spam()
['spam']
>>> add_spam()
['spam', 'spam']
```

To work around this, always use immutable values as default arguments:
```
def add_spam(menu=None):
  if menu is None:
    menu = []
  menu.append("spam")
  return menu
```

### Type System
Python has a **dynamic** and **strong** type system.

In a **dynamic type system** objects types are only resolved at runtime:
```python
>>> def add(a, b):
...     return a + b
...
>>> add(5, 7)
12
>>> add(3.1, 2.4)
5.5
>>> add("news", "paper")
'newspaper'
```

In a **strong type system** there is no implicit type conversion:
```python
>>> add("The answer is", 42)
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "<stdin>", line 2, in add
TypeError: can only concatenate str (not "int") to str
```

The system generally will not perform implicit conversions between object types or otherwise attempt to coerce one type to another. The exception being the conversion to `bool` in `if` statements and `while` loop predicates.

### Variable Scoping
*Scopes* are contexts in which name references can be looked up.

Name scopes include:
* `Local`: Inside the current function
* `Enclosing`: Any and all enclosing functions
* `Global`: Top-level of the module
* `Built-in`: Provided by the *builtins* module

Blocks (of indentation) do not introduce new scopes.

When you want to access a globally-scoped variabled from inside a function scope, use the `global` keyword as demonstrated in [exercise 8](./exercises/08\ -\scope/scope.py). To see this in action navigate to the [exercise directory](.exercises/08\ -\ scope) and run in the REPL:
```python
>>> import scope
>>> scope.show_count()
Count =  0
>>> scope.set_count(42)
>>> scope.show_count()
Count =  42
>>>
```

### Everything Is An Object
Everything in Python is an object. You can inspect the type of any object using the `type()` function:
```python
>>> type(2)
<class 'int'>
```

You can instrospect an object using the `dir()` function:
```python
>>> dir(2)
['__abs__', '__add__', '__and__', '__bool__', '__ceil__', '__class__', '__delattr__', '__dir__', '__divmod__', '__doc__', '__eq__', '__float__', '__floor__', '__floordiv__', '__format__', '__ge__', '__getattribute__', '__getnewargs__', '__gt__', '__hash__', '__index__', '__init__', '__init_subclass__', '__int__', '__invert__', '__le__', '__lshift__', '__lt__', '__mod__', '__mul__', '__ne__', '__neg__', '__new__', '__or__', '__pos__', '__pow__', '__radd__', '__rand__', '__rdivmod__', '__reduce__', '__reduce_ex__', '__repr__', '__rfloordiv__', '__rlshift__', '__rmod__', '__rmul__', '__ror__', '__round__', '__rpow__', '__rrshift__', '__rshift__', '__rsub__', '__rtruediv__',
'__rxor__', '__setattr__', '__sizeof__', '__str__', '__sub__', '__subclasshook__', '__truediv__', '__trunc__', '__xor__', 'bit_length', 'conjugate', 'denominator', 'from_bytes', 'imag', 'numerator', 'real', 'to_bytes']
```

This reveals the inner workings of any object.

## Collections
This section will cover various built-in Python collection types:
* [tuple](#tuple)
* [str](#str)
* [range](#range)
* [list](#list)
* [dict](#dict)
* [set](#set)

`byte` will not be covered as most of what is covered in `str` also applies to `byte`.

### Tuple
*Tuples* are heterogenous immutable sequences: sequences of arbitrary objects. Once created the objects within them cannot be replaced or removed, and new elements cannot be added.

Tuples can be created with a literal syntax using parentheses:
```python
t = ("Norway", 4.953, 3)
h = (391,) # To create a single-element tuple, use a trailing comma
e = () # To create an empty tuple, use empty parentheses
p = 1, 1, 1, 4, 6, 19 # Parens may be omitted in creating tuples
a = ((220, 284), (1184, 1219)) # It is possible to have nested tuples
```

Operations:
```python
t[0] # 0-indexed element access
len(t) # Get the length of a tuple

# Iterate over elements in a tuple
for item in t:
  print(item)

t + (5) # Concatenation using the `+` operator
t * 2   # Repetition using the `*` operator

a[0][1] # Index access of multi-dimensional tuple

def minmax(items):
    return min(items), max(items) # It can be useful to return multiple values using tuples


lower, upper = minmax([5, 2, 1, 8, 12, 10]) # Tuple unpacking can be a useful way to destructure assignment
(a, (b, (c, d))) = (4, (3, (2, 1))) # Tuple unpacking works with arbitrarily nested tuples

# Idiomatic Python swap
a = "jelly"
b = "bean"
a, b = b, a

tuple([1, 2, 3]) # Tuple constructor can be used to create a tuple from other iterable series of objects
tuple("abc") # This also works with strings

# Test membership with `in` or `not in` operators
5 in (3, 5, 7, 257, 65537)
5 not in (3, 5, 7, 257, 65537)
```

### Str
*Strings* are homogenous immutable sequences of Unicode codepoints (characters).

Operations:
```python
len("hello") # Determine the length of a string

"New" + "found" + "land" # The `+` operator can be used for string concatenation
# Use the `+` and `+=` operators sparingly as they rebind the reference to new objects

# For larger strings, the `join()` method should be used
colors = ";".join(["#45ff23", "#2321fa"]) # `join()` is called on the separator
# For pure concatenation, use `join()` on an empty string
"".join(["mult", "ipl", "e str", "ings"])
# The symmertrical operation of `join()` is `split()`
colors.split(";")

# `partition()` divides a string into three parts around the argument:
"unforgetable".partition("forget") # ('un', 'forget', 'able')
# This is useful for destructuring assignments:
departure, _, arrival = "London:Edinburgh".partition(":") # It is convention to use `_` for dummy values

# `format()` is used for string interpolation
"{0}, {1}!".format("Hello", "World")
# If the field names are used only once and in order, the index can be omitted:
"{}, {}!".format("Hello", "World")
# Named fields can be used instead of indexes
"Current Position: {latitude} {longitude}".format(latitude="60N", longitude="5E")
```

### Range
A *range* is an arithmetic sequence of integers.

A range is constructed with the `range()` constructor - there is no literal form:
```python
# Construct a range from 0 to 5
range(5) # range(0, 5)

# Construct a range from 5 to 10
range(5, 10) # range(5, 10)

# Step argument can be provided to specify the interval between successive numbers
range(0, 10, 2) # range(0, 10, 2)
```

The end of a range is one before the stop value provided:
```python
list(range(5)) # [0, 1, 2, 3, 4]
list(range(0, 10, 2)) # [0, 2, 4, 6, 8]
```

The number of arguments provided to the `range()` constructor specify their use:

|   constructor   |     arguments     |       result       |
| --------------- | ----------------- | ------------------ |
| range(5)        | stop              | 0, 1, 2, 3, 4      |
| range(5, 10)    | start, stop       | 5, 6, 7, 8, 9      |
| range(10, 20, 2 | start, stop, step | 10, 12, 14, 16, 18 |

Tips:
* Avoid using `range()` for iterating over lists
  * Prefer to use iteration over objects themselves
* Prefer `enumerate()` over `range()` for counters
  * `enumerate()` yields (*index*, *value*) tuples
* Because of the strong iteration primitives built into Python, ranges are not widely used in modern Python code.

### List
A *list* is a heterogenous mutable sequence. We've covered lists a bit in a [previous section](#lists).

Operations:
```python
lst = ["indexing", "within", "lists", "in", "python"]

lst[-2]  # 'in' - index from the end using negative indicies
lst[1:3] # ['within', 'lists'] - slice lists by using a colon in the index access brackets
lst[2:]  # Slice the list from the third element to the end of the list
lst[:3]  # Slice the list up to the third element
lst[:]   # Slice the entire list - import idiom for shallow-copying the list

# Copies
lst2 = lst.copy() # A more readable method for shallow-copying a list
lst3 = list(lst)  # Shallow-copy a list using the list constructor (preferred)

# Repetitions
# Useful for initializing a list of a known size in advance with elements set to a constant
# Repetition is shallow (creates copy of the reference, not the value)
a = [1, 2]
a * 3          # [1, 2, 1, 2, 1, 2]
b = [None] * 5 # [None, None, None, None, None]

# index()
# Returns the index of an element by value equivalence
c = [100, 101, 102, 103]
c.index(101) # 1
c.index(300) # Raises a ValueError

d = [100, 101, 102, 102, 103]
d.index(102) # 2 - returns the first equivalent value index

# count()
# Returns the number of matching elements in the list
d.count(102) # 2 - number of occurrences of 102 in d

# Testing membership
101 in [100, 101, 102]     # True - `in` operator tests element membership
101 not in [100, 101, 102] # False - `not in` tests for non-membership

# Element Removal
e = [100, 101, 102]
del e[1]      # Removes value at index 1 from e
e.remove(100) # Removes an element by value from e
f = [100, 100, 101, 102]
f.remove(100) # Only removes the first instance of the value, f = [100, 101, 102]
f.remove(300) # Raises a ValueError

# Element Insertion
g = [100, 101]
g.insert(1, 102) # g = [100, 102, 101] - inserts an element at the specified index

# Concatenation
h = [0, 1]
h + [2, 3]       # [0, 1, 2, 3] - Returns a new list concatenating the two
h += [2, 3]      # Mutates value of h, appending the new elements
h.extend([4, 5]) # Mutates value of h, appending the new elements
h                # [0, 1, 2, 3, 4, 5]

# Reverse
h.reverse() # Mutates h, reversing the order of its elements
h           # [5, 4, 3, 2, 1, 0]

# Sort
h.sort() # Sorts the elements of list in ascending order
h        # [0, 1, 2, 3, 4, 5]

# Sort with `reverse` parameter
h.sort(reverse=True) # Sorts h in descending order
h                    # [5, 4, 3, 2, 1, 0]

# Sort with `key` parameter
# Sort can accept a `key` parameter of any function that returns a sort key from any element
i = ['0', '012', '01', '0123']
i.sort(key=len) # Sorts i according to the length function
i               # ['0', '01', '012', '0123']

# Reversed
# Reverses lists without mutating them
# Returns an iterator
j = [0, 1, 2]
reversed(j) # Returns `list_reverseiterator object`
list(reversed(j)) # [2, 1, 0]

# Sorted
# Sorts lists without mutating them
# Returns a new list
k = [0, 2, 3, 1]
sorted(k) # [0, 1, 2, 3]
```

### Dict
A *dictionary* is an unordered mapping from unique, immutable keys to mutable values. We've covered dictionaries a bit in a [previous section](#dictionaries).

Dictionary keys **must be immutable** so strings, numbers, and tuples are valid keys but lists are not. The values may be mutable.

The order of keys in a dictionary is not to be relied upon - order is random and may differ between different runs of the same program.

Construction:
```python
# Literal syntax
my_dict = {
    'key_1': 'value_1',
    2:  2,
    (3): 3.0,
}

# Constructor
# Can be constructed from:
#     * An iterable series of key-value 2-tuples
#     * Keyword arguments - requires keys are valid Python identifiers
#     * A mapping, such as another dict
# Value assignments are shallow copies
names_and_ages = [('Alice', 32), ('Bob', 48), ('Charlie', 28)]
d = dict(names_and_ages)
phonetic = dict(a='alpha', b='bravo', c='charlie')
```

Operations:
```python
# Copies
e = d.copy() # Shallow-copy method of a dict
f = dict(e)  # Passing another dict into the dict() constructor creates a shallow copy

# Update - destructive shallow merge
g = { 1: 1, 2: 2, 3: 3 }
h = { 'four': 4, 'five': 5, 'six': 6 }
g.update(h)
g  # { 1: 1, 2: 2, 3: 3, 'four': 4, 'five': 5, 'six': 6 }

# Updates override original keys if corresponding keys exist in the target dict
g.update({ 'four': 4.0 })
g  # { 1: 1, 2: 2, 3: 3, 'four': 4.0, 'five': 5, 'six': 6 }

# Iterating over keys
# The order is arbitrary
for key in g:
    print(f'{ key } => { g[key] }')

# Iterating over values
for value in g.values():
    print(value)

# Keys
g.keys() # Returns iterable list of keys

# Iterate over key/value pairs
for key, value in colors.items():
  print(f'{ key } => { value }')

# Testing membership
'four' in g      # True - returns membership of keys
'seven' not in g # True - returns non-membership of keys

# Deleting key/value pair
del g[1] # Removes key 1 from g

# Adding key/value pair
g[1] = 1.00 # Dictionaries themselves are mutable

# Pretty printing
# Built into the standard library
from pprint import pprint as pp
pp(g)
```

### Set
A *set* is an unordered collection of unique, immutable objects. A set, itself, is mutable: elements can be added/removed from the set.

Construction:
```python
# Literal syntax
my_set = { 1, 2, 3, 5, 8, 13 }

# Constructor
# Accepts any iterable series of values including lists 
# Duplicates are discarded
empty_set = set() # Returns an empty set when not provided arguments
set_from_list = set([0, 1, 1, 2, 3, 5, 8, 13])
set_from_list # {0, 1, 2, 3, 5, 8, 13}
```

Operations:
```python
# Iterate over a set
# Order is arbitrary
for x in set_from_list:
    print(x)

# Testing membership
1 in set_from_list      # True
10 not in set_from_list # True

# Add an element to a set
set_from_list.add(10)
set_from_list.add(1) # Adding an element that already exists has no effect and produces no errors

# Add multiple elements to a set from any iterable series
set_from_list.update([2, 4, 6, 8, 10])

# Removing an element from a set
set_from_list.remove(4)  # Removes an element from the set
set_from_list.remove(13) # Throws KeyError when trying to remove an element not in the set

# Discarding an element from a set
set_from_list.discard(6)
set_from_list.discard(13) # Does not throw if item is not in the set

# Copy a set (shallow)
clone_set = set_from_list.copy()
boba_set = set(clone_set) # set() constructor can be used to copy

# Setup for set operations
blue_eyes = {'Olivia', 'Harry', 'Lily', 'Jack', 'Amelia'}
blond_hair = {'Harry', 'Jack', 'Amelia', 'Mia', 'Josh'}
o_blood = {'Mia', 'Joshua', 'Lily', 'Olivia'}
a_blood = {'Harry'}
b_blood = {'Amelia', 'Jack'}

# Union
blue_eyes.union(blond_hair) # Retrieve the list of members of either blue_eyes or blond_hair (returns all)

# Intersection
blue_eyes.intersection(blond_hair) # Only return the members of both blue_eyes and blond_hair

# Difference
blue_eyes.difference(blond_hair) # Only return the members of blue_eyes who are not members of blond_hair
blond_hair.difference(blue_eyes) # Only return the members of blond_hair who are not members of blue_eyes

# Symmetric difference
blue_eyes.symmetric_difference(blond_hair) # Return members exclusive to each set

# Testing subsets
a_blood.issubset(blue_eyes) # True - tests if a_blood is a subset of blue_eyes

# Testing supersets
blond_hair.issuperset(b_blood) # True - tests if blond_hair is a superset of b_blood

# Testing disjoints
a_blood.isdisjoing(o_blood) # True - tests that a_blood and o_blood has no intersections
```

### Collection Protocols
A *protocol* is a set of operations or methods that a type must support if it is to implement that protocol.

Table of protocols supported by collection types:

|     Protocol     |          Implementing Collections         |
| ---------------- | ----------------------------------------- |
| Container        | str, list, range, tuple, bytes, set, dict |
| Sized            | str, list, range, tuple, bytes, set, dict |
| Iterable         | str, list, range, tuple, bytes, set, dict |
| Sequence         | str, list, range, tuple, bytes            |
| Mutable Sequence | list                                      |
| Mutable Set      | set                                       |
| Mutable Mapping  | dict                                      |

* The `container` protocol requires membership testing using `in` and `not in` operators
* The `sized` protocol requires that the number of elements can be determined using the `len()` function
* Types that implement the `iterable` protocol provide a means of yielding their elements one by one as they are requested
  * They can be used with `for` loops
* The `sequence` protocol requires:
  * Its items can be retrieved using square brackets and an integer index
  * Its items can be searched for using the `.index()` method
  * Its items can be counted with the `.count()` method
  * A reversed copy of the sequence can be produced with the `.reversed()` method

## Exceptions
Raise an exception to interrupt the flow of a program. Handle an exception to resume control. Unhandled exceptions will terminate the program.

Exception objects contain information about the exceptional event.

Exceptions bubble up through the call stack.

Sample syntax for exception handling using the `except` keyword found in [exercise 09](./exercises/09%20-%20exception%20handling/exceptional.py).

To print an error back to the system standard error output stream:
```python
import sys

def convert(s):
    try:
      return int(s)
    except (ValueError, TypeError) as e:
      print("Conversion error: {}"\
            .format(str(e)),
            file=sys.stderr)
      return -1
```

Assign the error to a variable using the `as` operator, then print it out to `sys.stderr`.

As a matter of philosophy errors should never pass silently unless explicitly silenced. Silent errors are of no use.

### Programmer Errors
Exception types for programmer errors:
* `IndentationError`
* `SyntaxError`
* `NameError`

These are not meant to be handled by exception blocks, but rather in the course of development.

### Re-raising Exceptions
It is a best practice to re-raise an error code rather than to pass numerical error codes around:
```python
import sys

def convert(s):
    try:
        return int(s)
    except (ValueError, TypeError) as e:
        print("Conversion error: {}"\
              .format(str(e)),
              file=sys.stderr)
        raise
```

When `raise` is invoked without a parameter it will re-raise the exception that is currently being handled.

### Exceptions As API
Exceptions are part of the API. Callers need to know what exceptions to expect and when.

For an example of how to supply a DocString with errors as well as handling and throwing errors, see [exercise 10](./exercises/10%20-%20exceptions%20as%20api/roots.py).

### Exception Protocols
Exceptions are part of families of related functions referred to as *protocols*. Use common or existing exception types when possible.

A few common exception types:
* `IndexError` is raised when integer index is out of range
```python
z = [1, 4, 2]
z[4] # Raises an IndexError: list index out of range
```
* `ValueError` is raised when an object is of the right type but contains an inappropriate value
```python
int("jim") # Raises a ValueError: invalid literal for int() with base 10: 'jim'
```
* `KeyError` is raised when a look-up in a mapping fails
```python
codes = dict(gb=44, us=1, no=47, fr=33, es=34)
codes['de'] # Raises a KeyError: 'de'
```

**Note**: Avoid protecting against `TypeErrors`. This is against the grain of dynamic typing in Python and limits the reuse potential of code.

It's usually not worth checking types - incompatible types will typically result in a `TypeError` anyway. Checking types can unnecessarily limit your functions.

### EAFP vs LBYL
Another tenet of Python philosophy and culture: it's easier to ask for forgiveness than permission.

There are two approaches to dealing with a program operation that might fail:
1. Check that all preconditions of a failure-prone operation are met in advance of attempting the operation.
2. Blindly hope for the best, but be prepared to deal with the consequences.

These two philosophies are known as:
1. Look Before You leap (`LBYL`)
2. It's Easier To Ask Forgiveness Than Permission (`EAFP`)
  * Coined by Rear Admiral Grace Hopper

The EAFP approach is preferred. With the LBYL approach you need to write preemptive tests for all possible conditions. The EAFP approach is to wrap failure-prone operations with exception handlers to deal with the cases where it does fail.

**Exceptions vs Error Codes**
* Error codes require interspersed, *local handling*
* Exceptions allow centralized, *non-local handling*
* Exceptions require explicit handling
  * They interrupt the program flow and cannot be ignored
* Error codes are silent by default

### Finally
The `finally` block allows clean up operations to occur regardless of exceptions:
```python
import os
import sys

def make_at(path, dir_name):
    original_path = os.getcwd()
    try:
        os.chdir(path)
        os.mkdir(dir_name)
    except OSError as e:
        print(e, file=sys.stderr)
        raise
    finally:
        os.chdir(original_path)
```
If `os.mkdir()` fails, the Python process won't be restored to its original value. This would cause the `make_at()` function to leave an unintentional side-effect. The `finally` block ensures that the function restores the original current working directory regardless of success or failure.

## Iterables
Iterables are a central feature of Python. There are a number of built-in functions for performing common iterator operations (such as `enumerate()` and `sum()`). In addition to the built-in functions, the [itertools](https://docs.python.org/3/library/itertools.html) module contains functions for processing iterable streams of data.

### Iteration Protocols
There are two important concepts on which a great deal of Python language behavior is constructed: *iterable objects* and *iterator objects*.

**Iterable Protocol**

Iterable objects can be passed to the built-in `iter()` function to get an iterator: `iterator = iter(iterable)`

**Iterator Protocol**

Iterator objects can be passed to the built-in `next()` function to fetch the next item: `item = next(iterator)`

**Demo**
```python
>>> iterable = ['Spring', 'Summer', 'Autumn', 'Winter']
>>> iterator = iter(iterable)
>>>
>>> next(iterator)
'Spring'
>>> next(iterator)
'Summer'
>>> next(iterator)
'Autumn'
>>> next(iterator)
'Winter'
>>> next(iterator)
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  StopIteration
  Error in sys.excepthook:
  Traceback (most recent call last):
    File "/usr/lib/python3/dist-packages/apport_python_hook.py", line 63, in apport_excepthook
        from apport.fileutils import likely_packaged, get_recent_crashes
    File "/usr/lib/python3/dist-packages/apport/__init__.py", line 5, in <module>
        from apport.report import Report
    File "/usr/lib/python3/dist-packages/apport/report.py", line 30, in <module>
        import apport.fileutils
    File "/usr/lib/python3/dist-packages/apport/fileutils.py", line 23, in <module>
        from apport.packaging_impl import impl as packaging
    File "/usr/lib/python3/dist-packages/apport/packaging_impl.py", line 24, in <module>
        import apt
    File "/usr/lib/python3/dist-packages/apt/__init__.py", line 23, in <module>
        import apt_pkg
  ModuleNotFoundError: No module named 'apt_pkg'

  Original exception was:
  Traceback (most recent call last):
    File "<stdin>", line 1, in <module>
  StopIteration
>>>
```

Calling `next()` on an iterator at the end of the iterable raises a *StopIteration* exception.

Higher-level iteration concepts such as `for` loops and comprehensions are built directly upon this lower-level iteration protocol.

### Comprehensions
*Comprehensions* are a concise syntax for describing [lists](#list), [sets](#set), or [dictionaries](#dict) in a declarative or functional style.

Comprehensions should ideally be purely functional: they should produce no side effects.

#### List Comprehensions
```python
>>> words = "A list constructed out of a string".split(' ')
>>> words
['A', 'list', 'constructed', 'out', 'of', 'a', 'string']
>>> [len(word) for word in words] # List comprehension
[1, 4, 11, 3, 2, 1, 6]
>>>
```

The general form of list comprehensions is `[ expr(item) for item in iterable ]`.

#### Set Comprehensions
```python
>>> from math import factorial
>>> { len(str(factorial(x))) for x in range(20) }
{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15, 16, 18}
>>>
```

The general form of set comprehensions is `{ expr(item) for item in iterable }`.

#### Dictionary Comprehensions
```python
>>> from pprint import pprint as pp
>>> country_to_capital = { 'United Kingdom': 'London',
...                        'Brazil': 'Brazília',
...                        'Morocco': 'Rabat',
...                        'Sweden': 'Stockholm' }
>>> capital_to_country = { capital: country for country, capital in country_to_capital.items() }
>>> pp(capital_to_country)
{'Brazília': 'Brazil',
 'London': 'United Kingdom',
 'Rabat': 'Morocco',
 'Stockholm': 'Sweden'}
>>>
```

The general form of dictionary comprehensions is `{ key_expr:value_expr for item in iterable }`.

Dictionary comprehensions do not usually operate directly on dictionary sources, but they can. Iterating on dictionaries will usually only yield the keys, so if you want both the keys and values, use the `.items()` method as seen in the example above.

Duplicate keys will override previous duplicates in the mapping:
```python
>>> words = ['hi', 'hello', 'foxtrot', 'hotel']
>>> { x[0]: x for x in words }
{'h': 'hotel', 'f': 'foxtrot'}
>>>
```

While there is no technical limit to the complexity of the expression used in comprehensions, for the sake of readability it is best to limit the complexity in a comprehension. Extract out to a sensibly named function for more complex operations.

#### Generator Comprehensions
Generator comprehensions have a similar syntax to [list comprehensions](#list-comprehensions), but they result in the creation of a generator object which produces the specified sequence lazily.

```python
>>> million_squares = (x*x for x in range(1, 1000001))
>>> million_squares
<generator object <genexpr> at 0x7f4984d415e8>
>>> len(list(million_squares))
1000000
>>> list(million_squares) # The generator has already run through, exhausting its items
[]
>>>
```

The general form of generator comprehensions is `( expr(item) for item in iterable )`.

The parentheses used for the generator comprehension can be combined with the parentheses used for a function call increasing readability:
```python
>>> sum(x*x for x in range(1, 1000001))
333333833333500000
>>>
```

#### Filtering Predicates
```python
>>> words = ['hi', 'hello', 'foxtrot', 'hotel']
>>> { x[0]: x for x in words }
{'h': 'hotel', 'f': 'foxtrot'}
>>> 
>>> 
>>> 
>>> from math import sqrt
>>> def is_prime(x):
...     if x < 2:
...             return False
...     for i in range(2, int(sqrt(x)) + 1):
...             if x % i == 0:
...                     return False
...     return True
... 
>>> [x for x in range(101) if is_prime(x)]
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
>>>
```

Comprehensions can be conditionalized with an optional *filtering clause* at the end of the comprehension in the format of `[ expr(item) for item in iterable if predicate(item) ]`.

### Generators
*Generators* are *iterators* that specify *iterable* sequences. They are lazily evaluated, with the next value in the sequence being computed on demand. This property allows them to model infinite sequences, such as data streams, with no definite end.

Generators are composable into pipelines for natural stream processing.

Generator functions are defined as functions which use the `yield` keyword at least once in its definition:
```python
def gen123():
    yield 1
    yield 2
    yield 3
```

Generator functions return *generator objects*, iterators. We can operate on generator objects with the built-in `next()` function. They can also be used in any standard python operations that accept iterators, such as `for` loops and comprehensions.
```python
>>> for val in gen123():
...     print(val)
... 
1
2
3
>>>
```

Each call to a generator function returns a distinct instance of a generator object. Each generator can be advanced independently.

When a generator object is called, the code defined in the generator function runs up to and including the next yield operation.

Generators are *lazy*: the next result is computed just-in-time, when it is requested. This property of generators means that they can be used to model infinite sequences (since values are only produced by the caller and no data structure needs to be built to contain the elements of the sequence). They are ideal for operating on sequences such as:
* Sensor readings
* Mathematical sequences (like primes, or factorials)
* The contents of mult-terabyte files

#### Stateful Generator Functions
Generators resume execution from the last `yield` statement when called. This allows for the maintenance of state in local variables, complex control flow, and lazy evaluation.

[Exercise 11: gen.py](./exercises/11%20-%20stateful%20generator%20functions/gen.py) demonstrates how generator objects can be used in iterables while maintaining internal state over multile calls.

[Exercise 11: gen2.py](./exercises/11%20-%20stateful%20generator%20functions/gen2.py) demonstrates statefulness by maintaining an internal set in the generator function.

[Exercise 11: gen3.py](./exercises/11%20-%20stateful%20generator%20functions/gen3.py) demonstrates composing two generator objects together into a pipe.

[Exercise 11: lucas.py](./exercises/11%20-%20stateful%20generator%20functions/lucas.py) demonstrates iterating over an infinite series using a generator.

## Classes
*Classes* in Python can be used to create custom types. An object's class controls its initialization. Classes are a tool that can make complex problems more tractable, or they can make simple solutions overly complex.

Classes are defined using the `class` keyword. By convention Python uses PascalCase for class names. Class instances are created by executing the class:
```python
class Flight:
    pass

f = Flight()
```

*Methods* are functions defined within a class. *Instance methods* are functions which can be called on an object. The first argument passed to all instance methods is `self`.
```python
class Flight:

    def number(self):
        return "SN060"

f = Flight()
f.number()
```

An instance method invocation is simply syntactic sugar over invoking the class method and passing in the instance:
```python
f.number()

# The above call is equivalent to the following:
Flight.number(f)
```

The initializer for a class is `__init__()`. Like all instance methods, the first argument to `__init__()` must be `self`:
```python
class Flight:

    def __init__(self, number):
        self._number = number

    def number(self):
      return self._number
```

It is convention that implementation detail objects in a class should be stored in a variable that starts with a `_`.

An *initializer* is not a *constructor*. `self` is similiar to `this` in C++ or Java.

**In Python everything is public**. It is through the use of conventions and discipline that developers avoid using methods and objects intended to be private through the use of underscores in the variable names.

*Class invariants* are truths about an object that endure for its lifetime. It is good practice for a class initializer to establish these class invariants.

This is done through validation of initializer arguments:
```python
class Flight:

    def __init__(self, number):
        if not number[:2].isalpha():
            raise ValueError("No airline code in {}".format(number))
        if not number[:2].isupper():
            raise ValueError("Invalid airline code {}".format(number))
        if not (number[2:].isdigit() and int(number[2:]) <= 9999):
            raise ValueError("Invalid airline code {}".format(number))

        self._number = number

    def number(self):
        return self._number

    def airline(self):
        return self._number[:2]
```

Now the class invariant of `number` is enforced:
```python
>>> f = Flight("MN035")
>>> f.number()
'MN035'
>>> f = Flight("foobar")
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "<stdin>", line 6, in __init__
ValueError: Invalid airline code foobar
Error in sys.excepthook:
Traceback (most recent call last):
  File "/usr/lib/python3/dist-packages/apport_python_hook.py", line 63, in apport_excepthook
    from apport.fileutils import likely_packaged, get_recent_crashes
  File "/usr/lib/python3/dist-packages/apport/__init__.py", line 5, in <module>
    from apport.report import Report
  File "/usr/lib/python3/dist-packages/apport/report.py", line 30, in <module>
    import apport.fileutils
  File "/usr/lib/python3/dist-packages/apport/fileutils.py", line 23, in <module>
    from apport.packaging_impl import impl as packaging
  File "/usr/lib/python3/dist-packages/apport/packaging_impl.py", line 24, in <module>
    import apt
  File "/usr/lib/python3/dist-packages/apt/__init__.py", line 23, in <module>
    import apt_pkg
ModuleNotFoundError: No module named 'apt_pkg'

Original exception was:
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "<stdin>", line 6, in __init__
ValueError: Invalid airline code foobar
>>>
```

Inheritance in Python is achieved by passing the base class to the child class definition:
```python
class Aircraft:

    def num_seats(self):
        rows, row_seats = self.seating_plan()
        return len(rows) * len(row_seats)


class AirbusA319(Aircraft):

    def __init__(self, registration):
        self._registration = registration

    def registration(self):
        return self._registration

    def model(self):
        return "Airbus A319"

    def seating_plan(self):
        return range(1, 23), "ABCDEF"
```

The child will inherit methods from the parent class:
```python
>>> a = AirbusA319("G-EZBT")
>>> a.num_seats()
132
>>>
```

## Files And Resource Management
To open a file use the built-in [open()](https://docs.python.org/3/library/functions.html#open) function. It is a good idea to always specify an encoding - do not rely on the default encoding.

Python distinguishes beteween files opened in binary and text modes. Files opened in *binary* mode return and manipulate their content as `bytes` objects without any decoding. Binary mode files reflect the raw data in the file. A file opened in *text* mode treats its contents as if it contains text strings of the `str` type. By default *text* mode provides support for Python's [universal newlines](https://docs.python.org/2.3/whatsnew/node7.html).

### Reading And Writing Text Files
**Reading A File**
```python
f = open("wasteland.txt", mode="rt", encoding="utf-8")
f.read(32)    # Reads the next (first) 32 characters of the file
f.read()      # Reads all remaining text in a file
f.read()      # After the end of file, further calls to read returns an empty string ''
f.seek(0)     # Moves the filepointer back to the passed-in offset from the start of the file
f.readline()  # Returns up to and including the next newline character, or end-of-file
f.seek(0)
f.readlines() # Returns a list of strings of all lines in the file, read
f.close()     # Remember to close the file after reading
```

**Writing A File**
```python
g = open("wasteland.txt", mode="wt", encoding="utf-8")
g.write("What are the roots that clutch, ")
g.write("what branches grow\n")
g.write("Out of this stony rubbish?")
g.close()
```

In the open mode argument: `w` stands for *write* and `t` stands for *text mode*.

Full list of [file open modes](https://docs.python.org/3/library/functions.html#open) are available in the Python documentation.

It is the caller's responsibility to specify newlines where needed - there is no write-line method.

**Appending A File**
```python
h = open("wasteland.txt", mode="at", encoding="utf-8")

# Write an iterable series of lines to a file
h.writelines(
    ["Son of man,\n",
     "You cannot say, or guess, ",
     "for you know only,\n",
     "A head of broken images, ",
     "where the sun beats\n"])
)
h.close()
```

When finished, always remember to call the `.close()` method on the file.

It is a good practice to wrap file operatations with `try`/`finally` blocks to ensure a file is always closed, even in exceptional cases:
```python
def read_series(filename):
    try:
        f = open(filename, mode="rt", encoding="utf-8")
        return [ int(line.strip()) for line in f ]
    finally:
        f.close()
```

`.close()` is required to actually write the data.

Documentation on [file objects](https://docs.python.org/3/glossary.html#term-file-object) can be found in the Python documentation.

### Files As Iterators
*File objects* support the [iterator protocol](https://docs.python.org/3/c-api/iter.html). They can be used in for-loops and any other place an iterator can be used.

```python
import sys

def main(filename):
    f = open(filename, mode="rt", encoding="utf-8")
    for line in f:
        sys.stdout.write(line)
    f.close()

if __name__ == "__main__":
  main(sys.argv[1])
```

The above module reads the filename from the command line argument and prints out each line of the specified file to the command line.

### Context Managers
The need for resource cleanup is common enough that Python implements a control-flow structure called *with-blocks* to support it. With-blocks can be used with any object that supports the [context manager protocol](https://www.python.org/dev/peps/pep-0343/), which includes file objects.

Using the `with` operator we can simplify the try-finally convention:
```python
def read_series(filename):
    with open(filename, mode="rt", encoding="utf-8") as f:
        return [ int(line.strip()) for line in f ]
```

The `with` operator ensures that `.close()` is called on the file object, either at the end of the operation or after an exception is raised.

The `with` statement can be used with any type of object that implements the context manager protocol. A list of standard library context manager helpers can be found in the documentation for [contextlib](https://docs.python.org/3/library/contextlib.html).

### File Like Objects
There is a notion in Python of *file-like objects*. This is not a formal as a specific protocol, but thanks to the polymorphism afforded by duck-typing it works well in practice.

## Unit Tests
The Python Standard Library includes the [unittest](https://docs.python.org/3/library/unittest.html) module. Despite its name, this module a flexible framework for automating tests of all sorts: unit tests, integration tests, acceptance tests.

The `unittest` module is built around a handful of key concepts:
* A `TestCase` groups together related test functions
  * Basic unit of test organization in `unittest`
* `fixtures` run code before and/or after each test function
  * Ensures the test environment is in an expected state before each test is run
  * Clean up any resources that may have been used in a test method
* `assertions` are specific tests for conditions and behaviors
  * Can make simple boolean checks (e.g. `x.is_valid()`)
  * Can perform object equality tests (e.g. `x == y`)
  * Can verify the proper exceptions are thrown (e.g. `raise ValueError()`)
  * If an `assertion` fails, the test function fails

Fixtures that are available in the `TestCase` class:
* `setUp`: run before each test method
* `tearDown`: run after each test method

The `setUp` and `tearDown` method names aren't in line with what [PEP 8](https://www.python.org/dev/peps/pep-0008/) prescribes. The `unittest` module predates the parts of PEP 8 that specify the convention of function names.

To run your unit tests call `unittest.main()`: this searched for all `TestCase` subclasses in a module and execute all of their test methods.

To see a working function with accompanying unit test, see [exercise 12](./exercises/12%20-%20unit%20tests/text_analyzer.py). To run it navigate into [exercises/12 - unit tests](./exercises/12%20-%20unit%20tests) and run `python3.7 text_analyzer.py`.

## Python Debugger
Python's standard library includes a powerful debugger called [PDB](https://docs.python.org/3/library/pdb.html).

Usage:
```python
import pdb

pdb.set_trace()
```

The `.set_trace()` method places you into the debugger on the command line. To see available operations in the debugger type in `help`. For help with a specific command call `help` on that command (e.g. `help continue`).

To run PDB as a script, passing a module to that script:
```python
>>> python3.7 -m pdb palindrome.py
```

The `-m` flag tells Python to execute the module `pdb` as a script, the remaining arguments are passed to the script. In this mode PDB will execute the module passed in, pausing before the first line. The `next` command can be used to step through the module.

`continue` can be used to allow the program to run. `CTRL+C` can be used to halt execution again and the PDB prompt will once again become available. `where` can be used to determine where in the code PDB is currently paused at. `list` can be used to show the source code at this line. The `print` command can be used to inspect variables in their current state of execution.

## Virtual Environments
Introductory notes on Python virtual environments in [VirtualEnv notes](../virtualenv). More detailed information can be found in the [*Python: Getting Started* notes](https://github.com/chichiwang/notes/tree/master/languages/python/getting_started#virtual-environments).

## Distributing Programs
The [distutils](https://docs.python.org/3/library/distutils.html) package provides support for building and installing additional modules into a Python installation. It allows you to write a simple Python script which knows how to install your Python modules into any Python installation.

By convention this setup file is called `setup.py` and it exists at the top of your project structure. This script can be executed to perform the installation.

Sample `setup.py`:
```python
from distutils.core import setup

setup(
    name = "palindrome",
    version = "1.0",
    py_modules = ["palindrome"],

    # metadata
    author = "Austin Bingham",
    author_email = "austin@sixty-north.com",
    description = "A module for finding palindromic integers.",
    license = "Public domain",
    keywords = "example",
)
```

`distutils` setup can also bundle your Python script up into packages, that are easy to distribute to others, in a number of distribution formats.

This is done with the `sdist` (source distribution) command:
```console
$ python setup.py sdist --format zip
```

To see all of the formats supported by `sdist` you can use:
```console
$ python setup.py sdist --help-formats
```

To find out more about `distutils`, pass `--help` to `setup.py`:
```console
$ python setup.py --help
```

## Installing Third-Party Modules
Install third parties from the [Python Package Index](https://pypi.org/)(PyPI) using [Pip](https://pypi.org/project/pip/). Pip is included and installed with Python since version 3.4.

Pip can search for and install packages from PyPI (also know as the *CheeseShop*).

For specialist uses such as numerical or scientific computing which rely on the NumPy or SciPy packages, [Anaconda](https://www.anaconda.com/distribution/) may be a good alternative to Pip.
