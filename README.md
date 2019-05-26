# Python Fundamentals
Course: [PluralSight](https://app.pluralsight.com/library/courses/python-fundamentals/table-of-contents)

## Table of Contents
* [Overview](#overview)
* [Getting Started](#getting-started)
  * [The Read-Eval-Print-Loop](#the-read-eval-print-loop)
  * [Significant Whitespace](#significant-whitespace)
  * [Python Culture and The Zen of Python](#python-culture-and-the-zen-of-python)

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
