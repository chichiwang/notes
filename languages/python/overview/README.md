# Python Overview
A high level investigation into the Python Programming Language. This is meant to be a profile of the language itself more than an investigation of the syntax and operations.

Notes ported over from Google Docs, built between August 7, 2018 and September 30, 2018.

## Table of Contents
* [Profile](#profile)
* [Semantics](#semantics)
* [Tools/Tooling](#toolstooling)
* [History](#history)
* [Resources](#resources)

## Profile
* Design philosophy: code readability
  * Rejects exuberant syntax
* Core philosophy: Exemplified in the Zen of Python
  * Beautiful is better than ugly.
  * Explicit is better than implicit.
  * Simple is better than complex.
  * Complex is better than complicated.
  * Flat is better than nested.
  * Sparse is better than dense.
  * Readability counts.
  * Special cases aren't special enough to break the rules.
  * Although practicality beats purity.
  * Errors should never pass silently.
  * Unless explicitly silenced.
  * In the face of ambiguity, refuse the temptation to guess.
  * There should be one-- and preferably only one --obvious way to do it.
  * Although that way may not be obvious at first unless you're Dutch.
  * Now is better than never.
  * Although never is often better than *right* now.
  * If the implementation is hard to explain, it's a bad idea.
  * If the implementation is easy to explain, it may be a good idea.
  * Namespaces are one honking great idea -- let's do more of those!
  * Beautiful is better than ugly
  * Explicit is better than implicit
  * Simple is better than complex
  * Complex is better than complicated
  * Readability counts
* Open source
* Interpreted language
* Dynamic type system
  * Also strongly typed: forbidding operations that are not well defined
* Dynamic name resolution
  * Binds method and variable names during execution
* Automatic memory management
* Supports multiple programming paradigms
  * Object-oriented
  * Imperative
  * Functional
  * Procedural
  * Structured
  * Aspect-oriented
* Comes with a large, comprehensive standard library
* Whitespace delimiters: indentation-based
* Does not support Tail-call optimization or first-class continuation and, according to the creator, never will

## Semantics
* Assignment: binding assignment - assigns a variable of a specific name in memory to point to a separate memory location with the value
* The `pass` statement serves as a NOP to create an empty code block
* The `import` statement is used to import modules
* The `==` operator compares by value
* The `is` operator compares identity (by reference)
* Anonymous function/Lambda expression bodies can only be one expression
* Makes a distinction between lists and tuples
  * Lists are mutable (cannot be used as keys of dictionaries)
  * Tuples are immutable (can be used as keys of dictionaries)
* The string format operator `%` and f-string syntaxis used to format strings with variables
* String literals
  * Single quote marks and double quote marks function identically
  * Use backslash as an escape character
  * Triple-quoted strings can span multiple lines
  * Raw strings (prefix a string literal with r) are useful where literal backslashes are common
* Array indexing: zero-indexed
* Array slicing: `a[start:stop]` or `a[start:stop:step]`
  * Each element of a slice is a shallow copy; deep copies can also be made.
* Python methods have an explicit `self`
  * Reference to a Class instance
* List/dictionary comprehensions are favored over filter and map.
* The switch statement does not exist.
* Classes are recommended only if needed.
* Generators are used extensively for more performant operations. See the [itertools](https://docs.python.org/3.7/library/itertools.html) library for efficient looping.
* Python has a large list of alternative, high-performance data structures in the [collections](https://docs.python.org/3.7/library/collections.html) module.

## Tools/Tooling
* PyPy: JIT compiler
* Cython: Translates Python script into C and makes C-level API calls into the Python interpreter
* Pytest/unitest are the two prominent testing libraries
* Extensive libraries exist for data science, machine learning and mathematical/scientific work.

## History
* Conceived in the late 1980s
  * Implementation began in December 1989
* Created by Guido van Rossum, who has since stepped down from leading the direction of the language.
* Named for Monty Python’s Flying Circus
* Successor to [ABC](https://en.wikipedia.org/wiki/ABC_(programming_language))
* Migration from Python 2 to Python 3 was a significant update that has caused controversy and difficult upgrades over the past decade. Python 2 support is ending in 2020.

## Resources
* [Wikipedia](https://en.wikipedia.org/wiki/Python_(programming_language))
