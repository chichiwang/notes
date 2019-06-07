# Haskell Overview
**Author**: [Chi-chi Wang](https://github.com/chichiwang)

A high level investigation into the Haskell Programming Language. This is meant to be a profile of the language itself more than an investigation of the syntax and operations.

Notes ported over from Google Docs, built between October 15, 2018 and November 5, 2018.

## Table of Contents
* [Profile](#profile)
* [History](#history)
* [Features](#features)
* [Resources](#resources)

## Profile
* Standardized, general purpose, purely functional programming language
* Non-strict semantics
  * Arguments to a function are not evaluated unless they are actually used in the evaluation of the function body
* Strong static typing
* Type system with type inference and lazy evaluation
* One of the youngest children of ML and Lisp
* Particularly useful for programs that manipulate data structures (such as compilers and interpreters), and for concurrent/parallel programming

## History
* **1987**: At the conference on Functional Languages and Computer Architecture (FPCA) in Portland, Oregon a consensus was reached that a committee should be formed to define an open standard for lazy functional languages
* **1990**: Haskell 1.0 was defined
* **1997**: Haskell 98 was specified as a minimal, stable, portable version of the language with a standard library
  * For teaching, and as a base for future extensions
* **2006**: Process of defining a successor to the Haskell 98 standard began
  * Informally named *Haskell Prime*
* **2009**: First revision of Haskell Prime (Haskell 2010) was announced
* **2010**: Haskell 2010 was published

## Features
* Immutable variables (default)
  * Mutable state programmed via monads
* Pure (default)
  * Side effects programmed via monads
* Lazy evaluation
  * Results are only computed if they’re required
* Everything is an expression
* First class functions
* Compiled and Interpreted implementations both available
* Full type inference
  * Type declarations optional
* Pattern matching on data structures
  * Data structures first class
* [Parametric polymorphism](https://en.wikipedia.org/wiki/Parametric_polymorphism)
  * Generic functions that take generic data types
* [Bounded parametric polymorphism](https://en.wikipedia.org/wiki/Bounded_quantification)
  * Placing bounds on the type parameters
  * Requiring types to belong to a [type class](https://en.wikipedia.org/wiki/Type_class)

## Resources
* [Wikipedia](https://en.wikipedia.org/wiki/Haskell_(programming_language))
* [https://wiki.haskell.org/A_brief_introduction_to_Haskell](https://wiki.haskell.org/A_brief_introduction_to_Haskell)
