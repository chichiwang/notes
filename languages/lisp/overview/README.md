# Lisp Overview
**Author**: [Chi-chi Wang](https://github.com/chichiwang)

A high level investigation into the Lisp Programming Language. This is meant to be a profile of the language itself more than an investigation of the syntax and operations.

Notes ported over from Google Docs, built on October 15, 2018.

## Table of Contents
* [Profile](#profile)
* [History](#history)
* [Major Dialects](#major-dialects)
* [Language Innovations](#language-innovations)

## Profile
* Fully parenthesized prefix notation
  * A mathematical notation in which operators precede their operands
* Originally created as a practical mathematical notation for computer programs
* Influenced by the notation of Alonzo Church’s lambda calculus
* Pioneered many ideas in computer science
  * Tree data structures
  * Automatic storage management
  * Dynamic typing
  * Conditionals
  * Higher order functions
  * Recursion
  * Self hosting compiler
  * Read-eval-print-loop
* Name derives from “LISt Processor”
  * Linked lists are one of Lisp’s major data structures

## History
* Created in 1958 by John McCarthy at MIT
* First implemented by Steve Russell on an IBM 704
* First complete Lisp compiler, written in Lisp, was implemented in 1962 by Tim Hart and Mike Levin at MIT
  * Introduced the Lisp model of incremental compilation: compiled and interpreted functions can intermix freely
* Since inception, Lisp was closely linked with the AI research community, especially PDP-10 (a mainframe computer family)

## Major Dialects
* Two major streams of Lisp development: Common Lisp and Scheme
  * Embody significantly different design choices
* Common Lisp
  * Successor of Maclisp
  * Primary influences: Lisp Machine Lisp, Maclisp, NIL, S-1 Lisp, Spice Lisp, Scheme
  * A general purpose programming language
  * Large language standard
  * Has many built-in data types, functions, macros, object system, etc.
  * Features such as lexical scoping and lexical closures
  * Implementations are available targeting many different platforms and operating systems
  * Unmatched consistency in syntax and good design, as well as true macros[¹](#references)
  * Dynamically typed
  * Mixes functional code with code that has side effects
  * Standard library is quite poor by modern standards (a lot of useful functions are missing)
* Scheme
  * Invented by Guy L. Steele, Jr. and Gerald Jay Sussman
  * Statically scoped
  * Properly tail-recursive
  * Designed to have exceptionally clear and simple semantics
  * Designed to have a few different ways to form expressions
  * A more minimalistic design than Common Lisp
    * Smaller set of standard features
    * Contains implementation features not in specified in Common Lisp
      * Tail-call optimizations
      * Full continuations
  * Supports a variety of programming paradigms
    * Imperative
    * Functional
    * Messaging-passing
* Clojure
  * Recent dialect of Lisp that targets mainly the JVM, the Common Language Runtime (CLR), Python VM, Ruby VM, YARV, and compiling to JS
  * Designed to be a pragmatic general-purpose language
  * Draws considerable influences from Haskell
  * Places a strong emphasis on Immutability
  * Provides access to Java frameworks and libraries
    * Calls to Java can avoid reflection and enable fast primitive operations

---
<a name="references"></a>¹ See [Resources](#resources): Mark Karpov post

## Language Innovations
* First language where the structure of program code is represented faithfully and directly in a standard data structure
  * A quality later dubbed *homoiconicity*
  * Lisp functions can be created, manipulated, altered within a Lisp program without lower level manipulations
* Automatic garbage collection
* Object systems
  * Multiple inheritance
  * Mixins
  * Multimethods
  * Multiple dispatch
  * First-class generic functions

## Resources
* [Wikipedia](https://en.wikipedia.org/wiki/Lisp_(programming_language))
* [Mark Karpov: Lisp and Haskell](https://markkarpov.com/post/lisp-and-haskell.html)
