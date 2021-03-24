# Macros: Standard Control Constructs
[Chapter Link](http://www.gigamonkeys.com/book/macros-standard-control-constructs.html)

Many ideas that originated in Lisp have been incorporated into other languages(the conditional expression, garbage collection, etc). Common Lisp's macro system continues to set it apart from other languages, though. The word _macro_ describes a lot of things in computing to which Common Lisp's macros bear little similarity with.

Commonly the definition of a language can include a standard library of functionality that is mplemented in terms of the "core" language. One advantage of definining languages this way is that it makes them easier to understand and implement. A bigger benefit is that it lends expressiveness to a language, ensuring the language is easy to extend.

Programmers can extend Common Lisp by writing functions or libraries that extend the language for themselves. Macros provide another avenue of doing this as well.

Each macro defines its own syntax, determining how the s-expressions passed to it are turned into Lisp forms. With macros as part of the core language it is possible to build new syntax as part of the standard library rather than having to hardwire them into the core: control constructs such as `WHEN`, `DOLIST`, and `LOOP` as well as definitional forms such as `DEFUN` and `DEFPARAMETER`.

This chapter provides an overview of some of these standard control-construct macros defined by Common Lisp.

| [Previous: Variables](../06/README.md) | [Table of Contents](../README.md#notes) | Next |
