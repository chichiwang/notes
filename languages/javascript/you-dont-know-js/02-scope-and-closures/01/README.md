# Chapter 1: What's the Scope?
Creating and working with variables is one of the most foundational actions programmers perform. A set of well-defined rules called _scope_ determine how the JavaScript engine knows which variables are accessible by any given statement and how to handle two variables of the same name.

## Sections
* [About This Book](#about-this-book)

[◂ Return to Table of Contents](../README.md)

## About This Book
The focus of this book will be on the first of the three pillars of Javascript: [the scope system and its function closures](../../01-get-started/04/README.md#pillar-1-scope-and-closure). It will also dive into the power of the module design pattern.

Prior to execution, during parse/compile, the JavaScript engine analyzes the code's placement of variables, functions, and blocks according to the rules of scope. The resulting scope structure is generally unaffected by runtime conditions.

JavaScript functions are _first-class values_, and are assigned and passed around like any other value. Regardless of where a function is eventually executed, it retains its original scope. This is called _closure_.

_Modules_ are a code organization pattern characterized by public methods that have privileged access (via closure) to private variables and functions.

[▲ Return to Sections](#sections)

| [Table of Contents](../README.md#table-of-contents) |
