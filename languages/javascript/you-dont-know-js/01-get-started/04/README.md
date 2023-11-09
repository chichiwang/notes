# Chapter 4: The Bigger Picture
This chapter divides the organization of the JavaScript language into three main pillars.

## Sections
* [Pillar 1: Scope and Closure](#pillar-1-scope-and-closure)

[◂ Return to Table of Contents](../README.md)

## Pillar 1: Scope and Closure
The organization of variables into units of scope is one of the most fundamental characteristics of any language. Scopes nest inside of each other and only variables at any given level of scope (or in higher/outer scopes) are accessible - variables from lower/inner scopes are inacessible. This behavior is called lexical scope: the scope unit boundaries is determined at parse/compile time. Where a function/scope of a program is located is an author-time decision that determines what the scope structure of a particular part of a program will be.

JavaScript is lexically scoped but it contains two characteristics not present in any other lexically scoped languages:
1. _Hoisting_: All variables declared anywhere within a scope are treated as if they are declared at the beginning of the scope.
2. `var`-declared variables are function-scoped, even if they appear within a block.

`let`/`const` declarations have a peculiar error behavior called the _Temporal Dead Zone_ (TDZ) which results in observable but unusable variables.

_Closure_ is a natural result of lexical scope when the language has functions as first-class citizens. Closure is simply when a function is passed as value and executed in other scopes, but the function maintains access to its original scope variables. Across all of programming (and especially in JavaScript) closure drives many of the most important programming patterns, including modules.

[▲ Return to Sections](#sections)

| [Previous: Chapter 3 - Digging to the Roots of JS](../03/README.md) | [Table of Contents](../README.md#table-of-contents) |
