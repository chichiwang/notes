# Functions
[Chapter Link](http://www.gigamonkeys.com/book/functions.html)

Functions in Common Lisp, like their counterparts in other languages, provide the basic mechanism for abstracting functionality. More than three quarters of the names defined in the Lisp language standard name functions. All of the built-in data types are defined purely in terms of what functions operate on them. Lisp's powerful object system is built upon a conceptual extension to functions (generic functions).

## Sections
* [Defining Functions](#defining-functions)

[◂ Return to Table of Contents](../README.md)

## Defining Functions
Normally functions are defined using the `DEFUN` macro:
```lisp
(defun name (parameter*)
  "Optional documentation string."
  body-form*)
```

Any symbol can be used as a function name. Usually function names only contain alphabetic characters and hyphens (though other characters are allowed and are used in certain naming conventions). The most important naming convention is that compound names are constructed with hyphens rather than underscores or inner caps.

Functions that convert one kind of value to another sometimes use `->` in the name. A function that converts strings to widgets might be called `string->widget`.

A function's parameter list defines the variables that will be used to hold the arguments passed to the function when it's called. If the function takes no arguments the list is empty: `()`.

If a string literal follows the parameter list it is a documentation string that will be used to describe the purpose of the function. The `DOCUMENTATION` function can be used to retrieve a function's documentation.

The body of a `DEFUN` consists of any number of Lisp expressions. These are evaluated in order when the function is called, and the value of the last expression is returned as the value of the function. The `RETURN-FROM` special operator can also be used to return immediately from anywhere in a function.

A simple function definition:
```lisp
(defun hello-world () (format t "hello, world"))
```

A slightly more complex function definition:
```lisp
(defun verbose-sum (x y)
  "Sum any two numbers after printing a message."
  (format t "Summing ~d and ~d.~%" x y)
  (+ x y))
```

[▲ Return to Sections](#sections)

| [Previous: Syntax and Semantics](../04/README.md) | [Table of Contents](../README.md#notes) | Next |
