# Functions
[Chapter Link](http://www.gigamonkeys.com/book/functions.html)

Functions in Common Lisp, like their counterparts in other languages, provide the basic mechanism for abstracting functionality. More than three quarters of the names defined in the Lisp language standard name functions. All of the built-in data types are defined purely in terms of what functions operate on them. Lisp's powerful object system is built upon a conceptual extension to functions (generic functions).

## Sections
* [Defining Functions](#defining-functions)
* [Parameter Lists](#parameter-lists)
  * [Optional Parameters](#optional-parameters)
  * [Rest Parameters](#rest-parameters)
  * [Keyword Parameters](#keyword-parameters)
  * [Mixing Parameter Types](#mixing-parameter-types)
* [Return Values](#return-values)
* [Higher-Order Functions](#higher-order-function)

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

## Parameter Lists
The basic purpose of a parameter list is to declare the variables that will receive the arguments passed to the function. When a parameter list is a simple list of variables names, the parameters are called _required parameters_.

When a function is called it must be supplied with one argument for every required parameter. Each parameter is bound to the corresponding argument.

Lisp checks that the appropriate nunmber of arguments are passed to a function (accounting for optional parameters) and signals an error if the function is called with an incorrect number of arguments.

### Optional Parameters
To define a function with optional parameters: after the names of any required parameters, place the symbol `&optional` followed by the names of optional parameters:
```lisp
(defun foo (a b &optional c d) (list a b c d))
```

When the function is called, arguments are first bound to the required parameters. After this, if there are arguments remaining, thier values are assigned to the optional parameters. If the arguments run out before the optional parameters, the remaining optional parameters are bound to the value `NIL`:
```console
* (defun foo (a b &optional c d) (list a b c d))
FOO
* (foo 1 2)
(1 2 NIL NIL)
* (foo 1 2 3)
(1 2 3 NIL)
* (foo 1 2 3 4)
(1 2 3 4)
*
```

To specify a default value for a parameter define a list containing the parameter name and an expression. The expression will be evaluated only if the caller doesn't pass enough arguments:
```console
* (defun foo (a &optional (b 10)) (list a b))
* (foo 1 2)
(1 2)
* (foo 1)
(1 10)
*
```

The default-value expression can refer to parameters that occur earlier in the parameter list:
```lisp
(defun make-rectangle (width & &optional (height width)) ...)
```

In order to know whether an optional argument was supplied by the caller or if it was assigned the default value, a variable name passed to the default-value expression can provide this information:
```console
* (defun foo (a b &optional (c 3 c-supplied-p))
    (list a b c c-supplied-p))
FOO
* (foo 1 2)
(1 2 3 NIL)
* (foo 1 2 3)
(1 2 3 T)
* (foo 1 2 4)  
(1 2 4 T)
*
```

### Rest Parameters
Some functions need to take a variable number of arguments. Some bult-in functions work this way: `FORMAT` has two required arguments (the stream and control string), after which it takes a variable number of arguments depending on how many values need to be interpolated into the control string. The `+` function also takes a variable number of arguments. The following are all legal calls of these two functions:
```lisp
(format t "hello, world")
(format t "hello, ~a" name)
(format t "x: ~d y: ~d" x y)
(+)
(+ 1)
(+ 1 2)
(+ 1 2 3)
```

Lisp allows the inclusion of a catch-all parameter after the symbol `&rest`. Any arguments remaining after values have been assigned to all required and optional parameters are gathered up into a list that becomes the value of the `&rest` parameter. The parameter lists for `FORMAT` and `+` likely look something like:
```lisp
(defun format (stream string &rest values) ...)
(defun + (&rest numbers) ...)
```

### Keyword Parameters
The problem with optional parameters is that they are positional. Keyword parameters allow the caller to specify which values go with which parameters.

To define keyword parameters, after any required, `&optional`, and `&rest` parameters include the symbol `&key` and then any number of keyword parameter specifiers:
```lisp
(defun foo (&key a b c) (list a b c))
```

When this function is called, each keyword parameter is bound to the value immediate following a keyword of the same name in the arguments list. If a given keyword doesn't appear in the argument list, then the corresponding parameter is assigned its default value. Because the keyword arguments are labeled, they can be passed in in any order as long as they follow any required arguments:
```console
* (foo)
(NIL NIL NIL)
* (foo :a 1)
(1 NIL NIL)
* (foo :b 1)
(NIL 1 NIL)
* (foo :c 1)
(NIL NIL 1)
* (foo :a 1 :c 3)
(1 NIL 3)
* (foo :a 1 :b 2 :c 3)
(1 2 3)
* (foo :a 1 :c 3 :b 2)
(1 2 3)
*
```

As with optional parameters, keyword parameters can provide a default value form and the name of a supplied-p variable:
```lisp
(defun foo (&key (a 0) (b 0 b-supplied-p) (c (+ a b)))
  (list a b c b-supplied-p))

(foo :a 1)           ; (1 0 1 NIL)
(foo :b 1)           ; (0 1 1 T)
(foo :b 1 :c 4)      ; (0 1 4 T)
(foo :a 2 :b 1 :c 4) ; (2 1 4 T)
```

If the keyword the caller uses to specify the parameter needs to be different from the name of the actual parameter, the parameter name can be replaced with another list containing the keyword to use when calling the function and the name to be used for the parameter:
```lisp
(defun foo (&key ((:apple a)) ((:box b) 0) ((:charlie c) 0 c-supplied-p))
  (list a b c c-supplied-p))

(foo :apple 10 :box 20 :charlie 30)    ; (10 20 30 T)
```

This style of declaring keyword parameters is useful for using short variable names internal to the function definition, but using descriptive keywords in the function API.

### Mixing Parameter Types
Parameters must be declared in the following order:
1. The names of required parameters.
2. The names of optional parameters.
3. The name of the rest parameter.
4. The names of keyword parameters.

It is unlike that optional parameters and keyword parameters both need to be employed in a function signature. In these instances, it is likely more useful to just use keyword parameters (they are more flexible and can be added without breaking the existing function API).

If rest parameters and keyword parameters are used in a function, both behaviors occur: all remaining values, including the keywords, are gatherd into a list that's bound to the `&rest` parameter, and the appropriate values are also bound to the `&key` parameters:
```lisp
(defun foo (&rest rest &key a b c) (list rest a b c))

(foo :a 1 :b 2 :c 3)    ; ((:A 1 :B 2 :C 3) 1 2 3)
```

[▲ Return to Sections](#sections)

## Return Values
By default functions return the value of evaluating the last expression in the function body. This is the most common way to return a value from a function.

The `RETURN-FROM` special operator can be used to immediately return any value from the function. `RETURN-FROM` is not actually tied to functions; it is used to return from a block of code defined with the `BLOCK` special operator. However, `DEFUN` automatically wraps the whole function body in a block with the same name as a function so evaluating a `RETURN-FROM` with the name of the function and the return value will cause the function to immediate exit with said value. The first argument to `RETURN-FROM` is the name of the block from which to return, the second argument is the value to return.

The following function uses nested loops to find the first pair of numbers, each less than 10, whose product is greater than the argument:
```lisp
(defun foo (n)
  (dotimes (i 10)
    (dotimes (j 10)
      (when (> (* i j) n)
        (return-from foo (list i j))))))
```

Having to supply the name of the function being returned from is not ideal: if the name of the function changes, the name used in `RETURN-FROM` must also be changed. However, explicit `RETURN-FROM` statements are used much less frequently in Lisp than return statements in C-derived languages (all Lisp expressions, including control constructs such as loops and conditionals, evlaluate to a value).

[▲ Return to Sections](#sections)

## Higher-Order Functions
Situations exist where it is useful to be able to treat functions as data. There are times when it is advantageous to pass one function into another. Callbacks and hooks depend on being able to store references to code in order to run it later.

In Lisp functions are just another kind of object. When a function is created with `DEFUN` two things happen:
* A new function object is created.
* The new function object is given a name.

The special operator `FUNCTION` provides the mechanism for getting a reference to a function object. It takes a single argument and returns the function with that name:

```console
* (defun foo (x) (* 2 x))
FOO
* (function foo)
#<FUNCTION FOO>
*
```

The syntax `#'` is syntactic sugar for `FUNCTION`:
```console
* #'foo
#<FUNCTION FOO>
*
```

Common Lisp provides two functions for invoking a function through a function object:
* `FUNCALL` invokes a function, taking the function object as the first parameter, and the arguments as the rest.
* `APPLY` invokes a function, taking the function object as the first parameter, and a list of arguments to pass to the function as the second parameter.

Using `FUNCALL` the following expressions are equivalent:
```lisp
(foo 1 2 3)
(funcall #'foo 1 2 3)
```

The following function demonstrates an apt use of `FUNCALL`. It accepts a function object as an argument and plots a simple ASCII-art histogram of the values returned by the provided function when it's invoked on the values from `min` to `max`, stepping by `step`:
```lisp
(defun plot (fn min max step)
  (loop for i from min to max by step do
    (loop repeat (funcall fn i) do (format t "*"))
    (format t "~%")))
```

`PLOT` can be called with any function that takes a single numeric argument, such as the built-in function `EXP` that returns the value of _e_ raised to the power of its argument:
```console
* (plot #'exp 0 4 1/2)
*
**
***
*****
********
*************
*********************
**********************************
*******************************************************
NIL
*
```

`APPLY` behaves similar to `FUNCALL` but it accepts arguments differently. The first argument to `APPLY` is the function object to invoke, but the second argument is a list containing all of the arguments to pass to the target function.
```console
* (defvar *plot-arguments* (list #'exp 0 4 1/2))
*PLOT-ARGUMENTS*
* (apply #'plot *plot-arguments*)
*
**
***
*****
********
*************
*********************
**********************************
*******************************************************
NIL
*
```

`APPLY` can also accept "loose" arguments as long as the last argument is a list:
```console
* (defvar *plot-data* (list 0 4 1/2))
*PLOT-DATA*
* (apply #'plot #'exp *plot-data*)
*
**
***
*****
********
*************
*********************
**********************************
*******************************************************
NIL
*
```

[▲ Return to Sections](#sections)

| [Previous: Syntax and Semantics](../04/README.md) | [Table of Contents](../README.md#notes) | Next |
