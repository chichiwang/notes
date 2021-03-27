# Macros: Standard Control Constructs
[Chapter Link](http://www.gigamonkeys.com/book/macros-standard-control-constructs.html)

Many ideas that originated in Lisp have been incorporated into other languages(the conditional expression, garbage collection, etc). Common Lisp's macro system continues to set it apart from other languages, though. The word _macro_ describes a lot of things in computing to which Common Lisp's macros bear little similarity with.

Commonly the definition of a language can include a standard library of functionality that is mplemented in terms of the "core" language. One advantage of definining languages this way is that it makes them easier to understand and implement. A bigger benefit is that it lends expressiveness to a language, ensuring the language is easy to extend.

Programmers can extend Common Lisp by writing functions or libraries that extend the language for themselves. Macros provide another avenue of doing this as well.

Each macro defines its own syntax, determining how the s-expressions passed to it are turned into Lisp forms. With macros as part of the core language it is possible to build new syntax as part of the standard library rather than having to hardwire them into the core: control constructs such as `WHEN`, `DOLIST`, and `LOOP` as well as definitional forms such as `DEFUN` and `DEFPARAMETER`.

This chapter provides an overview of some of these standard control-construct macros defined by Common Lisp.

## Table of Contents
* [WHEN](#when)
* [UNLESS](#unless)
* [COND](#cond)
* [AND, OR, and NOT](#and-or-and-not)
* [Loops](#loops)
  * [DOLIST](#dolist)
  * [DOTIMES](#dotimes)
  * [DO](#do)
  * [LOOP](#loop)

[◂ Return to Table of Contents](../README.md)

## WHEN
The `IF` special operator has the following base form:
```lisp
(if condition then-form [else-form])
```

The `condition` is evaluated and, if the value is non-`NIL`, the `then-form` is evaluated and the resulting value is returned. If the `condition` evaluates to a `NIL` value, the `else-form` is evaluated instead. If there is no `else-form` and the `condition` evaluates to `NIL`, then `NIL` is returned:
```lisp
(if (> 2 3) "Yup" "Nope") ; "Nope"
(if (> 2 3) "Yup")        ; NIL
(if (> 3 2) "Yup" "Nope") ; "Yup"
```

However, `IF` is restrictive in that the `then-form` and `else-form` are each restricted to being a single Lisp form. In order to perform a sequence of actions in either clause, they need to be wrapped in some other syntax:
```lisp
(if (spam-p current-message)
    (progn
      (file-in-spam-folder current-message)
      (update-spam-database current-message)))
```

`PROGN` is a special operator that executes any number of forms in order and returns the value of the last form. The above can become tedious to write, however.

The macro `WHEN` alleviates this burden, taking a `condition` that if evaluated to true, will then evaluate all of the lisp forms in the body:
```lisp
(when (spam-p current-message)
  (file-in-spam-folder current-message)
  (update-spam-database current-message))
```

`WHEN` is provided by Common Lisp as a standard macro, but if it wasn't built into Lisp, it could be defined (using the backquote notation discussed in [Chapter 3](../03/README.md#macros)):
```lisp
(defmacro when (condition &rest body)
  `(if ,condition (progn ,@body)))
```

[▲ Return to Sections](#sections)

## UNLESS
The symmetrical counterpart to the `WHEN` macro is `UNLESS` which evaluates the `body` only if the condition evaluates to `NIL`. This standard macro can be defined:
```lisp
(defmacro unless (condition &rest body)
  `(if (not ,condition) (progn ,@body)))
```

[▲ Return to Sections](#sections)

## COND
Another instance where `IF` expressions can get ugly is in multi-branching conditions:
```lisp
(if a
  (do-x)
  (if b
    (do-y)
    (do-z)))
```

This gets even messier when multiple forms are required in the `then-form`s requireing `PROGN`s to resolve. `COND` is Common Lisp's standard macro for expressing multibranch conditionals:
```lisp
(cond
  (condition-form-1 form*)
      .
      .
      .
  (condition-form-N form*))
```

Each element of the body represents one branch pf the conditional and consists of a list containing a `condition-form` and zero or more `form`s to be evaluated if that branch passes the `condition`. The `condition-form`s are evaluated in the order the branches appear in the body until one of them evaluates to true. The remaining forms of that branch are evaluated  and the value of the last form in the branch is returned. If a branch contains no form after the `condition-form`, the value of the `condition-form` is returned. By convention the `condition-form` passed to the last branch of a `COND` expression is `T`:
```lisp
(cond (a (do-x))
      (b (do-y))
      (t (do-z))))
```

[▲ Return to Sections](#sections)

## AND, OR, and NOT
Three useful boolean logic operators available in Lisp are: `AND`, `OR`, and `NOT`.

`NOT` is a function that takes a single argument and inverts its truth value, returning `T` if the argument is `NIL` and `NIL` otherwise.

`AND` and `OR` are standard macros that implement logical conjunction and disjunction of any number of subforms. They are defined as macros so that they can _short-circuit_: they evaluate only as many of their subforms, in left-to-right order, as necessary to determine the overall truth value.

`AND` stops and returns `NIL` as soon as one of its subforms evaluated to `NIL`. If none of its subforms evaluates to `NIL`, `AND` returns the value of the last subform.

`OR` stops as soon as one of its subforms evaluates to non-`NIL` and returns the resulting value. If none of the subforms evaluate to true, `OR` returns `NIL`.

```console
* (not nil)
T
* (not (= 1 1))
NIL
* (and (= 1 2) (= 3 3))
NIL
* (or (= 1 2) (= 3 3))
T
* (and (= 1 1) (+ 2 3))
5
* (and (= 1 1) (not t) (= 2 2))
NIL
* (or (= 0 1) (+ 2 3))
5
*
```

[▲ Return to Sections](#sections)

## Loops
None of Lisp's 25 special operators directly support structured looping. All of Lisp's looping control constructs are macros built on top of a pair of special operators that provide a primitive goto facility.

`DO` is the general looping construct that two other Lisp standard marcos, `DOLIST` and `DOTIMES`, are built on top of. `LOOP` is a standard macro that provides a full-blown mini-language for expressing looping constructs in a non-Lisp-like language.

[▲ Return to Sections](#sections)

### DOLIST
`DOLIST` loops across a list of items, executing the loop body with a variable holding the successive items of the list:
```lisp
(dolist (var list-form)
  body-form*)
```

When the loop starts, the `list-form` is evaluated once to produce a list. Then the body of the loop is evaluated once for each item in the list with the variable `var` holding the value of the item:
```console
* (dolist (x '(1 2 3)) (print x))
1 
2 
3 
NIL
*
```

To break out of a `DOLIST` before the end of the list use `RETURN`:
```console
* (dolist (x '(1 2 3)) (print x) (if (evenp x) (return)))
1 
2 
NIL
*
```

[▲ Return to Sections](#sections)

### DOTIMES
`DOTIMES` is a high level looping construct for counting loops:
```lisp
(dotimes (var count-form)
  body-form*)
```

The `count-form` must evaluate to an integer. Each time through the loop `var` holds successive integers from 0 to one less than that number:
```console
* (dotimes (i 4) (print i))
0 
1 
2 
3 
NIL
*
```

As with `DOLIST`: `RETURN` may be used to break out of the loop early.

[▲ Return to Sections](#sections)

### DO
`DOLIST` and `DOTIMES` are not flexible enough to use for all loops. The general `DO` loop is available for the situations such as stepping multiple variables in parallel, or using an arbitrary expression to test for the end of the loop.

`DO` allows the binding of any number of variables and gives complete control over how they change on each step through the loop. It also accepst the test that determines when to end the loop, as well as a form to evaluate at the end of the loop to generate a return value for the entire `DO` expression.
```lisp
(do (variable-definition*)
    (end-test-form retsult-form*)
  statement*)
```

Each `variable-definition` introduces a variable that will be available in the scope of the body of the loop. The full form of a single variable definition is a list consisting of three elements:
```lisp
(var init-form step-form)
```

The `init-form` is evaluated at the beginning of the loop and the resulting values bound to the variable `var`. Before each subsequent iteration of the loop, the `step-form` will be evaluated and the new value assigned to `var`. The `step-form` is optional and if left out the variable will kep its value from iteration to iteration unless explicitly assigned a new value in the loop body. If the `init-form` is not provided for a variable, that variable will be bound to `NIL`. A planin variable name can be provided as shorthand for a list containing just the name.

At the beginning of each iteration, after all loop variables have been assigned their new values, the `end-test-form` is evaluated. As long as it evaluates to `NIL`, the iteration proceeds, evaluating the `statements` in order. When the `end-test-form` evaluates to true, the `results-forms` are evaluated, and the value of the last result form is returned as the value of the entire `DO` expression.

At each step of the iteration the `step-form`s for all variables are evaluated before assigning any of the values to the variables. Any other loop variable can be referenced in the `step-form`s:
```lisp
(do ((n 0 (1+ n))
     (cur 0 next)
     (next 1 (+ cur next)))
    ((= 10 n) cur))
```

In the above example `(1+ n)`, `next`, and `(+ cur next)` are all evaluated using the old values of `n`, `cur`, and `next`. Only after all of the `step-form`s have been evaluated are the variables given their new values.

Because multiple variables can be stepped, often there is no need for a body at all. The `result-form` may be left out if the loop is being used as a control construct.

Example of a `DO` loop that omits the `result-form`:
```lisp
(do ((i 0 (1+ i)))
    ((>= i 4))
  (print i))
```

Example of a `DO` loop that omits the body (`statement`):
```lisp
(do ((n 0 (1+ n))
     (cur 0 next)
     (next 1 (+ cur next)))
    (= 10 n) cur)
```

Exampe of a `DO` loop that binds no variables:
```lisp
(do ()
    ((> (get-universal-time) *some-future-date*))
  (format t "Waiting~%")
  (sleep 60))
```

[▲ Return to Sections](#sections)

### LOOP
The `LOOP` macro provides an easier way to express looping over various data structures: lists, vectors, hash tables, and packages. It also allows accumulating values in various ways while looping: collecting, counting, summing, minimizing, and maximizing.

The `LOOP` macro comes in two flavors: _simple_ and _extended_.

The _simple_ version is an infinite loop that doesn't bind any variables:
```lisp
(loop
  body-form*)
```

The forms in the body are evaluated each time through the loop, which will iterate forever unless `RETURN` is used to break out.
```lisp
(loop
  (when (> (get-universal-time) *some-future-date*)
    (return))
  (format t "Waiting~%")
  (sleep 60))
```

The _extended_ `LOOP` is quite different: it is distinguished by the use of certain _loop keywords_ that implement a special-purpose language for expressing looping idioms. This syntax is contreversial in the Lisp community. Detractors complain that its syntax is un-Lispy. `LOOP`'s fans assert that complicated looping constructs are difficult enough to understand without wrapping them in `DO`s cryptic syntax.

Here is an example of an idiomatic `DO` loop that collects the numbers from 1 to 10 into a list:
```lisp
(do ((nums nil) (i 1 (1+ i)))
    ((> i 10) (nreverse nums))
  (push i nums))
```

The above example returns the value `(1 2 3 4 5 6 7 8 9 10)`. The `LOOP` version, by contrast, looks like:
```lisp
(loop for i from 1 to 10 collecting i)
```

This example of a `LOOP` sums the first 10 squares:
```lisp
(loop for x from 1 to 10 summing (expt x 2))  ; 385
```

This example counts the number of vowels in a string:
```lisp
(loop for x across "the quick brown fox jumps over the lazy dog"
      counting (find x "aeiou"))
```

The above example returns `11`.

This example computes the eleventh Fibbonaci number:
```lisp
(loop for i below 10
      and a = 9 then b
      and b = 1 then (+ b a)
      finally (return a))
```

The symbols `across`, `and`, `below`, `collecting`, `counting`, `finally`, `for`, `from`, `summing`, `then`, and `to` are some of the _loop keywords_ whose presence identifies these examples as instances of the _extended_ `LOOP`.

While the `LOOP` macro is quite a bit more complicated than macros such as `WHEN` or `UNLESS` it is still just another macro. If it hadn't been included in the standard library, it could be implemented by a programmer and shared via a library.

[▲ Return to Sections](#sections)

| [Previous: Variables](../06/README.md) | [Table of Contents](../README.md#notes) | Next |
