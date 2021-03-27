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

| [Previous: Variables](../06/README.md) | [Table of Contents](../README.md#notes) | Next |
