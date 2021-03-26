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

| [Previous: Variables](../06/README.md) | [Table of Contents](../README.md#notes) | Next |
