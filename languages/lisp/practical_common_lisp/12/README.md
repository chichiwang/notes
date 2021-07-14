# Collections
[Chapter Link](https://gigamonkeys.com/book/they-called-it-lisp-for-a-reason-list-processing.html)

Lists play an important role in the Lisp language. Historically, lists were Lisp's original composite data type. Practically speaking, lists remain an excellent solution to certain problems, such as representing code as data. Generally lists are an excellent data structure for representing any kind of heterogeneous/hierarchical data. They are lightweight and support a functional style of programming.

## Sections
* [Lists](#lists)

[◂ Return to Table of Contents](../README.md)

## Lists
The key to understanding lists is to understand they are abstractions built on top of objects that are instances of a more primitive data type. The simpler objects are pairs of values called _cons cells_, after the `CONS` function used to create them.

`CONS` accepts two arguments and returns a new cons cell containing the two values. These values can be references to any kind of object. Unless the second value is `NIL` or another cons cell, a cons is printed as two values in a parentheses separated by a dot (known as a _dotted pair_).

```console
CL-USER> (cons 1 2)
(1 . 2)
CL-USER>
```

The two values in the cons cell are known as the _car_ and _cdr_, named after the functions used to access them. These names started as mnemonic by those implementing Lisp on the IBM 704. They have their origins in the assembly mnemonics used to implement the operations.

```lisp
(car (cons 1 2)) ; ==> 1
(cdr (cons 1 2)) ; ==> 2
```

Both `CAR` and `CDR` are `SETF`able places:

```lisp
(defparameter *cons* (cons 1 2))
*cons*                 ; ==> (1 . 2)
(setf (car *cons*) 10) ; ==> 10
*cons*                 ; ==> (10 . 2)
(setf (cdr *cons*) 20) ; ==> 20
*cons*                 ; ==> (10 . 20)
```

[▲ Return to Sections](#sections)

| [Previous: Collections](../11/README.md) | [Table of Contents](../README.md#notes) | Next |
