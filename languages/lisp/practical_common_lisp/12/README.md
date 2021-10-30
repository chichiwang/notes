# Collections
[Chapter Link](https://gigamonkeys.com/book/they-called-it-lisp-for-a-reason-list-processing.html)

Lists play an important role in the Lisp language. Historically, lists were Lisp's original composite data type. Practically speaking, lists remain an excellent solution to certain problems, such as representing code as data. Generally lists are an excellent data structure for representing any kind of heterogeneous/hierarchical data. They are lightweight and support a functional style of programming.

## Sections
* [Cons Cells](#cons-cells)
* [Lists](#lists)
* [Functional Programming and Lists](#functional-programming-and-lists)
* [Destructive Operations](#destructive-operations)

[◂ Return to Table of Contents](../README.md)

## Cons Cells
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

The values of `CONS` cells can be references to any kind of object. More complex data structures can be be composed out of cons cells by linking them together.

## Lists
Lists are created by linking together cons cells in a chain. The elements of the list are stored in the `CAR`s of the cons cells, the links to subsequent cons cells are stored in the `CDR`s. The last cell in the list chain has a `NIL` value in the `CDR`. The Lisp printer understands this convention and prints such chains of cons cells as parenthesized lists rather than as dotted pairs:

```lisp
(1 nil)                        ; ==> (1)
(cons 1 (cons 2 nil))          ; ==> (1 2)
(cons 1 (cons 2 (cons 3 nil))) ; ==> (1 2 3)
```

The `LIST` function builds cons cells and links them together under the hood. The previous `LIST` expressions are equivalent to the previous `CONS` expressions:

```lisp
(list 1)     ; ==> (1)
(list 1 2)   ; ==> (1 2)
(list 1 2 3) ; ==> (1 2 3)
```

When thinking in terms of lists, the meaningless function names `CAR` and `CDR` do not need to be used, the aliases `FIRST` and `REST` should be used instead:

```lisp
(defparameter *list* (list 1 2 3 4))
(first *list*)                        ; ==> 1
(rest *list*)                         ; ==> (2 3 4)
(first (rest *list*))                 ; ==> 2
```

Because cons cells can hold any type of values, so can lists. A single list can hold objects of different types:

```lisp
(list "foo" (list 1 2) 10) ; ==> ("foo" (1 2) 10)
```

The structure of the above list would look like:
![Linked list example](./linked_list.png)
<br /><br />

Lists can be used to represent trees of arbitrary depth and complexity. They make excellent representations of heterogeneous, hierarchical data. Common Lisp provides a large library of functions for manipulating lists. These functions will be easier to understand in the context of a few ideas borrowed from functional programming.

## Functional Programming and Lists
In [functional programming](../../../../paradigms/declarative/functional/README.md), programs are built entirely out of pure functions (functions that have no side effects and compute their results solely on the values of their arguments). The advantage of the functional style is that it makes programs easier to understand:
* Eliminating side effects eliminates almost all possibilities for [action at a distance](https://en.wikipedia.org/wiki/Action_at_a_distance_(computer_programming)).
* Since the result of a function is determined only by the values of its arguments, its behavior is easier to understand and test.

Functions that deal with numbers are naturally functional since numbers are immutable. A list, however, can be mutated (such as with `SETF`). Lists can be treated as a functional data type if their value is considered to be determined by the elements they contain. Any list consisting of the form `(1 2 3 4)` is functionally equivalent to any other list containing those four values, regardless of what cons cells are actually used to represent the list.

Any function that takes a list as argument and returns a value based solely on the contents of the list can be considered functional. The `REVERSE` function, given the list `(1 2 3 4)` will always return `(4 3 2 1)`. Different calls to `REVERSE` with functionally equivalent lists as the argument will return functionally equivalent result lists. Most of Common Lisp's list-manipulation functions are written in a functional style - this allows them to return results that share cons cells with their arguments.

For example: the function `APPEND` takes any number of list arguments and returns a new list containing the elements of all of its arguments.

```lisp
(append (list 1 2) (list 3 4))  ; ==> (1 2 3 4)
```

`APPEND`, in the above example, creates a a new list with 2 cons cells holding the values `1` and `2`, with the cdr of `2` pointing to the the head of the last argument `(3 4)`. It then returns the cons cell containing `1`. None of the orginal arguments have been modified and the resulting list is `(1 2 3 4)`. `APPEND` must copy all values except for the last argument passed in, but it can return a result that *shares structure* with the last argument provided. There are other functions that take similar advantage of lists' ability to share structure.

Another aspect of functional programming is the use of higher-order functions: functions that treat other functions as data, taking them as arguments or returning them as results.

## Destructive Operations

[▲ Return to Sections](#sections)

| [Previous: Collections](../11/README.md) | [Table of Contents](../README.md#notes) | Next |
