# Collections
[Chapter Link](https://gigamonkeys.com/book/collections.html)

Common Lisp provides standard data types that collect multiple values into a single object. Every language approaches collections a little bit differently but basic collection types usually boil down to:
* Integer-indexed array type: _arrays_, _lists_, or _tuples_.
* Table type that can be used to map keys to values: _hash tables_, _associative arrays_, _maps_, _dictionaries_.

Lisp is famous for its list data structure and most Lisp books start their discussion of Lisp's collections with lists. However, it is a mistake to focus too much on lists; while they are a crucial data structure for representing Lisp code as Lisp data, in many situations other data structures are more appropriate.

## Sections
* [Vectors](#vectors)
  * [Subtypes of Vectors](#subtypes-of-vectors)
  * [Vectors as Sequences](#vectors-as-sequences)
* [Sequence Iterating Functions](#sequence-iterating-functions)
  * [Higher-Order Function Variants](#higher-order-function-variants)
* [Whole Sequence Manipulations](#whole-sequence-manipulations)
* [Sorting and Merging](#sorting-and-merging)
* [Subsequence Manipulations](#subsequence-manipulations)
* [Sequence Predicates](#sequence-predicates)
* [Sequence Mapping Functions](#sequence-mapping-functions)

[◂ Return to Table of Contents](../README.md)

## Vectors
Vectors are Common Lisp's basic integer-indexed collection. There are two flavors of vectors:
1. **Fixed-size vectors**: a thin veneer over a a chunk of contiguous memory that holds the vector's elemnts (similar to arrays in languages like Java).
2. **Resizable vectors**: abstracts the actual storage, allowing the the vector to grow/shrink as elements are added/removed (similar to arrays in Perl or Ruby, or lists in Python).

**Fixed-size vectors** are created using the `VECTOR` function, which takes any number of arguments and returns a freshly allocated fixed-size vector containing those arguments:

```console
CL-USER> (vector)
#()
CL-USER> (vector 1)
#(1)
CL-USER> (vector 1 2)
#(1 2)
CL-USER>
```

The `#(...)` syntax is the literal notation for vectors used by the Lisp printer and reader. This syntax allows vectors to be saved and restored by `PRINT`ing them out and `READ`ing them back in. While the `#(...)` syntax can be used to include vectors in the source code, the effects of modifying literal objects are not defined the functions `VECTOR` and `MAKE-ARRAY` are the preferred option for creating vectors that will be modified.

`MAKE-ARRAY` is more general than `VECTOR`. `MAKE-ARRAY` can be used to create arrays of any dimensionality as well as to create both fixed-size and resizable vectors. The one required argument to `MAKE-ARRAY` is a list containing the dimensions of the array. Since a vector is a one-dimensional array, this argument will be a list containing one number (the size of the vector). `MAKE-ARRAY` will also accept a plain number in place of a one-item list.

If no other arguments are passed in, `MAKE-ARRAY` will create a vector with uninitialized elements that must be set before they can be accessed. To create a vector with all of the elements set to a particular value, a keyword argument `:initial-element` can be provided.

```console
CL-USER> (make-array 3)
#(0 0 0)
CL-USER> (make-array 5 :initial-element nil)
#(NIL NIL NIL NIL NIL)
CL-USER>
```

A **resizable vector** is a slightly more complicated object than a fixed-size vector; in addition to keeping track of the memory used to hold the elements and the number of slots available, a resizable vector also keeps track of the number of elements actually stored in the vector. This number is stored in the vector's _fill pointer_ which corresponds to the index of the next position to be filled.

To create a vector will a fill pointer, a named argument `:fill-pointer` is passed to `MAKE-ARRAY`.
```console
CL-USER> (make-array 5 :fill-pointer 0)
#()
CL-USER>
```

The vector looks empty because the fill-pointer is 0. To add an element to the end of the vector, the function `VECTOR-PUSH` can be used. `VECTOR-PUSH` will add an element at the current fill-pointer of a vector and then increment the fill-pointer by one, returning the index where the new element was added.
```console
CL-USER> (defparameter *x* (make-array 5 :fill-pointer 0))
*X*
CL-USER> (vector-push 'a *x*)
0
CL-USER> *x*
#(A)
CL-USER> (vector-push 'b *x*)
1
CL-USER> *x*
#(A B)
CL-USER> (vector-push 'c *x*)
2
CL-USER> *x*
#(A B C)
CL-USER>
```

The function `VECTOR-POP` returns the last filled element of the vector and decrements the fill-pointer by one.
```console
CL-USER> (vector-pop *x*)
C
CL-USER> *x*
#(A B)
CL-USER> (vector-pop *x*)
B
CL-USER> *x*
#(A)
CL-USER> (vector-pop *x*)
A
CL-USER> *x*
#()
CL-USER>
```

Even a vector with a fill-pointer is not completely resizable. The vector `*x*` can hold at most five elements. To create an arbitrarily resizable vector the `MAKE-ARRAY` function needs to be passed the keyword argument `:adjustable`.
```lisp
(make-array 5 :fill-pointer 0 :adjustable t)
```

This creates an _adjustable_ vector whose underlying memory can be resized as needed. To add an element to an adjustable vector the function `VECTOR-PUSH-EXTEND` is used. This function works just like `VECTOR-PUSH` except it will automatically expand the array if the vector is already full (if the fill-pointer is equal to the size of the underlying storage).

### Subtypes of Vectors
The vectors created in the last section are all _generalized_ vectors that can hold any type of objects. It is also possible to create _specialized_ vectors that are restricted to holding certain types of elements. One advantage to specialized vectors is that they may be stored more compactly, and can provide slightly faster access to their elements than generalized vectors.

One type of specialized vector is a _string_: strings are vectors specialized to hold characters. Strings are important enough to get their own read/print syntax in the form of double quotes and their own string-specific functions. Strings are, however, also vectors. All of the functions that take vector arguments can also accept strings as argument.

Literal strings, such as `"Foo"` are like the literal vectors written in the `#(...)` syntax: their size is fixed and they must not be modified. `MAKE-ARRAY` can be used to resizable strings if passed another keyword argument, `:element-type`. This argument takes a _type_ descriptor. To create a string, the symbol `CHARACTER` can be passed in as the element type:
```lisp
(make-array 5 :fill-pointer 0 :adjustable t :element-type 'character)
```

_Bit vectors_ are vectors whose elements are all zeroes and ones. They have a special read/print syntax that looks like `#*00001111` and a fairly large library of functions. The type descriptor passed as the `:element-type` to create a bit vector is the symbol `BIT`.

### Vectors as Sequences
Vectors and lists are the two concrete subtypes of the abstract type _sequence_. The two most basic sequence functions are:
* `LENGTH`: Returns the length of a sequence. For vectors with a fill-pointer, this will be the value of the fill-pointer.
* `ELT`: Short for _element_. Access an individual element of a sequence via an integer index. Will signal an error if the index is out of bounds.

```lisp
(defparameter *x* (vector 1 2 3))

(length *x*) ; ==> 3
(elt *x* 0)  ; ==> 1
(elt *x* 1)  ; ==> 2
(elt *x* 2)  ; ==> 3
(elt *x* 3)  ; ==> error
```

`ELT` is also a `SETF`able place, the value of a particular element can be set:
```lisp
(setf (elt *x* 0) 10)
```

[▲ Return to Sections](#sections)

## Sequence Iterating Functions
Common Lisp provides a large library of sequence functions. One group of basic sequence functions express certain operations on sequences such as finding or filtering specific elements without writing explicit loops.

**Basic sequence functions**
| Name         | Required Arguments           | Returns                                                |
| ------------ | ---------------------------- | ------------------------------------------------------ |
| `COUNT`      | Item and sequence            | Number of times item appears in sequence               |
| `FIND`       | Item and sequence            | Item or `NIL`                                          |
| `POSITION`   | Item and sequence            | Index into sequence or `NIL`                           |
| `REMOVE`     | Item and sequence            | Sequence with instances of item removed                |
| `SUBSTITUTE` | New item, item, and sequence | Sequence with instances of item replaced with new item |

Simple examples of these functions in use:
```lisp
(count 1 #(1 2 1 2 3 1 2 3 4))         ; ==> 3
(remove 1 #(1 2 1 2 3 1 2 3 4))        ; ==> #(2 2 3 2 3 4)
(remove 1 '(1 2 1 2 3 1 2 3 4))        ; ==> (2 2 3 2 3 4)
(remove #\a "foobarbaz")               ; ==> "foobrbz"
(substitute 10 1 #(1 2 1 2 3 1 2 3 4)) ; ==> #(10 2 10 2 3 10 2 3 4)
(substitute 10 1 '(1 2 1 2 3 1 2 3 4)) ; ==> (10 2 10 2 3 10 2 3 4)
(substitute #\x #\b "foobarbaz")       ; ==> "fooxarxaz"
(find 1 #(1 2 1 2 3 1 2 3 4))          ; ==> 1
(find 10 #(1 2 1 2 3 1 2 3 4))         ; ==> NIL
(position 1 #(1 2 1 2 3 1 2 3 4))      ; ==> 0
```

`REMOVE` and `SUBSTITUTE` always return a sequence of the same type as their sequence argument.

These five functions accept keyword arguments that augment their behavior:
| Argument    | Meaning                                                                                                                     | Default |
| ----------- | --------------------------------------------------------------------------------------------------------------------------- | ------- |
| `:test`     | Two-argument function used to compare item (or value extracted by `:key` function) to element.                              | `EQL`   |
| `:key`      | One-argument function to extract key value from actual sequence element. `NIL` means use element as is.                     | `NIL`   |
| `:start`    | Starting index (inclusive) of subsequence.                                                                                  | 0       |
| `:end`      | Ending index (exclusive) of subsequence. `NIL` indicates end of sequence.                                                   | `NIL`   |
| `:from-end` | If true, the sequence will be traversed in reverse order, from end to start.                                                | `NIL`   |
| `:count`    | Number indicating the number of elements to remove or substitute or `NIL` to indicate all (`REMOVE` and `SUBSTITUTE` only). | `NIL`   |

Example usages of keyword arguments:
```lisp
(count "foo" #("foo" "bar" "baz") :test #'string=)                ; ==> 1

(find 'c #((a 10) (b 20) (c 30) (d 40)) :key #'first)             ; ==> (C 30)
(find 'a #((a 10) (b 20) (a 30) (b 40)) :key #'first)             ; ==> (A 10)
(find 'a #((a 10) (b 20) (a 30) (b 40)) :key #'first :from-end t) ; ==> (A 30)

(remove #\a "foobarbaz" :count 1)                                 ; ==> "foobrbaz"
(remove #\a "foobarbaz" :count 1 :from-end t)                     ; ==> "foobarbz"
```

While `:from-end` can't change the results of the `COUNT` function, it does affect the order the elements are passed to any `:test` or `:key` functions, which could possibly have side effects.

```console
CL-USER> (defparameter *v* #((a 10) (b 20) (a 30) (b 40)))
*V*
CL-USER> (defun verbose-first (x) (format t "Looking at ~s~%" x) (first x))
VERBOSE-FIRST
CL-USER> (count 'a *v* :key #'verbose-first)
Looking at (A 10)
Looking at (B 20)
Looking at (A 30)
Looking at (B 40)
2
CL-USER> (count 'a *v* :key #'verbose-first :from-end t)
Looking at (B 40)
Looking at (A 30)
Looking at (B 20)
Looking at (A 10)
2
CL-USER>
```

### Higher-Order Function Variants
For each of the above functions, Common Lisp provides two _higher-order function_ variants that, in place of the item argument, take a function to be called on each element of the sequence.

One set is each basic function with an `-IF` appended to them. These functions count, find, remove, and substitute elements of the sequence for which the function argument returns true.

The other set of variants is each basic function with an `-IF-NOT` suffix. These count, find, remove, and substitute elements of the sequence for which the function does not return true.

These two sets of variants will accept all of the same keyword arguments as their base counterparts execept for `:test` (which is no longer necessary since the main argument is already a function). Given a `:key` argument, the value extracted by the `:key` function is passed to the predicate function instead of the element.

The `REMOVE` family of functions also supports a variant `REMOVE-DUPLICATES` that only accepts one argument, the sequence, which removes all but one instance of each duplicated element. This variant accepts all of the same keyword arguments as `REMOVE` except for `:count` since it always removes all duplicates.

Example usages of these higher-order function variants:
```lisp
(count-if #'evenp #(1 2 3 4 5))         ; ==> 2
(count-if-not #'evenp #(1 2 3 4 5))     ; ==> 3
(position-if #'digit-char-p "abcd0001") ; ==> 4

;; ==> #("foo" "foom")
(remove-if-not #'(lambda (x) (char= (elt x 0) #\f))
  ("foo" "bar" "baz" "foom"))

(count-if #'evenp #((1 a) (2 b) (3 c) (4 d) (5 e)) :key #'first)     ; ==> 2
(count-if-not #'evenp #((1 a) (2 b) (3 c) (4 d) (5 e)) :key #'first) ; ==> 3

;; ==> #("foo" "bar")
(remove-if-not #'alpha-char-p
  #("foo" "bar" "1baz") :key #'(lambda (x) (elt x 0)))

(remove-duplicates #(1 2 1 2 3 1 2 3 4)) ; ==> #(1 2 3 4)
```

[▲ Return to Sections](#sections)

## Whole Sequence Manipulations
Some functions perform operations on an entire sequence (or sequences) at a time:

| Function      | Arguments                                     | Return Value                                                                                    |
| ------------- | --------------------------------------------- | ----------------------------------------------------------------------------------------------- |
| `COPY-SEQ`    | A sequence                                    | A new sequence of the same type as the argument, containing the same elements.                  |
| `REVERSE`     | A sequence                                    | A new sequence of the same type as the argument, containing the same elements in reverse order. |
| `CONCATENATE` | A type descriptor and any number of sequences | A new sequence of the type specified, containing the elements of all sequences provided.        |

Examples of these functions in use:
```lisp
(copy-seq #(1 2 3 2 1)) ; ==> #(1 2 3 2 1)
(copy-seq "foobar")     ; ==> "foobar"

(reverse #(1 2 3 4 5))  ; ==> #(5 4 3 2 1)
(reverse "foobar")      ; ==> "raboof"

(concatenate 'vector #(1 2 3) '(4 5 6))    ; ==> #(1 2 3 4 5 6)
(concatenate 'list #(1 2 3) '(4 5 6))      ; ==> (1 2 3 4 5 6)
(concatenate 'string "abc" '(#\d #\e #\f)) ; ==> "abcdef"
```

[▲ Return to Sections](#sections)

## Sorting and Merging
`SORT` and `STABLE-SORT` allow for the sorting of sequences. Both functions accept a sequence and a two-argument predicate and return a sorted version of the sequence.

`STABLE-SORT` guarantees not to reorder elements considered equivalent by the predicate function. `SORT` only guarantees the result is sorted and may reorder equivalent elements.

`SORT` and `STABLE-SORT` are both _destructive_ functions: they can modify the sequence passed in. If the sequence to be sorted must be preserved, it is advised a copy is used as argument instead. If the original sequence is no longer required, it may be passed in and subsequently overwritten with the return value:
```lisp
(setf my-sequence (sort my-sequence #'string<))
```

Both of these sort functions also accept a keyword argument `:key` which should be a function used to extract the values passed to the sorting predicate in place of the actual elements. The keys are only used to determine the ordering of the elements in the sequence, the returned sorted sequence will contain the actual elements of the argument sequence.

The `MERGE` function accepts two sequences and a predicate function and returns a sequence produced by merging the two sequences, according to the predicate. Like the two sort functions, `MERGE` can accept a `:key` argument. Like `CONCATENATE` the first argument to `MERGE` must be a type descriptor specifying the type of sequence to produce.

```lisp
(merge 'vector #(1 3 5) #(2 4 6) #'<) ; ==> #(1 2 3 4 5 6)
(merge 'list #(1 3 5) #(2 4 6) #'<) ; ==> (1 2 3 4 5 6)
```

[▲ Return to Sections](#sections)

## Subsequence Manipulations
The `SUBSEQ` function will extract a subsequence starting at a particular index:
```lisp
(subseq "foobarbaz" 3)   ; ==> "barbaz"
(subseq "foobarbaz" 3 6) ; ==> "bar"
```

`SUBSEQ` is `SETF`able but will not expand or shrink a sequence:
```lisp
(defparameter *x* (copy-seq "foobarbaz"))

(setf (subseq *x* 3 6) "xxx")  ; subsequence and new value are same length
*x*                            ; ==> "fooxxxbaz"

(setf (subseq *x* 3 6) "abcd") ; new value too long, extra character ignored.
*x*                            ; ==> "fooabcbaz"

(setf (subseq *x* 3 6) "xx")   ; new value too short, only two characters changed
*x*                            ; ==> "fooxxcbaz"
```

The `FILL` function will set multiple elements of a given sequence to a particular value. The required arguments are a sequence and the value with which to fill it. The `:start` and `:end` keyword arguments are used to limit the effects to a given subsequence.

The `SEARCH` function works like `POSITION` except the first argument is a sequence rather than a single item:
```lisp
(position #\b "foobarbaz") ; ==> 3
(search "bar" "foobarbaz") ; ==> 3
```

The `MISMATCH` function accepts two sequences and returns the index of the first pair of mismatched elements. It returns `NIL` if the two sequences match. `MISMATCH` accepts many of the sequence function keyword arguments: `:key`, `:start1`, `:end1`, `:start2`, `:end2`, `:from-end`.
```lisp
(mismatch "foobarbaz" "foom")         ; ==> 3
(mismatch "foobar" "bar" :from-end t) ; ==> 3
```

[▲ Return to Sections](#sections)

## Sequence Predicates
Four functions are used to iterate over sequences testing a boolean predicate. The first argument to these functions is the predicate, the remaining arguments are sequences. The predicate should accept as many arguments as the number of sequences passed to these functions. The elements of the sequences are passed to the predicate (one element from each sequence) until one of the sequences runs out of elements or the overall termination test is met.

| Function   | Termination Test                                                             |
| ---------- | ---------------------------------------------------------------------------- |
| `EVERY`    | Returns false as soon as the predicate fails, else returns true.             |
| `SOME`     | Returns the first non-`NIL` value the predicate returns, else returns false. |
| `NOTANY`   | Returns false as soon as the predicate is satisfied, else returns true.      |
| `NOTEVERY` | Returns true as soon as the predicate fails, else returns false.             |

```lisp
(every #'evenp #(1 2 3 4 5))    ; ==> NIL
(some #'evenp #(1 2 3 4 5))     ; ==> T
(notany #'evenp #(1 2 3 4 5))   ; ==> NIL
(notevery #'evenp #(1 2 3 4 5)) ; ==> T

(every #'> #(1 2 3 4) #(5 4 3 2))    ; ==> NIL
(some #'> #(1 2 3 4) #(5 4 3 2))     ; ==> T
(notany #'> #(1 2 3 4) #(5 4 3 2))   ; ==> NIL
(notevery #'> #(1 2 3 4) #(5 4 3 2)) ; ==> T
```

[▲ Return to Sections](#sections)

## Sequence Mapping Functions
`MAP` takes a _n_-argument function and _n_ sequences. It returns a new sequence containing the result of applying the function to subsequent elements of the sequences. `MAP` needs to be provided a symbol type descriptor for what sort of resulting sequence to generate:
```lisp
(map 'vector #'* #(1 2 3 4 5) #(10 9 8 7 6)) ; ==> #(10 18 24 28 30)
```

`MAP-INTO` is like `MAP` except it places the results of the operation into a sequence passed in as the first argument. The following expression will sum several vectors (`a`, `b`, and `c`) into one:
```lisp
(map-into a #'+ a b c)
```

`MAP-INTO` only affects as many elements as are present in the shortest sequence (including the sequence being mapped into). The number of elements affected is limited by the size of vectors. `MAP-INTO` will not extend an adjustable vector.

`REDUCE` maps over a single sequence and applies a two-argument function to each set of elements in the sequence. `REDUCE` takes the keyword arguments: `:key`, `:from-end`, `:start`, `:end`, and `:initial-value`.
```lisp
(reduce #'+ #(1 2 3 4 5 6 7 8 9 10)) ; ==> 55
(reduce #'max #(5 1 2 8 10 3 7))     ; ==> 10
```

[▲ Return to Sections](#sections)

| [Previous: Numbers, Characters, and Strings](../10/README.md) | [Table of Contents](../README.md#notes) | Next |
