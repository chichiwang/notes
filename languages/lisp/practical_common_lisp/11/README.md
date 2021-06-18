# Collections
[Chapter Link](https://gigamonkeys.com/book/collections.html)

Common Lisp provides standard data types that collect multiple values into a single object. Every language approaches collections a little bit differently but basic collection types usually boil down to:
* Integer-indexed array type: _arrays_, _lists_, or _tuples_.
* Table type that can be used to map keys to values: _hash tables_, _associative arrays_, _maps_, _dictionaries_.

Lisp is famous for its list data structure and most Lisp books start their discussion of Lisp's collections with lists. However, it is a mistake to focus too much on lists; while they are a crucial data structure for representing Lisp code as Lisp data, in many situations other data structures are more appropriate.

## Sections
* [Vectors](#vectors)
  * [Subtypes of Vectors](#subtypes-of-vectors)

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

[▲ Return to Sections](#sections)

| [Previous: Numbers, Characters, and Strings](../10/README.md) | [Table of Contents](../README.md#notes) | Next |
