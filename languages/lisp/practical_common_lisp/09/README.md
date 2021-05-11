# Building a Unit Test Framework
[Chapter Link](http://www.gigamonkeys.com/book/practical-building-a-unit-test-framework.html)

In this chapter we will develop a simple unit testing framework for Lisp. The main design goal of the test framework will be to make it as easy as possible to add new tests, to run various suites of tests, and to track down test failures. The key feature of an automated testing framework is that the framework is responsible for indicating whether all the tests passed. Each test case must be an expression that yields a boolean value: true or false, pass or fail.

The following may be reasonable test cases for the built-in function `+`:
```lisp
(= (+ 1 2) 3)
(= (+ 1 2 3) 6)
(= (+ -1 -3) -4)
```

Functions that have side effects will be tested slightly differently: the test function has to be called, then evidence of the side effects must be found.

The exercise file for this chapter is located at [./test.lisp](./test.lisp).

## Sections
* [Two First Tries](#two-first-tries)

[◂ Return to Table of Contents](../README.md)

## Two First Tries
The simplest implementation of a test runner that works is to write a function that runs the tests cases and `AND`'s the results together:
```lisp
(defun test-+ ()
  (and
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))
```

To run this test suite call `TEST-+`:
```console
CL-USER> (test-+) 
T
CL-USER> 
```

As long as the function `TEST-+` returns `T` the test cases are passing. However, when `TEST-+` returns `NIL`, there is no indication of which test case(s) failed. A simple approach to solving this is to have each test case output its result:
```lisp
(defun test-+ ()
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))
```

This time the reporting displays in a more user friendly manner:
```console
CL-USER> (test-+) 
pass ... (= (+ 1 2) 3)
pass ... (= (+ 1 2 3) 6)
pass ... (= (+ -1 -3) -4)
NIL
CL-USER> 
```

The code itself is messy to look at, however. The repeated calls to `FORMAT` as well as the tedious duplication of the test expression beg refactoring. Another problem is there is no longer a single indicator whether all the test cases passed.

[▲ Return to Sections](#sections)

| [Previous: Macros: Custom Defined](../08/README.md) | [Table of Contents](../README.md#notes) | Next |
