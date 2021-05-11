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
* [Refactoring](#refactoring)

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

## Refactoring
Ideally there would be a way to write test functions as streamlined as the first `TEST-+` that returns a single `T` or `NIL` value, but that also reports on the results of individual test cases as in the second version.

The simplest way to get rid of the duplicated calls to `FORMAT` is to create a new function:
```lisp
(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form))
```

Now `TEST-+` can be written with calls to `REPORT-RESULT` instead of `FOMAT`. It's not a huge improvement, but at least there is a single source of truth for the output logic:
```lisp
(defun test-+ ()
  (report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (report-result (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (report-result (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))
```

The next step is to eliminate the duplication of the test case expression (with its risk of mislabeling the results). Ideally the expression can be treated as both code (to evaluate for the result) and data (to use as the label). This can benefit from a macro that allows the expression to be written:
```lisp
(check (= (+ 1 2) 3))
```

The above expression should be expanded to:
```lisp
(report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
```

Writing such a macro is trivial:
```lisp
(defmacro check (form)
  `(report-result ,form ',form))
```

Now the call to `TEST-+` can be written using `CHECK`:
```lisp
(defun test-+ ()
  (check (= (+ 1 2) 3))
  (check (= (+ 1 2 3) 6))
  (check (= (+ -1 -3) -4)))
```

In the effort to eliminate duplication, the repeated calls to `CHECK` can be eliminated by refactoring `CHECK` to accept an arbitrary number of forms and wrap them each in a call to `REPORT-RESULT`:
```lisp
(defmacro check (&body forms)
  `(progn
    ,@(loop for f in forms collect `(report-result ,f ',f))))
```

This definition uses a common macro idiom of wrapping a `PROGN` around a series of forms in order to turn them into a single form. `,@` can be used to splice in the result of an expression that returns a list of expressions that are themselves generated with a backquote template. With this new version of `CHECK`, `TEST-+` can be written like such:
```lisp
(defun test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))
```

The above code now expands to the following:
```lisp
(defun test-+ ()
  (progn
    (report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
    (report-result (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
    (report-result (= (+ -1 -3) -4) '(= (+ -1 -3) -4))))
```

Running this new iteration of `TEST-+` yields the same results as the second iteration, with a benefit of having a cleaner definition:
```console
CL-USER> (test-+) 
pass ... (= (+ 1 2) 3)
pass ... (= (+ 1 2 3) 6)
pass ... (= (+ -1 -3) -4)
NIL
CL-USER> 
```

Any further changes to how `TEST-+` behaves can be made by changing the definition for `CHECK`.

[▲ Return to Sections](#sections)

| [Previous: Macros: Custom Defined](../08/README.md) | [Table of Contents](../README.md#notes) | Next |
