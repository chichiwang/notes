# A Tour of the REPL
[Chapter Link](http://www.gigamonkeys.com/book/lather-rinse-repeat-a-tour-of-the-repl.html)

The first task is to choose a Lisp implementation. Common Lisp is defined by its standard. There is neither a single implementation of Common Lisp controlled by a benevolent dictator nor a canonical implementation controlled by a single organization.

While the Common Lisp standard ensures that programs behave the same across conforming implementations of Common Lisp, many features are intentionally left unspecified in order to allow continuing experimentation by implementers in areas where there isn't consensus about the best way for the language to support them.

## Implementations of Lisp
* [Allegro Common Lisp](https://en.wikipedia.org/wiki/Allegro_Common_Lisp): a commercial software implementation of Common Lisp that ships with an IDE.
* [Embeddable Common Lisp](https://en.wikipedia.org/wiki/Category:Common_Lisp_implementations) (ECL): a small implementation of the ANSI Common Lisp programming language that can be used stand-alone or embedded in extant applications written in C.
* [Steel Bank Common Lisp](https://en.wikipedia.org/wiki/Steel_Bank_Common_Lisp) (SBCL): a high quality, open-source implementation of Common Lisp forked from [CMUCL](https://en.wikipedia.org/wiki/CMU_Common_Lisp).

Check the [full list of Common Lisp implementations](https://en.wikipedia.org/wiki/Category:Common_Lisp_implementations) for more information.

For the purposes of this series of notes, we will be using SBCL.

## Using the REPL
To use the REPL, use the executable `sbcl`:
```console
$ sbcl
This is SBCL 2.0.1.debian, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
* 
```

To exit the REPL, enter the expression `(quit)`.

The `*` is the Lisp prompt. Lisp expressions can be entered to be evaluated. From the environment provided by the REPL, program elements such as variables, functions, classes, and methods can be defined and redefined. Files containing Lisp source code or compiled code can be loaded. The REPL can compile whole files or individual functions. The debugger can be entered, code can be stepped through, and the state of individual Lisp objects can be inspected.

One of the simplest Lisp expressions is a number. To evaluate a number expression, enter the number and press return.

```console
* 10
10
*
```

This object is a _self-evaluating object_ meaning that when given to the evaluator it evaluates to itself.

```console
* (+ 2 3)
5
*
```

Anything in parentheses is a list. In this case it is a list of three elements:
1. The symbole `+`
2. The number `2`
3. The number `3`

Lisp, in general, evaluates lists by treating the first element as the name of a function and the rest of the elements as expressions to be evaluated to yield the arguments to the function. In this instance the `+` symbol names a function that performas addition. `2` and `3` evaluate to themselves and are then passed to the addition function, which returns `5`.

```console
* "hello, world"
"hello, world"
*
```

Strings, like numbers, are self-evaluating objects that have a literal syntax that's understood by Lisp. Lisp reads the double-quoted string and instantiates a string object in memory that, when evaluated, evaluates to itself.

To take it a step further, Common Lisp provides a couple ways to emit output. The most flexible is the [FORMAT](https://lispcookbook.github.io/cl-cookbook/strings.html#with-the-format-function) function. `FORMAT` takes a variable number of arguments, but the only two required arguments are the place to send the output and a string. If `t` is provided as the first argument, `FORMAT` sends the output to STDOUT.

```console
* (format t "hello, world")
hello, world
NIL
*
```

`NIL` is the result of evaluating the `FORMAT` expression, printed by the REPL. A `FORMAT` expression is more interesting for its side effect (printing to STDOUT) than for its return value. Every expression in Lisp evaluates to some result.

While this is an expression in Lisp, it is arguable whether it is a true "program". This expression can be packaged into a function. Functions are one of the basic program building blocks in Lisp and can be defined with a `DEFUN` expression:

```console
* (defun hello-world () (format t "hello, world"))
HELLO-WORLD
*
```

The `HELLO-WORLD` in the output of the `DEFUN` expression is the name of the function. The `()` after the name delimit the parameter list, which is empty in this case because the function takes no arguments. The rest of the expression is the body of the function.

This `DEFUN` expression is just another expression to be read, evaluated, and printed by the REPL. The return value is the name of the function that was just defined. However, like the `FORMAT` expression, `DEFUN` is more interesting for the side effects than for its return value. When the above expression is evaluated, a new function with the body `(format t "hello, world")` is created and given the name `HELLO-WORLD`. The function can then be called:

```console
* (hello-world)
hello, world
NIL
```

The output is the same as when the `FORMAT` expression was evaluated directly, including the `NIL` value printed by the REPL. Functions in Common Lisp automatically return the value of the last expression evaluated.

To create a Common Lisp file, it is customary to create a file with the extension `.lisp`. Some folks use `.cl` instead. Create a file [hello.lisp](./hello.lisp):

```lisp
(defun hello-world ()
  (format t "hello, world"))
```

After restarting the `sbcl` REPL, try invoking the `HELLO-WORLD` function:

```console
* (hello-world)
debugger invoked on a UNBOUND-VARIABLE in thread
#<THREAD "main thread" RUNNING {1000560083}>:

The variable HELLO-WORLD is unbound.

Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

restarts (invokable by number or by possibly-abbreviated name):
  0: [CONTINUE   ] Retry using HELLO-WORLD.
  1: [USE-VALUE  ] Use specified value.
  2: [STORE-VALUE] Set specified value and use it.
  3: [ABORT      ] Exit debugger, returning to top level.

(SB-INT:SIMPLE-EVAL-IN-LEXENV HELLO-WORLD #<NULL-LEXENV>)
0]
```

When trying to handle exceptions (in this case invoking a function that doesn't exist) Lisp does not just bail (throw an exception and unwind the stack). Instead Lisp presents the debugger. In the debugger, Lisp itself is still available so expressions can be evaluated - very useful for examining the state of the program and attempting to fix the isuse.

To exit the debugger press `3` (the `ABORT` option) and press enter.

The previous expression failed to evaluate because Lisp did not have the definition of `HELLO-WORLD` in memory. To load the contents of `hello.lisp` into memory, use the `LOAD` function in the REPL:

```console
* (load "hello.lisp")
T
*
```

The `T` means everything loaded correctly. Loading a file with `LOAD` will evaluate the code in the file. `HELLO-WORLD` should now be defined:

```console
* (hello-world)
hello, world
NIL
*
```

Another way to load a file's definitions is to copile the file first with `COMPILE-FILE` and then `LOAD` the resulting compiled file, called a _FASL file_, which is short for _fast-load file_. `COMPILE-FILE` returns the name of the FASL file, so that result can then be passed to `LOAD`:

```console
* (load (compile-file "hello.lisp))
; compiling file "/notes/languages/lisp/practical_common_lisp/02/hello.lisp" (written 22 FEB 2021 09:26:52 PM):
; compiling (DEFUN HELLO-WORLD ...)

; wrote /notes/languages/lisp/practical_common_lisp/02/hello.fasl
; compilation finished in 0:00:00.005
T
*
```

Even when a Lisp app is deployed, there's often still a way to get to a REPL. It is even possible to use SLIME to connect to a Lisp running on a different machine allowing for the debugging of a remote server. An impressive instance of remote debugging occurred on NASA's 1998 Deep Space 1 mission. A half year after the space craft launched, a bit of Lisp code was going to control the spacecraft for two days wile conducting a sequence of experiments. Unfortunately, a subtle race condition in the code had escaped detection during ground testing and was already in space. When the bug manifested in the wild (100 million miles away from Earth) the team was able to diagnose and fix the running code, allowing the experiments to complete. One of the programmers described it as follows:

> Debugging a program running on a $100M piece of hardware that is 100 million miles away is an interesting experience. Having a read-eval-print loop running on the spacecraft proved invaluable in finding and fixing the problem.

| [Previous: Why Lisp?](../01/README.md) | [Table of Contents](../README.md#notes) | Next |
