# Variables
[Chapter Link](http://www.gigamonkeys.com/book/variables.html)

Common Lisp supports two kinds of variables: _lexical_ and _dynamic_. These two types correspond roughly to _local_ and _global_ variables in other languages (but not quite).

## Sections
* [Basics](#basics)
* [Lexical Variables](#lexical-variables)
* [Dynamic Variables](#dynamic-variables)

[◂ Return to Table of Contents](../README.md)

## Basics
Common Lisp variables are _dynamically typed_: type errors are detected dynamically. A variable can hold values of any type. The values carry type information that can be used to check types at runtime.

If something other than a number is passed to the `+` function, Common Lisp will signal a type error. Common Lisp is _strongly typed_: all type errors will be detected - there is no way to treat an object as an instance of a class that it is not.

All values in Common Lisp are (conceptually at least) references to objects. Assigning a variable a new value changes what object the variable refers to. It does not have any effect on the previously referenced object. If a variable holds a reference to a mutable object, that reference can be used to modify the object directly.

One way to define new variables is to define function parameters. The parameter list defines the variables that will hold the function's arguments when it is called:
```lisp
(defun foo (x y z) (+ x y z))
```

Every time a function is called, Lisp creates new _bindings_ to hold the arguments passed by the caller. A binding is the runtime manifestation of a variable. A single variable can have many different bindings during a run of the program. A variable can even have multiple bindings at the same time, for example in the parameters of a recursive function.

Another way to create new variables in Lisp is using the `LET` special operator:
```lisp
(let (variable *)
  body-form*)
```

In the form above, `variable` is a variable _initialization form_. Each initialization form is either a list containing a variable name and an initial value or a plain variable name that initializes to `NIL`:
```lisp
(let ((x 10) (y 20) z)
  ...)
```

The above binds the variables `x`, `y`, and `z` to `10`, `20`, and `NIL` respectively. When the `LET` form is evaluated all of the initial value forms are first evaluated before the body forms are excuted. Within the body of the `LET` form the variable names refer to the newly created bindings. After the `LET`, the names refer to whatever they referred to before the `LET` (if anything). The value of the last expression in the body of the the `LET` expression is the return value of the expression.

The _scope_ of function parameters and `LET` variables is delimited by the form that introduces the variable. The form (function definition or `LET`) is known as the _binding form_. The two types of variables (_lexical_ and _dynamic_) use different scoping mechanisms, but in both cases the scope is delimited by the binding form. When variables are introduced with the same name, the bindings of the innermost variable _shadows_ the outer bindings:
```lisp
(defun foo (x)
  (format t "Parameter: ~a~%" x)        ; |<------ x is argument
  (let ((x 2))                          ; |
    (format t "Outer LET: ~a~%" x)      ; | |<---- x is 2
    (let ((x 3))                        ; | |
      (format t "Inner LET: ~a~%" x))   ; | | |<-- x is 3
    (format t "Outer LET: ~a~%" x))     ; | |
  (format t "Parameter: ~a~%" x))       ; |
```

Each reference to `x` will refer to the binding with the smallest enclosing scope. Once control leaves the scope of one binding form, the binding from the immediately enclosing scope is unshadowed and `x` is referred to it instead. Calling `FOO`:
```lisp
* (foo 1)
Parameter: 1
Outer LET: 2
Inner LET: 3
Outer LET: 2
Parameter: 1
NIL
*
```

Any construct that introduces a new variable name that's only usable within the construct is a binding form. `DOTIMES` is a basic counting loop that introduces a variable that holds the value of a counter that increments each time through the loop. In the following example, `DOTIMES` binds the variable `x`:
```lisp
(dotimes (x 10) (format t "~d " x))
```

Another binding form is a variant of `LET`: `LET*`. In a `LET*` expression the initial value forms for each variable can refer to variables intdocued earlier in the variables list:
```lisp
(let* ((x 10)
       (y (+ x 10)))
  (list x y))
```

[▲ Return to Sections](#sections)

## Lexical Variables
By default all binding forms in Common Lisp introduce _lexically scoped_ variables. Lexically scoped variables can only be referred to by code that's textually within the binding form. Anonymous functions within binding calls can also reference variables created in enclosing binding calls:
```lisp
(let ((count 0)) #'(lambda () (setf count (1+ count))))
```

In the above example: The anonoymous function containing the reference to `count` will be returned as the value of the `LET` form, and can be invoked via `FUNCALL` by code not in the scope of the `LET`. In this case the binding of `count` (created when the flow of control entered the `LET` form) will remain as long as a reference exists to the function object returned by the `LET` form. This anonymous function is called a _closure_ because it "closes over" the binding created by the `LET`.

Closures capture the binding (not the value) of variables. Closures can not only access the value of variables it closes over, but it can also assign new values that will persist between calls to the closure:
```console
* (defparameter *fn* (let ((count 0)) #'(lambda () (setf count (1+ count)))))
*FN*
* (funcall *fn*)
1
* (funcall *fn*)
2
* (funcall *fn*)
3
*
```

A single closure can close over many variable bindings simply by referring to them. Multiple closures can capture the same binding. The following expression returns a list of three closures:
```lisp
(let ((count 0))
  (list
   #'(lambda () (incf count))
   #'(labmda () (decf count))
   #'(lambda () count)))
```

[▲ Return to Sections](#sections)

## Dynamic Variables
Lexically scoped bindings help keep code understandable by limiting the scope in which a given name has meaning - this is why most modern languages use lexical scoping for local variables.

Sometimes, however, a global variable is useful. Despite the fact that indiscriminate use of global variables can be problematic for maintenance, there are legitimate uses for global variables and they exist in one form or another in almost all programming languages. Common Lisp's version of global variables, dynamic variables, are both more useful and more manageable.

Common Lisp provides two ways to create global variables: `DEFVAR` and `DEFPARAMETER`:
```lisp
(defvar *count* 0
  "Count of widgets made so far")

(defparameter *gap-tolerance* 0.0001
  "Tolerance to be allowed in widget gaps")
```

Both forms take a variable name, an initial value, and an optional documentation string. Global variables conventionally start and end with `*`. `DEFPARAMETER` always assigns the initial value to the named variable. `DEFVAR` will only do so if the variable is undefined. The `DEFVAR` form can also be used with no initial value to define a global variable without assigning it a value (an _unbound_ variable).

`DEFVAR` can only be used to assign a variable once:
```console
* (defvar *x* 1)
*X*
* (defvar *x* 2)
*X*
* *x*
1
*
```

`DEFPARAMETER` always assigns a value:
```console
* (defparameter *x* 1)
*X*
* (defparameter *x* 2)
*X*
* *x*
2
*
```

Dynamic variables allow for the rebinding of variables in a higher scope for the entire duration of the binding form. Unlike lexical binding, which can be referenced by code only within the lexical scope of a binding form, a dynamic binding can be referenced by any code that is invoked during the execution of the binding form. All global variables are, in fact, dynamic variables.

To temporarily redefine a global variable `*standard-output*`, rebind it with (for example) a `LET`:
```lisp
(let ((*standard-output* *some-other-stream*))
  (stuff))
```

In any code that runs as a result of a call to `STUFF`, references to `*standard-output*` will use the binding established by the `LET`. After `STUFF` returns and control leaves the `LET` block, the new binding of `*standard-output*` will release and subsequent references to `*standard-output*` will see the binding that was current before the `LET`.

At any given time: the most recently established binding shadows all other bindings. Conceptually each new binding for a given dynamic variable is pushed onto a stack of bindings for that variable. References to the variable always use the most recent binding. As binding forms return, the bindings they created are popped off the stack, exposing previous bindings:
```lisp
(defvar *x* 10)
(defun foo () (format t "X: ~d~%" *x*))

(foo)                     ; X: 10
(let ((*x* 20)) (foo))    ; X: 20
(foo)                     ; X: 10

(defun bar ()
  (foo)                   ; X: 10
  (let ((*x* 20)) (foo))  ; X: 20
  (foo))                  ; X: 10
```

As with lexical bindings, assigning a new value affects only the current binding:
```lisp
(defun foo()
  (formt t "Before assignment~18tX: ~d~%" *x*)
  (setf *x* (+1 *x*))
  (format t "After assignment~18tX: ~d~%" *x*))

;; Before assignment X: 10
;; After assignment  X: 11
(foo)

(defun bar ()
  (foo)
  (let ((*x* 20)) (foo))
  (foo))

;; Before Assignment X: 11
;; After assignment  X: 12
;; Before Assignment X: 20
;; After assignment  X: 21
;; Before Assignment X: 12
;; After assignment  X: 13
(bar)
```

Note that in the call `(foo)` the global value of `*x*` was changed from `10` to `11`. The first call to `FOO` from `BAR` increments the global binding to `12`. The second call to `FOO` does not see the global binding due to the `LET` binding form. The last call to `FOO` sees the global binding again and increments it from `12` o `13`.

The name of every variable defined with `DEFVAR` and `DEFPARAMETER` is automatically declared globally special. Whenever this name is used in a binding form (in a `LET`, function parameter, or any other construct that creates a new variable binding) the binding that's created will be a dynamic binding. This is why naming convention is important - it is crucial to distinguish between lexical variables and globally special variables.

It is also possible to declare a name locally special. In a binding form, if a name is declared special, then the binding created for that variable will be dynamic rather than lexical. Other code can locally declare a name special in order to refer to the dynamic binding. Locally special variables are rare.

Binding a global variable has two effects:
1. It can change the behavior of downstream code.
2. It opens up the possiblity that downstream code will assign a new value to a binding established higher up on the stack.

Dynamic variables should only be used when one or both of these characteristics are desired.

[▲ Return to Sections](#sections)

| [Previous: Functions](../05/README.md) | [Table of Contents](../README.md#notes) | Next |
