# A Simple Database
[Chapter Link](http://www.gigamonkeys.com/book/practical-a-simple-database.html)

This chapter follows an exercise to create a simple database in-memory. The purpose of this exercise to show a simple example of what working in Common Lisp looks like. The final code, after following this exercise, is available at [./cd-database.lisp](./cd-database.lisp).

## Sections
* [Creating a Property List](#creating-a-property-list)
* [A List of Lists](#a-list-of-lists)
* [Formatting the Output](#formatting-the-output)
* [Prompting the User for Input](#prompting-the-user-for-input)
* [Saving and Loading the Database](#saving-and-loading-the-database)
* [Querying the Database](#querying-the-database)

[◂ Return to Table of Contents](../README.md)

## Creating a Property List

To create a list, use the `LIST` function:
```lisp
(list 1 2 3)
```

A _property list_ (_plist_) is a list where every other element, starting from the first, is a _symbol_ that describes what the next element in the list is:
```lisp
(list :a 1 :b 2 :c 3)
```

A value can be retrieved by symbol out of a plist with the `GETF` function. This makes a plist a poor man's hash table. Lisp also has hash tables.
```console
* (getf (list :a 1 :b 2 :c 3) :a)
1
* (getf (list :a 1 :b 2 :c 3) :c)
3
```

To create a function that creates a list:
```lisp
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))
```

`DEFUN` defines a function, `make-cd` is the name of the function. This is followed by the parameter list: `title`, `artist`, `rating`, and `ripped`. After the parameter list is the body of the function. This function only has one form in the body: a call to `LIST`.

To use this function:
```console
* (make-cd "Roses" "Kathy Mattea" 7 t)
(:TITLE "Roses" :ARTIST "Kathy Mattea" :RATING 7 :RIPPED T)
*
```

[▲ Return to Sections](#sections)

## A List of Lists

To start recording a list of CDs, a larger construct to hold records is required. A list would make a good choice. Define a variable to create a new list:
```lisp
(defvar *db* nil)
```

`DEFVAR` is used to define a global variable. The asterisks (*) in the variable name are a Lisp convention for global variables. The `PUSH` macro can be used to add items to the list `*db*` - it is a good idea to abstract this addition. To define a function `add-record` that adds a record to the database:
```lisp
(defun add-record (cd) (push cd *db*))
```

CDs can be added to the database like so:
```console
* (add-record (make-cd "Roses" "Kathy Mattea" 7 t))
((:TITLE "Roses" :ARTIST "Kathy Mattea" :RATING 7 :RIPPED T))
* (add-record (make-cd "Fly" "Dixie Chicks" 8 t))
((:TITLE "Fly" :ARTIST "Dixie Chicks" :RATING 8 :RIPPED T)
  (:TITLE "Roses" :ARTIST "Kathy Mattea" :RATING 7 :RIPPED T))
* (add-record (make-cd "Home" "Dixie Chicks" 9 t))
((:TITLE "Home" :ARTIST "Dixie Chicks" :RATING 9 :RIPPED T)
  (:TITLE "Fly" :ARTIST "Dixie Chicks" :RATING 8 :RIPPED T)
  (:TITLE "Roses" :ARTIST "Kathy Mattea" :RATING 7 :RIPPED T))
*
```

The output of each call to `ADD-RECORD` is the return value. `PUSH` returns the new value of the variable it is modifying.

[▲ Return to Sections](#sections)

## Formatting the Output

Entering `*db*` into the REPL will display the value:
```console
* *db*
((:TITLE "Home" :ARTIST "Dixie Chicks" :RATING 9 :RIPPED T)
  (:TITLE "Fly" :ARTIST "Dixie Chicks" :RATING 8 :RIPPED T)
  (:TITLE "Roses" :ARTIST "Kathy Mattea" :RATING 7 :RIPPED T))
*
```

To write a function to make the format prettier:
```lisp
(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))
```

This function works by looping over all of the elements of `*db*` with the `DOLIST` macro, binding each element to the variable `cd` in turn. For reach value of `cd` the `FORMAT` function is used to print it. The second argument to `FORMAT` is a string that can contain both literal text and directives for how to interpolate the rest of its arguments. Format directives start with `~`. The `~a` directive is the _aesthetic directive_: it consumes one element and outputs it in a human readable form:
```console
* (format t "~a" :title)
TITLE
NIL
*
```

The `~t` directive is for tabulating. The `~10t` tells `FORMAT` to emit enough spaces to move to the tenth column before processing the next `~a`:
```console
* (format t "~a:~10t~a" :artist "Dixie Chicks")
ARTIST:   Dixie Chicks
NIL
* 
```

When presented with a `~{`, `FORMAT` expects the next argument to be a list. `FORMAT` will loop over that list, processing the directives between the `~{` and `~}` directives, consuming as many elements of the list as needed each time through the list. The `~%` directive tells `FORMAT` to emit a newline.

Technically, to have `FORMAT` loop over the database `*db*`, `FORMAT` could have been used to loop over every record:
```lisp
(defun dump-db ()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))
```

The output of `DUMP-DB`:
```console
* (dump-db)
TITLE:    Home
ARTIST:   Dixie Chicks
RATING:   9
RIPPED:   T

TITLE:    Fly
ARTIST:   Dixie Chicks
RATING:   8
RIPPED:   T

TITLE:    Roses
ARTIST:   Kathy Mattea
RATING:   7
RIPPED:   T

NIL
*
```

[▲ Return to Sections](#sections)

## Prompting the User for Input
Reading in a single line of text is done with the `READ-LINE` function. The global variable [*query-io*](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node183.html) contains the input stream conntected to the terminal. To create a function to read a line of input from the terminal:

```lisp
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))
```

This `PROMPT-READ` function returns the value returned by the last form `(read-line *query-io*)` which returns the string ready from `*query-io*` (without the trailing newline).

`FORMAT` is used to emit a prompt to `STDOUT`. There is no `~%` in the format string, so the cursor will remain on the same line. The `FORCE-OUTPUT` function starts the remaining output, but immediately returns and does not wait for all output to be done. This call is necessary in some implmentations to ensure that Lisp doesn't wait for a newline before it prints the prompt. The `READ-LINE` function reads a single line of text.

To combine the existing `MAKE-CD` function with `PROMPT-READ`, and build a function that makes a new CD record from data received from user input:
```lisp
(defun prompt-for-cd ()
  (make-cd
    (prompt-read "Title")
    (prompt-read "Artist")
    (prompt-read "Rating")
    (prompt-read "Ripped [y/n]")))
```

The problem with the function definition above is that `PROMPT-READ` returns a string. This is fine for the `Title` and `Artist` fields, but is not ideal for the `Rating` and `Ripped` fields. Use the function `PARSE-INTEGER` to convert a value to an integer. To avoid an exception in the case of conversion errors pass the optional keyworkd argument `:junk-allowed`:
```lisp
(parse-integer (prompt-read "Rating") :junk-allowed t)
```

If `PARSE-INTEGER` cannot coerce an integer value it will return `NIL` rather than a number. Using the `OR` macro, force the default value to be `0` rather than a `NIL` value:
```lisp
(or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
```

To wrap this logic up into a declaratively named function:
```lisp
(defun prompt-integer (prompt)
  (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0))
```

The Common Lisp function `Y-OR-N-P` will read in a boolean value. Wrap that up into a function:
```lisp
(defun prompt-bool (prompt)
  (y-or-n-p prompt))
```

Finally wrap `PROMPT-READ` into a declaratively named function as well:
```lisp
(defun prompt-string (prompt)
  (prompt-read prompt))
```

Now declare a `PROMPT-FOR-CD` function that adds records with the expected value types:
```lisp
(defun promprt-for-cd ()
  (make-cd
    (prompt-string "Title")
    (prompt-string "Artist")
    (prompt-integer "Rating")
    (prompt-bool "Ripped [y/n]")))
```

To create a function to loop over these prompts until the user is finished entering CDs, define a function `ADD-CDS`:
```lisp
(defun add-cds ()
  (loop (add-record (prompt-for-cd))
    (if (not (prompt-bool "Enter Another CD? [y/n]")) (return))))
```

The `LOOP` macro will repeatedly execute a body of expressions until it's exited by a call to `RETURN`.

The `ADD-CDS` function can be used to enter records of cds into `*db*`:
```console
* (add-cds)
Title: Rockin' the Suburbs
Artist: Ben Folds
Rating: 6

Ripped [y/n] (y or n) y

Enter Another CD? [y/n] (y or n) y
Title: Give Us a Break
Artist: Limpopo
Rating: 10

Ripped [y/n] (y or n) y

Enter Another CD? [y/n] (y or n) n
NIL
* 
```

Use `DUMP-DB` to show the contents of the `*db*` after adding the entries:
```console
* (dump-db)
TITLE:    Give Us a Break
ARTIST:   Limpopo
RATING:   10
RIPPED:   T

TITLE:    Rockin' the Suburbs
ARTIST:   Ben Folds
RATING:   6
RIPPED:   T

NIL
*
```

[▲ Return to Sections](#sections)

## Saving and Loading the Datbase
To save the current database (`*db*`) to a file, define a function `SAVE-DB`:
```lisp
(defun save-db (filename)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
  (with-standard-io-syntax
    (print *db* out))))
```

The `WITH-OPEN-FILE` macro opens a file, binds the stream to a variable, executes a set of expressions, then closes the file. It ensures that the fill will close even if something goes wrong while evaluating the body.

The first argument provided to `WITH-OPEN-FILE` is not a function call, but a list whose syntax is defined by `WITH-OPEN-FILE`. It contains the name of the variable (`out`) that will hold the file stream that will be written to within the body of `WITH-OPEN-FILE`, a value that must be the filename, and then some options that control how the file is to be opened. `:direction :output` specifies the file is being opened for writing. `:if-exists :supersede` will overwrite an existing file of the same name if it exists.

The second argument passed to `WITH-OPEN-FILE` is the body of the operation: `(print *db* out)`. Unlike `FORMAT` the `PRINT` function prints Lisp objects in a form that can be ready back in by the Lisp reader. The macro `WITH-STANDDARD-IO-SYNTAX` ensures that certain variables that affect the behavior of `PRINT` are set to their standard values.

The argument passed to `SAVE-DB` should be a string containing the name of the file where the user wants to save the database. The format of the filepath will depend on the operating system.
```console
* (save-db "my-cds.db")
((:TITLE "Roses" :ARTIST "Kathy Mattea" :RATING 9 :RIPPED T)
  (:TITLE "Fly" :ARTIST "Dixie Chicks" :RATING 8 :RIPPED T)
  (:TITLE "Home" :ARTIST "Dixie Chicks" :RATING 9 :RIPPED T)
  (:TITLE "Rockin' the Suburbs" :ARTIST "Ben Folds" :RATING 6 :RIPPED T)
  (:TITLE "Give Us a Break" :ARTIST "Limpopo" :RATING 10 :RIPPED T)
  (:TITLE "Lyle Lovett" :ARTIST "Lyle Lovett" :RATING 9 :RIPPED T))
*
```

The database has now been saved to [./my-cds.db](./my-cds.db).

A function to load the database back into memory:
```lisp
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))
```

This time, the `:direction` option of `WITH-OPEN-FILE` does not need to be specified, the default `:input` will be used. Instead of `PRINT`, `READ` is used to read from the stream `in`. The `WITH-STANDARD-IO-SYNTAX` macro ensures that `READ` is using the same basic syntax that `SAVE-DB` used when `PRINT`ing the data.

The `SETF` macro is Common Lisp's main assignment operator. It sets its first argument to the result of evaluating its second argument. This assignment clobbers whatever was in the variable.

To use `LOAD-DB`:
```console
* (load-db "languages/lisp/practical_common_lisp/03/my-cds.db")
((:TITLE "Roses" :ARTIST "Kathy Mattea" :RATING 9 :RIPPED T)
  (:TITLE "Fly" :ARTIST "Dixie Chicks" :RATING 8 :RIPPED T)
  (:TITLE "Home" :ARTIST "Dixie Chicks" :RATING 9 :RIPPED T)
  (:TITLE "Rockin' the Suburbs" :ARTIST "Ben Folds" :RATING 6 :RIPPED T)
  (:TITLE "Give Us a Break" :ARTIST "Limpopo" :RATING 10 :RIPPED T)
  (:TITLE "Lyle Lovett" :ARTIST "Lyle Lovett" :RATING 9 :RIPPED T))
*
```

[▲ Return to Sections](#sections)

## Querying the Database
The function `REMOVE-IF-NOT` takes a predicate and a list and returns a list containing only the elements that match the predicate (essentially a filter function). This function does not mutate the original list - it returns a new one. The predicate is a function that takes a single argument and returns a boolean value `NIL` for false, and any other value for true.
```console
* (remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 10))
(2 4 6 8 10)
*
```

In the above example, the predicate function `EVENP` returns `T` if its argument is an even number and `NIL` otherwise. The notation `#'` tells Lisp to retrieve a function with the provided name. Without `#'` Lisp would attempt to retrieve the value of a variable with the name instead of the function.

`REMOVE-IF-NOT` could also be passed an anonmymous function. To achieve the same behavior as using `EVENP`:
```console
* (remove-if-not #'(lambda (x) (= 0 (mod x 2))) '(1 2 3 4 5 6 7 8 9 10))
(2 4 6 8 10)
*
```

In this case, the predicate is the following anonymous function:
```lisp
(lambda (x) (= 0 (mod x 2)))
```

`LAMBDA` is not the name of a function, rather it is an indicator used to define an anonymous function. Excepting the lack of a name, a `LAMBDA` expression looks a lot like `DEFUN`: a parameter list followed by the body of the function.

`GETF` can be used to extract named fields from a _plist_. Assuming `cd` is the name of a variable holding a single database record, the expression `(getf cd :artist)` could be used to extract the name of the `Artist`.

The `EQUAL` function, when provided string arguments, compares them character by character. `(equal (getf cd :artist) "Dixie Chicks")` will test whether the `Artist` field of a given `cd` is equal to `"Dixie Chicks."`.

```console
* (remove-if-not
  #'(lambda (cd) (equal (getf cd :artist) "Dixie Chicks")) *db*)
((:TITLE "Fly" :ARTIST "Dixie Chicks" :RATING 8 :RIPPED T)
  (:TITLE "Home" :ARTIST "Dixie Chicks" :RATING 9 :RIPPED T))
*
```

To wrap this expression in a function that takes the name of the artist as argument:
```lisp
(defun select-by-artist (artist)
  (remove-if-not
  #'(lambda (cd) (equal (getf cd :artist) artist))
  *db*))
```

The lambda in the prior expression has access to the variable `artist` despite containing code that will not run until it is invoked in `REMOVE-IF-NOT`. The anonymous function doesn't just save from the need to write a regular function, it also allows the creation of a function that derives part of its meaning (the value of `artist`) from the context in which it is embedded.

`SELECT-BY-ARTIST` is useful, but a function would need to be created for every possible query: `SELECT-BY-TITLE`, `SELECT-BY-RATING`, `SELECT-BY-TITLE-AND-ARTIST`, and so on. Instead a more general `SELECT` can be created that takes a selector function as its argument:
```lisp
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))
```

`SELECT` can now be passed a function to select the field requested:
```console
* (select #'(lambda (cd) (equal (getf cd :artist) "Dixie Chicks")))
((:TITLE "Fly" :ARTIST "Dixie Chicks" :RATING 8 :RIPPED T)
  (:TITLE "Home" :ARTIST "Dixie Chicks" :RATING 9 :RIPPED T))
*
```

To wrap the lambda up into a declarative function:
```lisp
(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))
```

Now a selection by artist can be executed like:
```console
* (select (artist-selector "Dixie Chicks"))
((:TITLE "Fly" :ARTIST "Dixie Chicks" :RATING 8 :RIPPED T)
  (:TITLE "Home" :ARTIST "Dixie Chicks" :RATING 9 :RIPPED T))
*
```

Using this approach, a function would have to be created for every single field a user may want to select by. A better approach would be to write a single general-purpose selector-function generator: that is a function that would generate a selector-function for different fields, or even combinations of fields. To achieve this, a feature called _keyword parameters_ will beed to be leveraged.

In the functions created thus far, a list of paremeters has been specified that are bound to corresponding arguments in the call to the function:
```lisp
(defun foo (a b c) (list a b c))
```

The above function has 3 parameters and must be called with 3 arguments. However, at times it is advantageous to create a function that acceps a varying number of arguments. Keyword parameters is a way to achieve this. A usage of keyword parameters looks like:
```lisp
(defun foo (&key a b c) (list a b c))
```

With the addition of the `&key` at the start of the arguments list, legal function calls to `FOO` can look like:
```console
* (foo :a 1 :b 2 :c 3)
(1 2 3)
* (foo :c 3 :b 2 :a 1)
(1 2 3)
* (foo :a 1 :c 3)
(1 NIL 3)
* (foo)
(NIL NIL NIL)
*
```

The variables `a`, `b`, and `c` are now bound to the values that follow the corresponding symbols in the arguments list. If a particular symbol is not present at all, the corresponding value is set to `NIL`.

In order to distinguish between a `NIL` that was explicitly passed in as argument and a default value `NIL` for a given keyword symbol, rather than passing a list of keyword names to `&key` a list can instead be passed consisting of the name of the parameter, a default value, and another paramter name (called a _supplied-p_ parameter). The supplied-p parameter will be set to true or false depending on whether an argument was actually passed for that keyword parameter in a particular call to the function:
```lisp
(defun foo (&key a (b 20) (c 30 c-p)) (list a b c c-p))
```

The same function calls from earlier now yield:
```console
* (foo :a 1 :b 2 :c 3)
(1 2 3 T)
* (foo :c 3 :b 2 :a 1)
(1 2 3 T)
* (foo :a 1 :c 3)
(1 20 3 T)
* (foo)
(NIL 20 30 NIL)
*
```

A general selector-function generator, called `WHERE` (much like the DSL in SQL databases), is a function that takes four keyword parameters corresponding to the fields in the CD records, and generates a selector function that selects any CDs that match all of the values provided to `WHERE`. It will allow for operations like `(select (where :artist "Dixie Chicks"))` or `(select (where :rating 10 :ripped nil))`:
```lisp
(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
    (and
      (if title    (equal (getf cd :title)  title)  t)
      (if artist   (equal (getf cd :artist) artist) t)
      (if rating   (equal (getf cd :rating) rating) t)
      (if ripped-p (equal (getf cd :ripped) ripped) t))))
```

This function returns an anonymous function that returns the logical `AND` of one clause per field in the CD records. Each clause checks if the appropriate argument was passed in and then either compares to the value in the corresponding field of `cd` record or returns `T` if the parameter wasn't passed in.

Database queries can now be made:
```console
* (select (where :artist "Dixie Chicks"))
((:TITLE "Fly" :ARTIST "Dixie Chicks" :RATING 8 :RIPPED T)
  (:TITLE "Home" :ARTIST "Dixie Chicks" :RATING 9 :RIPPED T))
* (select (where :rating 10 :ripped nil))
NIL
* (select (where :rating 10 :ripped t))
((:TITLE "Give Us a Break" :ARTIST "Limpopo" :RATING 10 :RIPPED T))
*
```

[▲ Return to Sections](#sections)

| [Previous: A Tour of the REPL](../02/README.md) | [Table of Contents](../README.md#notes) | Next |
