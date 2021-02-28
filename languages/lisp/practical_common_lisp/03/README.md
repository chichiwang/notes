# A Simple Database
[Chapter Link](http://www.gigamonkeys.com/book/practical-a-simple-database.html)

This chapter follows an exercise to create a simple database in-memory. The purpose of this exercise to show a simple example of what working in Common Lisp looks like. The final code, after following this exercise, is available at [./cd-database.lisp](./cd-database.lisp).

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

## Querying the Database

| [Previous: A Tour of the REPL](../02/README.md) | [Table of Contents](../README.md#notes) | Next |
