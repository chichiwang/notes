# Python: Getting Started
[Course](https://app.pluralsight.com/library/courses/python-getting-started/table-of-contents)

## Table of Contents

* [Python 2 vs Python 3](#python-2-vs-python-3)
* [Types, Statements, Loops, Exceptions](#types-statements-loops-exceptions)
  * [Integers and Floats](#integers-and-floats)
  * [Strings](#strings)
    * [String Format Functions](#string-format-functions)
    * [Format String Literals](#format-string-literals)
  * [Boolean and None](#boolean-and-none)
  * [If Statements](#if-statements)
    * [Ternary If Statements](#ternary-if-statements)
  * [Lists](#lists)
    * [List Slicing](#list-slicing)
  * [Loops](#loops)
    * [Range](#range)
  * [Break and Continue](#break-and-continue)
  * [While Loops](#while-loops)
  * [Dictionaries](#dictionaries)
  * [Exceptions](#exceptions)
  * [Other Data Types](#other-data-types)
* [Functions, Files, Yield, Lambda](#functions-files-yield-lambda)
  * [Function Arguments](#function-arguments)

## Python 2 vs Python 3
**TLDR**: Just use Python 3.

Python 3 adoption did not go smoothly. Today (May 2019) you will still see Python 2 being used, mostly 2.7 versions.

| Python 2                                         | Python 3                        |
| ------------------------------------------------ | ------------------------------- |
| Last version 2.7.13 released in Dec. 2016        | Released Dec. 2008              |
| Maintained, but no new features                  | New features being added        |
| End of Life (EOL) in 2020                        | Unicode supported by default    |
| Still default on many systems (ie: Ubuntu 18.04) | Cleared some Python 2 confusion |
| Syntax: `print "Hello World"`                    | Syntax: `print("Hello World")`  |
|                                                  | Minor differences from Python 2 |

Not a lot of differences in terms of syntax.

## Types, Statements, Loops, Exceptions

* Dynamically typed
* Types are inferred by the compiler
* Type hinting
  * New feature added in Python 3.5
  * Allows you to annotate types
```python
def add_numbers(a: int, b: int) -> int:
  return a + b
```
  * Only used to inform editors and IDEs
    * Does not prevent code from running if incorrect types are used
  * Important to write a lot of unit tests for Python scripts

### Integers and Floats
Integers are numbers. Floats are decimal numbers.

Defining an integrer:
```python
answer = 42
```

Defining a float:
```python
pi = 3.13159
```

Python 3 also introduces complex numbers as a type.

Python will not throw a type error when doing operations between number types:
```python
answer + pi # 45.14159
```

Casting a value to a type:
```python
int(pi) == 3
float(answer) == 42.0
```

### Strings
Python 3 defaults strings to Unicode text. Python 2 defaults to ASCII.

Strings can be defined with single quotes `'`, double quotes `"`, or three times quotes (single or double) `'''` `"""`:
```python
'Hello World' == "Hello World" == """Hello World"""
```

Three times quotes are often used as function or class documentation. While `#` are used for comments in Python there are no operators for multi-line comments. Three times quotes are used for multi-line strings: it is acceptable to use them as multi-line comments that are not assigned to any variables. Your editor may even recognize it as a comment.

Useful string methods in python:
```python
"hello".capitalize() == "Hello"
"hello".replace("e", "a") == "hallo"
"hello".isalpha() == True
"123".isdigit() == True # Useful when converting to int
"some,csv,values".split(",") == ["some", "csv", "values"]
```

#### String Format Functions
[String format](https://docs.python.org/3.4/library/string.html#string.Formatter.format) functions are used to interpolate values into strings:
```python
name = "World"
machine = "HAL"
"Nice to meet you {0}. I am {1}".format(name, machine)
```

#### Format String Literals
Python 3.6 introduces string interpolation, known as [format string literals](https://docs.python.org/3/reference/lexical_analysis.html#f-strings):
```python
f"Nice to meet you {name}. I am {machine}"
```

### Boolean and None
Variables can be declared as boolean by assigning them as `True` or `False`:
```python
python_course = True
java_course = False
```

Booleans can be converted to integer or string:
```python
int(python_course) == 1
int(java_course) == 0
str(python_course) == "True"
```

`None` is similar to `null` in other languages:
```python
aliens_found = None
```

`None` is useful as a placeholder value, for variables that are defined in advance but not yet assigned a value. `None` evaluates to `False` in conditional statements.

`None`'s type is called `NoneType`:
```python
type(None) # <class 'NoneType'>
```

### If Statements
Normal `if` statement in python:
```python
number = 5
if number == 5:
  print("Number is 5")
else:
  print("Number is NOT 5")
```

`if` and `else` statements both end with a colon `:`. This is a requirement in python.

We use the `==` to check for equality. We can also use the operator `is` to see if two objects are pointing to the same value in memory.

Python can also evaluate a value for truthiness and falsiness:
* Any number other than `0` has a truthy value
* Any non-empty string has a truthy value
* Any non-empty list has a truthy value
* `None` has a falsey value

`!=` is the conditional operator to check for lack of equality. `not` is the negation operator and `is not` can be used to check the negative case of the `is` operator:
```python
if not python_course:
  print("This statement will NOT evaluate")
```

`and` and `or` operators in python can be used to check multiple conditions:
```python
number = 3
python_course = True

if number == 3 and python_course:
  print("This will evaluate")

if number == 17 or python_course:
  print("This will also evaluate")
```

#### Ternary If Statements
Python does not use the `?` operator like other languages for ternary statements:
```python
a = 1
b = 2
print("bigger" if a > b else "smaller") # "smaller"
```

### Lists
To define a list in python, use the square brackets:
```python
student_names = [] # empty list
student_names = ["Mark", "Katarina", "Jessica"] # populated list
```

Lists are 0-indexed and can be accessed by index:
```python
student_names[0] == "Mark"
student_names[2] == "Jessica"
```

Accessing elements from the end of the list is done with negative indicies. To access the last element in a list use an index of `-1`:
```python
student_names[-1] == "Jessica"
student_names[-3] == "Mark"
```

To add elements to the end of a list, use the `append` method:
```python
student_names.append("Homer")
student_names == ["Mark", "Katarina", "Jessica", "Homer"]
```

To check for the existence of an element in a list use the `in` operator:
```python
"Mark" in student_names == True
```

To check the length of a list use the python function `len()`:
```python
len(student_list) == 4
```

Lists in python are similar to arrays in other languages, but with added benefits: having multiple types in a single list is allowed. As a best practice it may be a good idea to restrict a list to single type - it could help avoid potential bugs.

To remove an element from a list, use the `del` keyword:
```python
del student_names[2]
student_names == ["Mark, "Katarina", "Homer"]
```

#### List Slicing
Slicing lists will not mutate the list. The operation is done with the `:` in the index of a list:
```python
student_names = ["Mark", "Katarina", "Homer"]
student_names[1:] == ["Katarina", "Homer"]
student_names[:-1] == ["Mark", "Katarina"]
student_names[1:-1] == ["Katarina"]
```

### Loops
Syntax for a `for` loop:
```python
for name in student_names:
  print("Student name is {0}".format(name))
```

#### Range
You can also use the `range` function in python:
```python
x = 0
for index in range(10):
  x += 10
  print("The value of x is {0}".format(x))
```

`range()` takes a value and creates a list whose elements begin at `0` and end at `value - 1`:
```python
list(range(10)) == [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```

`range()` also supports two arguments, a starting value and a value to end before:
```python
list(range(5, 10)) == [5, 6, 7, 8, 9]
```

`range()` also supports three arguments, with the last value determines the increments:
```python
list(range(2, 10, 2)) == [2, 4, 6, 8]
```

### Break and Continue
`break` will cause your loop to stop executing and exit without reaching the end of the list or range function:
```python
student_names = ["James", "Katarina", "Jessica", "Mark", "Bort", "Frank Grimes", "Max Power"]

for name in student_names:
  if name == "Mark":
    print("Found him! {0}".format(name))
    break
  print("Currently testing {0}".format(name))
```

`continue` tells a loop to exit the current iteration and continue to the next:
```python
for name in student_names:
  if name == "Bort":
    continue
  print("Currently testing {0}".format(name))
```

### While Loops
`while` loops can also make use of `break` and `continue`. `while` loop syntax:
```python
x = 0
while x < 10:
  print("Count is {0}".format(x))
  x += 1
```

`while` loops check the condition before even entering the loop.

## Dictionaries
Allows the storage of key/value pairs. Very similar to JSON (hash?).

Syntax for defining a dictionary:
```python
student = {
  "id": 15163,
  "name": "Mark",
  "feedback": None
}
```

Dictionaries are very valuable when you need to store structured data. You can group several dictionaries together in a list:
```python
all_students = [
  { "id": 15163, "name": "Mark" }
  { "id": 63112, "name": "Katarina" }
  { "id": 30021, "name": "Jessica" }
]
```

Syntax for reading a dictionary value:
```python
student["name"] == "Mark"
```

Python will raise an exception for non-existent keys:
```python
student["last_name"] == KeyError
```

You can set default values for keys to avoid key errors:
```python
student.get("last_name", "Unknown") == "Unknown"
student["last_name"] == "Unknown"
```

Retrieve a list of keys or values of a dictionary:
```python
student.keys() == ["id", "name", "feedback"]
student.values() == [15163, "Mark", None]
```

Removing a key is the same as removing an element in a list, using the `del` operator:
```python
del student["name"]
student == { "id": 15163, "feedback": None }
```

### Exceptions
Exceptions are events that occur during your program's excution that cause your program to stop executing. It generally means that some error has occurred and your program does not know how to deal with it.

There are ways to handle exceptions:
```python
student = {
  "id": 15163,
  "name": "Mark",
  "feedback": None
}

try:
  last_name = student["last_name"]
except KeyError:
  print("Error finding last_name")

print("The program was not halted by the exeception")
```

Catch multiple exceptions using multiple `except` blocks:
```python
try:
  last_name = student["last_name"]
  numbered_last_name = 3 + last_name
except KeyError:
  print("Error finding last_name")
except TypeError:
  print("Illegal add operation")
```

Catch all exceptions with `except Exception:`. This will handle any exception that comes its way. Generally you do not want to catch general exceptions, you want to catch specific exceptions as a matter of best practice.

You can access the error object this way:
```python
try:
  last_name = student["last_name"]
except KeyError as error:
  print("Error finding last_name")
  print(error)
```

This does not give you access to the full stack trace, for that you would need to use the python `traceback` module. This will be covered in a later section.

You can also raise your own exception, create any exception you want, and use a `finally` handler after any exception that may occur.

### Other Data Types
Overview of other data types in python:
* `complex`
  * Complex numbers
* `long`
  * Only in Python 2
  * Replaced by `integer` in Python 3
* `byes`
  * Sequence of integers in the range of 0..255
  * Sequence of strings or other objects
* `bytearray`
  * Similar to `bytes`
* `tuple`
  * Similar to lists
  * Are immutable
* `set` and `frozenset`
  * Similar to lists, but only have unique objects
  * Can be used to eliminate duplicate elements in a list: `set([3, 2, 3, 1, 5]) == (1, 2, 3, 5)`

## Functions, Files, Yield, Lambda
`def` keyword is used to define a function:
```python
students = []

def add_student(name):
  students.append(name)
```

`return` can be used to return values from a function:
```python
def get_students_titlecase():
  students_titlecase = []
  for student in students:
    students_titlecase.append(student.title())
  return students_titlecase


student_list = get_students_titlecase()
```

If a return value is not specified, `None` is returned.

**It is convention to follow function definitions with 2 newlines.**

### Function Arguments
Arguments are scoped to the function. Arguments are required by default and the Python parser will throw an exception if the arguments specified by the function definition are not provided in the function call.

In order to make an argument optional you must provide a default value:
```python
students = []

def add_student(name, student_id=332):
  students.append({ "id": student_id, "name": name })


add_student("Mark") # Will not throw an exception
```

Named arguments in calling a function specifies the name of the arguments and may lead to legibility for developers:
```python
add_student(name="Mark", student_id=15)
```

A function can be defined to accept a variable number of arguments using `*`:
```python
def var_args(name, *args):
  print(name, args)


var_args("Chi-chi", "Is Learning Python", None, "Hello, World", True)
# Chi-chi ('Is Learning Python', None, 'Hello, World', True)
```
In this setup the arguments are stored in a list and must be iterated over to parse through the contents.

Keyword arguments can be used to define a variable number of named arguments using `**`:
```python
def var_kwargs(name, **kwargs):
  print(name, kwargs)


var_kwargs("Mark", id=15, description="Python Student", feedback=None, pluralsight_subscriber=True)
""" Mark {
  'id': 15,
  'description': 'Python Student',
  'feedback': None,
  'pluralsight_subscriber': True
}
"""
```
Keyword arguments store the named arguments as a dictionary and can be accessed as such.
