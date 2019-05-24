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
  * [Input Function](#input-function)
  * [Nested Functions](#nested-functions)
  * [Basic File Operations](#basic-file-operations)
  * [Generator Functions: Yield](#generator-functions-yield)
  * [Lambda Functions](#lambda-functions)
* [Object Oriented Programming](#object-oriented-programming)
  * [Classes](#classes)
  * [Inheritance and Polymorphism](#inheritance-and-polymorphism)
  * [Modules](#modules)
* [Comment Block Convention](#comment-block-convention)
* [Installing Python Packages](#installing-python-packages)
* [Flask](#flask)
* [Tips And Tricks](#tips-and-tricks)
  * [Virtual Environments](#virtual-environments)
  * [Debugging Python Code](#debugging-python-code)
  * [Creating Executable Files](#creating-executable-files)

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

### Input Function
Python has a built-in [input function](https://docs.python.org/3/library/functions.html#input) to allow users to input values on the command line:
```python
def print_args(**kwargs):
  print(kwargs)


name = input("Name: ")
id = input("ID: ")

print_args(name=name, id=id)
"""
Name: Mark
ID: 15
{'name': 'Mark', 'id': '15'}
"""
```

### Nested Functions
You can nest functions inside of other functions to avoid polluting scope:
```python
def get_students():
  students = ["mark", "james"]
  def get_students_titlecase():
    students_titlecase = []
    for student in students:
      students_titlecase.append(student.title())
    return students_titlecase
  students_titlecase_names = get_students_titlecase()
  print(students_titlecase_names)
```

Function closures exist in Python. Nested inner functions have access to variables defined in their outer functions.

### Basic File Operations
Python's built in `open` function can be used to access files and perform operations:
```python
def save_file(filename, text):
  try:
    f = open(f"{filename}.txt", "a") # "a" is an access mode argument
    f.write(text + "\n")
    f.close()
  except Exception:
    print("Could not save file")
```

Access modes in Python:
| flag | mode                                          |
| ---- | --------------------------------------------- |
| "w"  | write: overwrites the entire file             |
| "r"  | read: read a text file                        |
| "a"  | append: append text to a new or existing file |
| "rb" | read binary: read a binary file               |
| "wb" | write binary: write to a binary file          |

[Other IO operations in Python](https://docs.python.org/3.7/library/io.html#class-hierarchy).

Reading a file in Python:
```pyton
def read_file(filename):
  try:
    f = open(filename, "r")
    text = f.read()
    f.close()
    return text
  except Exception:
    print("Could not read file")
```

It is always a good idea to wrap any file operations in a try-except block.

### Generator Functions: Yield
Resource: [Generators - Python Wiki](https://wiki.python.org/moin/Generators)

Example of building an iterator (generator pattern):
```python
class first_n(object):
  def __init__(self, n):
    self.n = n
    self.num, self.nums = 0, []

  def __iter__(self):
    return self

  # Python 3 compatibility
  def __next__(self):
    return self.next()

  def next(self):
    if self.num < self.n:
      cur, self.num = self.num, self.num + 1
      return cur
    else:
      raise StopIteration()

sum_of_first_n = sum(first_n(1000))
```

Negatives of the above pattern:
* There is a lot of boilerplate
* The logic has to be expressed in a convoluted way

Python provides generator functions as a shortcut to building iterators:
```python
def first_n(n):
  num = 0
  while num < n:
    yield num
    num += 1

sum_of_first_n = sum(first_n(1000))
```

### Lambda Functions
Lambda function notation is supported in Python:
```python
# Standard python function notation:
def double(x):
  return x * 2

# Lambda function notation:
double = lambda x: x * 2
```

Lambda functions are simple 1-liners in Python.

Lambda functions are useful in higher-order functions: functions that take another function as an argument.

## Object Oriented Programming
Python is an object-oriented language. While you are not required to use classes, Python provides them.

There is a debate about whether Python is truly an object-oriented language because it does not provide encapsulation (class methods are not private and hidden from the consumer).

There are no private methods. Most programmers prefix a method with `_` or `__` to indicate a method should not be accessed directly.

### Classes
A logical group of functions and data.

Defining a class:
```python
class Student:
  pass  # in Python pass is a no-op
```

Creating a new instance of a class `Student`:
```python
student = Student()
```

Defining a class with instance and class attributes and methods:
```python
class Student:

  school_name = "Springfield Elementary"

  def __init__(self, name, id):
    self.name = name
    self.id = id

  def __str__(self):
    retrun f"<{self.school_name}> #{self.id}: {self.name}"

mark = Student("Mark", 115)
print(mark) # <Springfield Elementary> #115: Mark
```

Notes:
* `__init__` is the constructor for classes
* All class methods receive `self` as their first argument
  * This is a reference to the class instance
* `__str__` is the built-in to-string method for a class
* Assign values to `self` to create instance attributes
* Class attributes are created by assigning variables inside the class body, but outside of methods
  * Also known as static variables
  * These are shared across all instances of the class
  * Must be accessed via `self` inside of methods (ex: `self.school_name`)
  * Can be directly accessed off of the class itself: `Student.school_name`

### Inheritance and Polymorphism
Defining a child class:
```python
class HighSchoolStudent(Student):

  school_name = "Springfield High"

  def __str__(self):
    return f"[self.school_name] #{self.id}: {self.name}"

  def primary_school(self):
    return super().school_name

james = HighSchoolStudent("James", 312)
print(james) # [Springfield High] #312: James
print(james.primary_school()) # Springfield Elementary
```

Notes:
* Pass the parent class into the class definition to create a child class
* Override parent methods by defining the same method on the child class
* `super()` can be used inside a class method to access the parent class
  * Methods and attributes on the parent class are accessed from the return value: `super().school_name`

### Modules
To import from another file simply import the filename:
```python
import hs_student

james = hs_student.HighSchoolStudent("James")
```


To import just one class from a module:
```python
from hs_student import HighSchoolStudent

james = HighSchoolStudent("James")
```

To import all named entities from a module:
```
from hs_student import *

james = HighSchoolStudent("James")
```

You can import from your own modules as well as standard and third-party libraries.

## Comment Block Convention
Annotating code:
```python
students = []

def add_student(name, id):
  """
  Adds a student dictionary to the students list
  :param name: string - student name
  :param id: integer - student id number
  """

  students.append({ name: name, id: id, school: "Springfield Elementary" })

```

It is convention to annotate your functions and classes using multi-line string blocks. Provide a space between the comment and the function implementation.

## Installng Python Packages
Install Python packages with [pip](https://pypi.org/project/pip/). They are then available for any Python program run with the version and instance of Python that pip installed the dependency to.

## Flask
[Flask](http://flask.pocoo.org/) is a simple package for creating web servers. Flask can render html as well as html containing a templating language called [Jinja2](http://jinja.pocoo.org/docs/2.10/).

## Tips And Tricks

### Virtual Environments
Virtual environments allow you to set up independent Python environments when working with your Python application. This allows you to install different versions of the same depenencies in isolated environments.

By default pip will only install depdencies globally, which can make running different applications that use different versions of the same dependencies impossible without virutal environments.

From [stackoverflow](https://stackoverflow.com/questions/41573587/what-is-the-difference-between-venv-pyvenv-pyenv-virtualenv-virtualenvwrappe):

**[PyPI](https://pypi.org/) packages not in the standard library**

* [virtualenv](https://pypi.python.org/pypi/virtualenv) is a very popular tool that creates isolated Python environments for Python libraries. If you're not familiar with this tool, I highly recommend learning it, as it is a very useful tool, and I'll be making comparisons to it for the rest of this answer.

 It works by installing a bunch of files in a directory (eg: `env/`), and then modifying the `PATH` environment variable to prefix it with a custom `bin` directory (eg: `env/bin/`). An exact copy of the `python` or `python3` binary is placed in this directory, but Python is programmed to look for libraries relative to its path first, in the environment directory. It's not part of Python's standard library, but is officially blessed by the PyPA (Python Packaging Authority). Once activated, you can install packages in the virtual environment using `pip`.

* [pyenv](https://github.com/yyuu/pyenv) is used to isolate Python versions. For example, you may want to test your code against Python 2.6, 2.7, 3.3, 3.4 and 3.5, so you'll need a way to switch between them. Once activated, it prefixes the `PATH` environment variable with `~/.pyenv/shims`, where there are special files matching the Python commands (`python`, `pip`). These are not copies of the Python-shipped commands; they are special scripts that decide on the fly which version of Python to run based on the `PYENV_VERSION` environment variable, or the `.python-version` file, or the `~/.pyenv/version` file. `pyenv` also makes the process of downloading and installing multiple Python versions easier, using the command `pyenv install`.

* [pyenv-virtualenv](https://github.com/yyuu/pyenv-virtualenv) is a plugin for `pyenv` by the same author as `pyenv`, to allow you to use `pyenv` and `virtualenv` at the same time conveniently. However, if you're using Python 3.3 or later, `pyenv-virtualenv` will try to run `python -m venv` if it is available, instead of `virtualenv`. You can use `virtualenv` and `pyenv` together without `pyenv-virtualenv`, if you don't want the convenience features.

* [virtualenvwrapper](https://pypi.python.org/pypi/virtualenvwrapper) is a set of extensions to `virtualenv` (see [docs](http://virtualenvwrapper.readthedocs.io/en/latest/)). It gives you commands like  `mkvirtualenv`, `lssitepackages`, and especially `workon` for switching between different `virtualenv` directories. This tool is especially useful if you want multiple `virtualenv` directories.

* [pyenv-virtualenvwrapper](https://github.com/yyuu/pyenv-virtualenvwrapper) is a plugin for `pyenv` by the same author as `pyenv`, to conveniently integrate `virtualenvwrapper` into `pyenv`.

* [pipenv](https://pypi.python.org/pypi/pipenv) by Kenneth Reitz (the author of `requests`), is the newest project in this list. It aims to combine `Pipfile`, `pip` and `virtualenv` into one command on the command-line. The `virtualenv` directory typically gets placed in `~/.local/share/virtualenvs/XXX`, with `XXX` being a hash of the path of the project directory. This is different from `virtualenv`, where the directory is typically in the current working directory.

The Python Packaging Guide [recommends pipenv](https://packaging.python.org/guides/tool-recommendations/#application-dependency-management) when developing Python applications (as opposed to libraries). There does not seem to be any plans to support `venv` instead of `virtualenv` ([#15](https://github.com/pypa/pipenv/issues/15)). Confusingly, its command-line option `--venv` refers to the `virtualenv` directory, not `venv`, and similarly, the environment variable `PIPENV_VENV_IN_PROJECT` affects the location of the `virtualenv` directory, not `venv` directory ([#1919](https://github.com/pypa/pipenv/issues/1919)).

**Standard library**

* `pyvenv` is a script shipped with Python 3 but [deprecated in Python 3.6](https://docs.python.org/dev/whatsnew/3.6.html#id8) as it had problems (not to mention the confusing name). In Python 3.6+, the exact equivalent is `python3 -m venv`.

* [venv](https://docs.python.org/3/library/venv.html) is a package shipped with Python 3, which you can run using `python3 -m venv` (although for some reason some distros separate it out into a separate distro package, such as `python3-venv` on Ubuntu/Debian). It serves a similar purpose to `virtualenv`, and works in a very similar way, but it doesn't need to copy Python binaries around (except on Windows). Use this if you don't need to support Python 2. At the time of writing, the Python community seems to be happy with `virtualenv` and I haven't heard much talk of `venv`.

Most of these tools complement each other. For instance, `pipenv` integrates `pip`, `virtualenv` and even `pyenv` if desired. The only tools that are true alternatives to each other here are `venv` and `virtualenv`.

**Recommendation for beginners**

This is my personal recommendation for beginners: start by learning `virtualenv` and `pip`, tools which work with both Python 2 and 3 and in a variety of situations, and pick up the other tools once you start needing them.

### Debugging Python Code
Python 3.7 introduces a new `breakpoint()` function through the standard library [pdb](https://docs.python.org/3/library/pdb.html). For more details check the [quick tour post on Hackernoon](https://hackernoon.com/python-3-7s-new-builtin-breakpoint-a-quick-tour-4f1aebc444c).

### Creating Executable Files
Executables of your Python applications can be created using [pyinstaller](https://www.pyinstaller.org/).

To create an exe file, simply enter into the command line while in your project directory:
```bash
$ pyinstaller main.py
```
Replace `main.py` with your own entrypoint file.

Pyinstaller will create a `/build` and `/dist` directory, as well as a `main.spec` file, in your project directory. Inside of the `/dist/main` directory you will find a `main.exe` file.

Building your project this way generates a number of `.pyd` and `.dll` files in your `/dist` directory that are necessary to run your new executable. In order to bundle everything into a single `.exe` file run the following in your command line:
```bash
$ pyinstaller --onefile main.py
```
The `--onefile` flag tells pyinstaller to bundle everything into a single executable. When you run pyinstaller with this flag, the `/dist` directory will only contain a single file: `main.exe`.

This executable file will be very large. This is because the Python interpreter along with required Python packages have to be bundled in with this executable.
