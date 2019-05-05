# Python: Getting Started
[Course](https://app.pluralsight.com/library/courses/python-getting-started/table-of-contents)

## Table of Contents

* [Python 2 vs Python 3](#python-2-vs-python-3)
* [Types](#types)
* [Integers and Floats](#integers-and-floats)
* [Strings](#strings)
  * [String Format Functions](#string-format-functions)
  * [Format String Literals](#format-string-literals)
* [Boolean and None](#boolean-and-none)
* [If Statements](#if-statements)
  * [Ternary If Statements](#ternary-if-statements)
* [Lists](#lists)
  * [List Slicing](#list-slicing)

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

## Types

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

### String Format Functions
[String format](https://docs.python.org/3.4/library/string.html#string.Formatter.format) functions are used to interpolate values into strings:
```python
name = "World"
machine = "HAL"
"Nice to meet you {0}. I am {1}".format(name, machine)
```

### Format String Literals
Python 3.6 introduces string interpolation, known as [format string literals](https://docs.python.org/3/reference/lexical_analysis.html#f-strings):
```python
f"Nice to meet you {name}. I am {machine}"
```

## Boolean and None
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

## If Statements
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

### Ternary If Statements
Python does not use the `?` operator like other languages for ternary statements:
```python
a = 1
b = 2
print("bigger" if a > b else "smaller") # "smaller"
```

## Lists
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

### List Slicing
Slicing lists will not mutate the list. The operation is done with the `:` in the index of a list:
```python
student_names = ["Mark", "Katarina", "Homer"]
student_names[1:] == ["Katarina", "Homer"]
student_names[:-1] == ["Mark", "Katarina"]
student_names[1:-1] == ["Katarina"]
```
