# Python: Getting Started
[Course](https://app.pluralsight.com/library/courses/python-getting-started/table-of-contents)

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
