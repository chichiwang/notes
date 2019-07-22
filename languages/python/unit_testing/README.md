# Unit Testing With Python - WIP
**Author**: [Chi-chi Wang](https://github.com/chichiwang)

Course: [PluralSight](https://app.pluralsight.com/library/courses/unit-testing-python/table-of-contents)

## Table of Contents
* [Vocabulary](#vocabulary)
* [First Test Case](#first-test-case)
* [Test Runner](#test-runner)

## Vocabulary
A **unit test** checks the behavior of a *system*, *elements of code*, and *behavior of code*. An **element of code** refers to a *method*, a *function*, a *module*, or a *class*.

An **automated test** is designed by a person and runs without intervention, unambiguously reporting the results as *pass* or *fail*. An *automated test* is designed to be run by any developer who has access to your code.

By the strict definition of a *unit test*, a test is **not** a unit test if it uses any external resource, such as the filesystem, a database, or a network.

Other tests that utilize external resources may be referred to as *integration tests*, *functional tests*, or *acceptance tests*.

A **test case** tests a specific behavior of a system. Each *test case* should be able to be run independently of others as many times as necessary, and clearly report a *pass* or *fail* result. It should not contain side effects that other test cases will rely on. The name of a test case should be explanatory to readers of the test, providing clarity to what is being tested.

## First Test Case
A simple [example of a unit test with one test case](./exercises/00%20-%20simple%20test%20case/test_phonebook_00.py) in the [exercises directory](./exercises).

To run the example test case, navigate into the [test directory](./exercises/00%20-%20simple%20test%20case) and run the following command: `python3.7 -m unittest`.

**NOTE**: Within a `unittest` class, a test case method name must begin with `test_` to be picked up by the test runner.

## Test Runner
A **test runner** is a program that executes test cases and reports the results. In the [unittest module](https://docs.python.org/3/library/unittest.html) the command line test runner is built-in: `python3.7 -m unittest`. It will discover and run all of the unit tests in that directory and below.

A test case should be designed independently of how it is going to be run: the developer should not have to care which test runner is going to be used while designing the test case.

A new test case has been added to [exercise 01](./exercises/01%20-%20assertEqual) demonstrating the use of [unittest.assertEqual](https://docs.python.org/3/library/unittest.html#unittest.TestCase.assertEqual):

```python
def test_lookup_entry_by_name(self):
    phonebook = Phonebook()
    phonebook.add("Bob", "12345")
    self.assertEqual("12345", phonebook.lookup("Bob"))
```

The first argument to `assertEqual` is the *expected result* and the second argument is the *actual result*.
