# Unit Testing With Python - WIP
**Author**: [Chi-chi Wang](https://github.com/chichiwang)

Course: [PluralSight](https://app.pluralsight.com/library/courses/unit-testing-python/table-of-contents)

## Table of Contents
* [Vocabulary](#vocabulary)
* [First Test Case](#first-test-case)
* [Test Runner](#test-runner)
* [Test Suite](#test-suite)
* [Skipping A Test Case](#skipping-a-test-case)
* [setUp and tearDown](#setup-and-teardown)
* [Anatomy of a Test Case](#anatomy-of-a-test-case)

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

Another new test case added, `test_missing_entry_raises_KeyError` demonstrates the use of [unittest.assertRaises](https://docs.python.org/3/library/unittest.html#unittest.TestCase.assertRaises).

You can run a test case with the CLI in verbose mode by changing into the test directory and running `python3.7 -m unittest -v`. The `-v` flag tells Python's unit test runner to run in verbose mode.

This mode will list out all of the test cases being run:
```console
test_create_phonebook (test_phonebook_01.PhonebookTest_01) ... ok
test_lookup_entry_by_name (test_phonebook_01.PhonebookTest_01) ... ok
test_missing_entry_raises_KeyError (test_phonebook_01.PhonebookTest_01) ... ok

----------------------------------------------------------------------
Ran 3 tests in 0.001s

OK
```

To run a single test case, you can pass in the `-q` flag to the CLI with the fully qualified name of the test case:
```console
> python3.7 -m unittest -q test_phonebook_01.PhonebookTest_01.test_lookup_entry_by_name
```

## Test Suite
A *test suite* is a number of test cases that are being executed together by a test runner. A test suite can be chosen from a number of test classes, which themselves contain test cases.

## Skipping A Test Case
To skip a test case, decorate the test case with `@unittest.skip()`:
```python
@unittest.skip("Output message")
def test_test_case_to_skip(self):
    pass
```

An example of a skipped test case can be found in [exercise 02](./exercises/02%20-%20skip%20test%20case) under the test file [test_phonebook_02.py](./exercises/02%20-%20skip%20test%20case/test_phonebook_02.py). 

Running this test in verbose mode via CLI, `python3.7 -m unittest -v`, will display the skipped test in the output:
```console
test_create_phonebook (test_phonebook_02.PhonebookTest_02) ... ok
test_empty_phonebook_is_consistent (test_phonebook_02.PhonebookTest_02) ... skipped 'WIP'
test_lookup_entry_by_name (test_phonebook_02.PhonebookTest_02) ... ok
test_missing_entry_raises_KeyError (test_phonebook_02.PhonebookTest_02) ... ok

----------------------------------------------------------------------
Ran 4 tests in 0.001s

OK (skipped=1)
```

## setUp and tearDown
[setUp](https://docs.python.org/3/library/unittest.html#unittest.TestCase.setUp) and [tearDown](https://docs.python.org/3/library/unittest.html#unittest.TestCase.tearDown) are methods called immediately before and after a test method is called, respectively.

These methods are used to set up and clean up *test fixtures*. A *test fixture* is a piece of code that can construct and configure the system of the test to get it ready to be tested, and to clean up afterwards. It allows for the separation of concerns so that the test cases can concentrate on specifying and checking a particular behavior, and not be cluttered with general setup details.

## Anatomy of a Test Case
A *test case* generally has a *name*, and then four parts: *arrange*, *act*, *assert*, and *cleanup*.

The *name* of your test case is very important: it acts as a headline summarizing the behavior your test case is looking for. It is the first thing you see when a test case fails - the test case name should give you a good indication of which functionality is not working.

The parts of a test case are as follows:
**Arrange**: Set up the object to be tested, as well as collaborators. This section of the test can be split up between the test fixture and the test case itself.

**Act**: Excercise functionality of the object. This should usually only be one line of code, because a unit test is only checking one behavior.

**Assert**: Make claims about the object and its collaborators. Often there is only one assertion needed, because you are only checking one behavior.

**Cleanup**: Release resources, restore to the original state. Usually happens in the `tearDown` part of the test fixture.
