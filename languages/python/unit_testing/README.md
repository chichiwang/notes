# Unit Testing With Python - WIP
**Author**: [Chi-chi Wang](https://github.com/chichiwang)

Course: [PluralSight](https://app.pluralsight.com/library/courses/unit-testing-python/table-of-contents)

## Table of Contents
* [Vocabulary](#vocabulary)

## Vocabulary
A **unit test** checks the behavior of a *system*, *elements of code*, and *behavior of code*. An **element of code** refers to a *method*, a *function*, a *module*, or a *class*.

An **automated test** is designed by a person and runs without intervention, unambiguously reporting the results as *pass* or *fail*. An *automated test* is designed to be run by any developer who has access to your code.

By the strict definition of a *unit test*, a test is **not** a unit test if it uses any external resource, such as the filesystem, a database, or a network.

Other tests that utilize external resources may be referred to as *integration tests*, *functional tests*, or *acceptance tests*.

A **test case** tests a specific behavior of a system. Each *test case* should be able to be run independently of others as many times as necessary, and clearly report a *pass* or *fail* result. It should not contain side effects that other test cases will rely on. The name of a test case should be explanatory to readers of the test, providing clarity to what is being tested.
