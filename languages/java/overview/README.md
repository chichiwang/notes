# Java Overview
**Author**: [Chi-chi Wang](https://github.com/chichiwang)

A high level investigation into the Java Programming Language. This is meant to be a profile of the language itself more than an investigation of the syntax and operations.

## Table of Contents
* [Profile](#profile)
* [History](#history)
* [Principles](#principles)
* [Execution System](#execution-system)
* [Syntax](#syntax)
* [Class Libraries](#class-libraries)
* [Resources](#resources)

## Profile
* General purpose programming language
  * As opposed to a Domain Specific Language (DSL)
* Class-based
* Object-oriented
* Designed to have as few implementation dependencies as possible
* Intended to be *write once, run anywhere*
* Compiled to bytecode and run on any Java Virtual Machine (JVM)
* Syntax is similar to *C* and *C++*
  * Has fewer low-level facilities than either of them
* Automatic memory management
* Does not support operator overloading
* Does not support multiple-inheritance for classes
* Supports multiple-inheritance for interfaces
* All code is written inside of classes
* Every data item is an object
  * The only exceptions are primitive data types (integers, floating point numbers, boolean values, characters)
* JavaDoc is an executable that converts properly formatted comments into documentation
  * Supported by some IDEs
* String literals are automatically converted to string objects by the compiler
* Generics were introduced to the Java language as part of J2SE 5.0 in 2004

## History
* Created by James Gosling at Sun Microsystems
* Language development started in 1991
* Publicly released version 1.0 in 1996
* Major web browsers incorporated the ability to run Java Applets within web pages
* Remains a de facto standard controlled through the [Java Community Process](https://www.jcp.org/en/home/index)
  * A formalized mechanism that allows interested parties to develop standard technical specifications for Java technology
* Sun Microsystems released much of the JVM as free and open-source software on November 13, 2006 under the GPL

## Principles
There were five primary goals in the creation of the Java language:
* It must be simple, object-oriented, and familiar
* It must be robust and secure
* It must be architecture-neutral and portable
* It must execute with high performance
* It must be interpreted, threaded, and dynamic

## Execution System
To achieve portability Java Language code is compiled to Java bytecode, an intermediate representation, rather than compiling directly to architecture-specific machine code.

End users commonly use a *Java Runtime Environment*(JRE) installed on their machine for standalone Java applications.

Standard libraries provide a generic way to access host-specific features (graphics, threading, networking, etc).

Java programs have a reputation for being slower and utilizing more memory than C++ programs. Just-in-time compliation was introduced in 1997/1998 and significantly improved performance.

## Syntax
Java syntax is heavily influenced by C++. Where C++ combines the syntax for structured, generic, and object-oriented programming, Java was built almost exclusively as an object-oriented language.

* Source files must be named after the public class they contain
* Source files use the file extension `.java`
* Compiled bytecode uses the extension `.class`
* A source file may only contain one public class
  * May contain private classes
  * May contain public inner classes
* A class not declared public may be stored in any `.java` file
* The keyword `public` can be used to denote public classes and public methods
  * May be called from code in other classes
  * May be used by classes outside the class hierarchy
* The keyword `static` in front of a method indicates a static method
  * Associated with a class itself, and not to instances of the class
  * Only static methods can be invoked without a reference to an object
  * Cannot access any class members that are not also static
* Methods not designated `static` are instance methods and may only be accessed via instances of a class
* The keyword `void` indicates the main method does not return any value to the caller
* If a Java program needs to exit with an error code, it must call `System.exit()` explicitly
* The method `main()` is not a keyword in Java
  * `main()` is the method that the Java launcher calls to pass control to the main program
  * `main()` method must accept an array of string objects

## Class Libraries
The *Java Class Library* is the standard library for Java, controlled by Oracle in cooperation with others through the JCP. Documentation on all of the available classes and methods can be found on the [Oracle documentation page](https://docs.oracle.com/javase/8/docs/api/allclasses-frame.html).

## Resources
* [Wikipedia](https://en.wikipedia.org/wiki/Java_(programming_language))
