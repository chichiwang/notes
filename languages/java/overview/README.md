# Java Overview - WIP
**Author**: [Chi-chi Wang](https://github.com/chichiwang)

A high level investigation into the Java Programming Language. This is meant to be a profile of the language itself more than an investigation of the syntax and operations.

## Table of Contents
* [Profile](#profile)
* [History](#history)
* [Principles](#principles)
* [Execution System](#execution-system)
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

## Resources
* [Wikipedia](https://en.wikipedia.org/wiki/Java_(programming_language))
