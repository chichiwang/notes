# Modern Java: The Big Picture
**Author**: [Chi-chi Wang](https://github.com/chichiwang)

Date: June 2020

Course: [PluralSight](https://app.pluralsight.com/library/courses/modern-java-big-picture/table-of-contents)

## Table of Contents
* [The Java Platform](#the-java-platform)
* [The Philosophy of Java](#the-philosophy-of-java)

## The Java Platform
The Java Platform is comprised of:
* A programming language
  * Used by an application's source code
  * Compiles to application byte code
    * Using the Java compiler
    * Third party libraries are also published as byte code
      * Can be used by applications
* Runtime environment
  * The Java Virtual Machine (JVM) is the runtime evnrionment for byte code
    * Translates byte code into machine code to execute machine instructions
* Standard Library
  * Composed of Java Standard Edition (SE) APIs

The runtime environment (JDK) and standard library (SE) are bundled together in the Java Development Kit (JDK).

## The Philosophy of Java
Java was built on a number of guiding principles:
* Portability
  * Java applications should be able to run on any platform
    * Any OS, any hardware
  * Acronym used by Java to encapsulate this principle is WORA (write once, run anywhere)
  * Acheived by having a JVM for each OS/architecture
* Optimized for Readability
  * Based on the observation that "**Reading** code is more important than **writing** code"
  * Focus on maintainability over terseness
  * Prioritizes understandable code over clever code
  * Leads to a conservative approach to new features
    * Guided by the principle "First do no harm"
    * Focus is on the long term future and developer productivity
* Backward compatibility
  * It is not uncommon to have older code running on newer JVMs
  * There is a large focus on long term support (LTS)
  * There is a controlled way of deprecating features
* Open
  * Specification process
  * Specs are driven by the Java Community Process (JCP)
    * They get a say in specifying the platform
    * There is a lot of vendor and community participation around the design and direction of Java
  * A formal specification that allows non-Oracle implementations of Java
    * IBM, Eclipse are examples
  * The Oracle refrence implementation of Java is hosted as an open source project
    * OpenJDK Project
    * GPL2 Licensed
  * Large, global Java community
