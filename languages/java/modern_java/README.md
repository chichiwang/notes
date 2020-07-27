# Modern Java: The Big Picture
**Author**: [Chi-chi Wang](https://github.com/chichiwang)

Date: June 2020

Course: [PluralSight](https://app.pluralsight.com/library/courses/modern-java-big-picture/table-of-contents)

## Table of Contents
* [The Java Platform](#the-java-platform)
* [The Philosophy of Java](#the-philosophy-of-java)
* [Target Environments](#target-environments)
  * [Desktop](#desktop)
  * [Enterprise](#enterprise)
  * [Microframeworks](#microframeworks)
  * [Android](#android)
* [Spring Framework](#spring-framework)
* [Common Libraries](#common-libraries)
  * [Utility Libraries](#utility-libraries)
  * [Distributed System Libraries](#distributed-system-libraries)
  * [Data Access Libraries](#data-access-libraries)
  * [Data Processing Libraries And Applications](#data-processing-libraries-and-applications)
* [Java Development Lifecycle](#java-development-lifecycle)
* [Development Tools](#development-tools)
  * [IDEs](#ides)
  * [Unit Testing](#unit-testing)
  * [Build Tools](#build-tools)
  * [Static Analysis](#static-analysis)
* [Alternative JVM Languages](#alternative-jvm-languages)
  * [Groovy](#groovy)
  * [Scala](#scala)
  * [Kotlin](#kotlin)

## The Java Platform
The Java Platform is comprised of:
* A programming language
  * Used by an application's source code
  * Compiles to application bytecode
    * Using the Java compiler
    * Third party libraries are also published as bytecode
      * Can be used by applications
* Runtime environment
  * The Java Virtual Machine (JVM) is the runtime evnrionment for bytecode
    * Translates bytecode into machine code to execute machine instructions
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

## Target Environments
### Desktop
Java can be used to build applications that:
* Run on a single machine
* Are interactive
* Have a graphical user interface

Java ships with a GUI toolkit called AWT (Abstract Windowing Toolkit). AWT provides access to native OS controls. AWT provides simple graphics primitives. AWT is difficult to develop UI on, however, since it uses native graphical elements so layouts and styles can be difficult to develop to look correct across all devices.

A newer GUI toolkit provided by Java is called Swing. Swing provides a pure Java GUI. Swing does not use native OS objects like AWT does, instead allowing the developer to choose from many cross-platform look and feels. Regardless of the choice, it will look the same across all platforms that the application runs on. Swing also introduces layout managers which allow developers to define placement of components in relative terms, rather than absolute terms, for flexible and scalable UI. UI development in Swing conform to the MVC pattern.

It is possible to mix AWT and Swing components in a single application if so desired.

A third GUI toolkit available to Java is called JavaFX. JavaFX provides a declarative, XML-based format for UIs called FXML. This allows for the design and creation of the GUI to be decoupled from the JavaFX application APIs and the underlying implementation. Because of this, external drag-and-drop applications can be used to create Java GUIs and the resulting FXML can be loaded by JavaFX to achieve a fully functional user interace. JavaFX offers more advanced components than AWT or Swing. All the UI components are rendered by Java itself (as opposed to using native OS components). JavaFX components support animations and effects (not supported by AWT or Swing). These components are skinnable with CSS. JavaFX also allows for the creation of 3D graphics.

Up until Java 10, JavaFX shipped with the standard JDK. As of Java 11, JavaFX is shipped separately from the JDK and run in its own project called OpenJFX.

### Enterprise
Enterprise Java is best known by Java EE (Enterprise Edition) as opposed to SE (Standard Edition). Java EE was designed to make enterprise development more productive. To achieve this it provides APIs for common tasks/utilities used in enterprise development such as: data persistence, web applications, security, messaging, JSON/XML handling.

Java EE application servers are containers that run on top of the JVM that can host Java EE applications. Application servers implement specifications such as:
* Java Persistence Architecture
  * Deals with storing/retrieving data from relational DBs and translating them into objects
* Enterprise Java Beans
  * Used to create tranactional business logic
* Java Server Faces
  * Used to create web front-ends

With Java SE, your applications run directly on top of the JVM. In Java EE the application server is what runs on top of the JVM. Applications are then deployed into the application server - these applications are packaged as a WAR (Web Archive), or an EAR (enterprise archive). Multiple applications can be deployed to an application server.

Benefits of this setup:
* Application server offers fully functional sets of compatible Java EE libraries
* Application servers can also be clusters
  * Takes care of scaling out applications
* Offers a productive, scalable development model for enterprise applications

There are multiple application servers available that implement the Java EE specifications. Among them are:
* Wildfly (Red Hat)
* WebSphere (IBM)
* WebLogic (Oracle)
* Tomcat (Apache)

The Java EE 8 release will be the last release of the Java EE specification by Oracle. The specification is being donated by Oracle to the Eclipse Foundation where it will be open-sourced. Java EE will be rebranded to Jakarta EE. It is expected that Jakarta EE will continue the legacy of the scalable, maintainable platform that Java EE has become.

A competitor to Java EE called the Spring Framework has gained huge popularity as well.

### Microframeworks
Java microframeworks were developed to help build Java applications as microservices hosted on cloud architecture. These applications are typically smaller than their enterprise counterparts.

Microframeworks include:
* Spring Boot
  * Comprised of the Spring Boot application and other Spring libraries that provide solutions to challenges that are encountered in the cloud
  * Netflix offers many libraries to deal with the challenges of cloud architecture
  * Takes a convention over configuration approach
* MicroProfile
  * Selects a subset of Java EE specifications that are relevant for building cloud applications and adds some missing components
  * A developer can still leverage previous Java EE knowledge to build microservices
* Vert.x
  * Open source project by Red Hat
* Play Framework
  * Open source microframework sponsored by Lightbend

### Android
Java has always been the primary development language of applications running on Android. However, Java is not the same as Android Java.

Android Java leverages the Java programming language. Google is lacking support for other versions of Java, so applications on Android are written in Java 7 or a subset of features of Java 8.

Android Java applications are still compiled to Java bytecode. The bytecode, however, does not run on the device. Another tool tranlates the Java bytecode into DEX (Dalvik Executable Format). DEX is the code that runs on the device.

DEX leverages Android APIs and JAVA SE APIs. The Android version of Java SE APIs are not compatible with the Java SE APIs of the official Java platform.

Because of this Android applications do not run on the JVM but run on a Dalvik Virtual Machine (DVM).

## Spring Framework
The Spring Framework was developed as an alternative to J2EE (Enterprise Java) with Enterprise Java Beans (EJB) in 2002. J2EE was a large, cumbersome stack and EJB was difficult to work with form a developer perspective.

One of the main innovations of Spring is dependency injection. At its core, Spring is a dependency injection container that wires the classes of an application based on instructions provided. These instructions can be provided as annotations to the code, as explicit configuration code, or as XML configuration.

In a standard Java application, a class will instantiate other classes it depends on at the point that it itself is instantiated. This creates tight coupling. This also makes it difficult to test the class in isolation since running tests will always load the dependency classes, preventing the ability to replace them with stubs or mocks for testing.

With Spring, all classes are still instantiated. The difference is that instead a developer would instruct the Spring Framework of the dependencies of a class, and Spring would instantiate them instead. The instances of classes created by Spring are called Spring Beans. The Spring Dependency Injection Container then injects dependency instances into the dependent class. This approach decouples dependents from their dependencies. This leads to more flexible composition because the dependency chains are dictated by configuration code. It is also possible for Spring to inject some interceptor code between the classes that handle cross-cutting concerns (security, transactionality, etc).

This strategy ensures that developers declares dependencies, and Spring brings them together at runtime in the Spring Dependency Injection Container.

In addition to this, Spring also integrates other technologies and makes them available through dependency injection with a bit of configuration (utilities). These utilities include things such as data access technologies, Java EE technologies, Spring WebFlux (web stack), branch components, integration framework.

## Common Libraries
### Utility Libraries
* Google Guava
  * A collection of useful utilities that extend Java functionality in useful ways
  * Includes additional collection types
  * Includes caching implementations
  * Includes IO helper classes
* Apache Commons
  * A collection of libraries hosted at the Apache Foundation that extend core Java functionality
  * Contains more utilities than provided by Google Guava
  * Includes collections helpers
  * Includes CSV helpers
  * Includes IO helpers
* Apache Log4J
  * A library for structured application logging
    * Allows you to introduce logging statements and send those statements to many different packages depending on the configuration

### Distributed System Libraries
* Netty
  * Low level library
  * Makes it easy to build clients and servers for various protocols
    * HTTP, HTTP2, Web Sockets, etc
  * Provides productive abstractions over the underlying TCP and UDP mechanisms
  * One of the best performing networking libraries in the Java space
* Akka
  * Actor model for concurrency
  * Can be used within a single JVM or distributed throughout a cluster for distribution
* RxJava
  * Implementation of the reactive programming paradigm
  * Asyc, event-model based
* Apache Camel
  * Library to wire up existing systems into new workflows
    * Enterprise application integration
  * Provides support for many transports and connectors to enterprise systems

### Data Access Libraries
**Java Database Connectivity (JDBC)**
Part of the Java Standard Library
* JDBC drivers are available for all the popular relational databases
* Provides an abstraction layer for data access so your code can move between different DBMSs
* Is still too low level to be productive with

**Object Relational Mappers (ORMs)**
Rather than writing queries, everything is done through plain Java objects.
* Hibernate
* EclipseLink

**SQL Domain-Specific Languages (DSLs)**
Embed SQL's query language as a domain-specific language into Java.
* jOOQ
* QueryDSL

There are two distinctive approaches to Database access in Java:
1. Enterprise Java embraces the ORM approach by standardizing the Hibernate and EclipseLink APIs into Java Persistence Architecture (JPA).
2. There is no standard for the SQL DSL approach.

### Data Processing Libraries And Applications
* Apache Hadoop
  * Open source implementation of Google's MapReduce concept
  * Well suited to processing petabytes of data on large clusters running many instances of Hadoop
  * Not really a library, but a distributed system implemented in Java for processing big data
  * Implement a Hadoop job using the Hadoop library and submit it to a running Hadoop cluster
* Apache Spark
  * Goals are similiar to Hadoop
    * Newer and even more scalable implementation
  * Possible to process large amounts of streaming data
* DL4J
  * Deep Learning for Java
  * Implement deep neural networks in Java
    * Face detection, object detection, etc.
* Cassandra
  * NOSQL Database written in Java
* Neo4J
  * Graph DB written in Java
* ElasticSearch
  * Used to create efficient search indexes for big data sets
* HDFS
  * Hadoop Distributed File System
  * Sits beneath the Hadoop data processing layer

## Java Development Lifecycle
1. Code
2. Build
3. Test
4. Static Analysis

## Development Tools
### IDEs
Integrated Development Environments (IDEs) offer advanced features that help a developer code more productively.

Features that help productivity include:
* Code completion
* Inline documentation
* Smart navigation between different elements in the code
* Debug functionalities
  * Set breakpoints
  * Run the code
  * Inspect values, variables, contexts
* Format code
* Integrate with source control

Most Java developers use IDEs to aid their development process. Not using an IDE for developing Java applications is generally a bad idea, unless creating small and simple applications.

The most popular IDEs are:
* Eclipse
  * Open source
  * Hosted at the Eclipse Foundation
  * One of the most widely used Java IDEs
* IntelliJ
  * Commercial IDE created by JetBrains
  * Community Edition version is free to use

### Unit Testing
Java was one of the first languages where the practice of unit testing became widespread and supported by tools.

JUnit was the first unit testing framework and to this day is still the leading unit testing framework in Java.

Mockito is one of the most popular mocking libraries in Java.

### Build Tools
A major goal of Java build tools is to ensure repeatable builds. This means that different users in different environments, given the same code base, can produce the same build of an application.

Build tools also help to manage your application code base. As applications grow, they may be split up into sub-modules or sub-components. Build tools can assist in this.

Build tools will also help with the management of external dependencies in service of the goal of ensuring repeatable builds. They may also be used to automate the running of tests.

Maven is the most widely used build tool in Java. With Maven you describe your builds in an XML file called `pom.xml` in a declarative manner. Maven prioritizes convention over configuration: it will assume a standard directory layout for Java codebases.

Maven leverages a repository called [Maven Central](https://search.maven.org/) to fetch dependencies. Maven Central is the standard repository for all Java libraries to be published to.

The second most important Java build tool is [Gradle](https://gradle.org/). In Gradle, rather than specifying builds in XML, builds are instead specified using the Groovy scripting language. One improvement Gradle has over Maven is that Gradle supports incremental builds: it will only build a piece of code if the previously compiled version is outdated. Gradle also leverages dependencies from Maven Central.

Gradle is the default build system for Android applications.

### Static Analysis
A few important static analysis tools for Java:
* Checkstyle
  * Enforces stylistic choices for a Java codebase
* Spotbugs
  * Scans byte code with a set of predefined rules to identify bugs in the code
  * May help identify anti-patterns that will not lead to compilation errors but may lead to problems at runtime
* PMD
  * Similar to Spotbugs, but scans at the source code level
  * Finds different classes of bugs than Spotbugs
* SonarQube
  * Continuous inspection tool
  * Works in tandem with the continuous integration server
  * Tracks all the findings from all of the static analysis tools for each build
  * Tracks code coverage for unit tests
  * Detection for duplicate code
  * Provides a dashboard to show trends over time

## Alternative JVM Languages
Languages have been developed to compile to Java byte code, which can then be run on the JVM.

There are reasons why a developer would prefer a different language to compile the byte code:
* Productivity
  * Java is optimized for readability, some teams may prefer terse languages
* Familiarity
  * There are other language implementations, such as Ruby and Python (JRuby, JPython), which can compile to Java byte code
* Paradigms
  * Other languages are better designed to tackle paradigms other than OOP

Many ideas of alternative JVM languages have changes in the Java programming language over the years.

### Groovy
A popular, widely used alternative JVM language.

* Dynamic scripting language
  * Interpreted at runtime
  * Optionally can be compiled to byte code using a Groovy compiler
    * Improved performance - interpreting Groovy code can be quite slow
* Opt-in type system
  * Language is much more dynamic
  * Makes Groovy less statically safe
  * Developers can optionally add type annotations to Groovy code
* Syntactically more concise than Java
  * Still stylistically similar to Java
* Can be added to Java codebases
  * Groovy works very well with existing Java classes
  * Groovy was designed to integated seamlessly with Java code

### Scala
Scala combines object-oriented programming with functional programming. The focus of the language is more on functions and less on objects.

The concurrency library Akka, and the big data processing library Spark are both written in Scala.

* Compiled language
  * Scala compiler generates Java byte code
* Extensive type system
  * Scala is statically typed
  * Type system supports the advanced object-oriented features as well as the functional features of Scala
* Powerful language, but complex
* Compiler can get quite slow on larger codebases due to the extensive feature set of the language
* Runs on the JVM but requires adding of Scala runtime libraries

### Kotlin
Developed after Scala by JetBrains (the company behind the IntelliJ IDE).

* Designed as a 'better Java'
* Does not have the burden of backwards compatibility that Java has
* Fully interoperable with Java code
  * To a greater degree than Scala
* Offers null safety
* Offers data classes
* Endorsed by Google as a language for Android development in 2017
* Compiles to Java byte code as well as to JavaScript
  * Can share code between the browser and the backend
