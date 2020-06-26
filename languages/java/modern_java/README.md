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
