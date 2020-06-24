# Modern Java: The Big Picture
**Author**: [Chi-chi Wang](https://github.com/chichiwang)

Date: June 2020

Course: [PluralSight](https://app.pluralsight.com/library/courses/modern-java-big-picture/table-of-contents)

## Table of Contents
* [The Java Platform](#the-java-platform)
* [The Philosophy of Java](#the-philosophy-of-java)
* [Target Environments](#target-environments)
  * [Desktop](#desktop)

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
