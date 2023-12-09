# Chapter 8: The Module Pattern
This chapter will explore one of the most important code organization patterns in all of programming: the module.

## Sections
* [Encapsulation and Least Exposure (PoLE)](#encapsulation-and-least-exposure-pole)
* [What Is a Module?](#what-is-a-module)
  * [Namespaces (Stateless Grouping)](#namespaces-stateless-grouping)
  * [Data Structures (Stateful Grouping)](#data-structures-stateful-grouping)
  * [Modules (Stateful Access Control)](#modules-stateful-access-control)
    * [Module Factory (Multiple Instances)](#module-factory-multiple-instances)

[◂ Return to Table of Contents](../README.md)

## Encapsulation and Least Exposure (PoLE)
The goal of encapsulation is the bundling/co-location of data and behavior that together serve a common purpose. The spirit of encapsulation can be realized as simply as using separate files to organize portions of a single program with a common purpose.

The recent trend in front-end programming to organize programs around Component architecture pushes encapsulation further: it feels natural to bundle all code, styles, and markup used for a single UI element into its own unit of programming logic to interact with. This collection of logic, markup, and styles is labeled a _component_.

Another key goal of encapsulation is control over access to aspects of the encapsulated data and functionality. [PoLE](../06/README.md#least-exposure) seeks to defend against the various dangers of scope over-exposure. In JavaScript, this access control is implemented through the mechanics of lexical scope. The idea is to only allow access to the data/functionalities considered _public_ and programatically limit access to those deemed _private_.

The intended result of this effort is better code organization. It is easier to build and maintain software when data/functionality exists where expected, with clear and obvious boundaries and access points. It is easier to maintain quality when the pitfalls of over-exposed data and functionality are avoided.

[▲ Return to Sections](#sections)

## What Is a Module?
A module is a collection of related data and functions, characterized by a division between hidden _private_ details and _public_ accessible details (called a _public API_).

A module is stateful, maintaining some information over time, along with functionality to access and update that information.

**NOTE**: A broader concern of the module pattern is fully embracing systel-level modularization through loose-coupling and other program architecture techniques. That topic is beyond this discussion, but worth further study.

Let's compare some module charateristics to useful code patterns that are not quite modules.

#### Namespaces (Stateless Grouping)
A grouping of related, _stateless_ functions (without data) would be better described as a _namespace_, rather than a module:

```javascript
// namespace, not module
var Utils = {
  cancelEvt(evt) {
    evt.preventDefault();
    evt.stopPropagation();
    evt.stopImmediatePropagation();
  },
  wait(ms) {
    return new Promise(function c(res){
      setTimeout(res,ms);
    });
  },
  isValidEmail(email) {
    return /[^@]+@[^@.]+\.[^@.]+/.test(email);
  }
};
```

`Utils` is a useful collection of utilities, but they are all state-independent functions. Gathering functionality together is a good practice, but that does not make it a module. Instead, a `Utils` namespace has been defined, and functions have been organized under it.

#### Data Structures (Stateful Grouping)
When data and stateful functions are bound together without limiting access to any of it, it stops short of the PoLE aspect of encapsulation, and is therefore it is not helpful to label it a module:

```javascript
// data structure, not module
var Student = {
  records: [
    { id: 14, name: "Kyle", grade: 86 },
    { id: 73, name: "Suzy", grade: 87 },
    { id: 112, name: "Frank", grade: 75 },
    { id: 6, name: "Sarah", grade: 91 }
  ],
  getName(studentID) {
    var student = this.records.find(
      student => student.id == studentID
    );
    return student.name;
  }
};

Student.getName(73);
// Suzy
```

`Student` isn't really a module since `records` is publicly accessible data. Without limiting access to any of the data/functionality contained within `Student`, it is just a data structure.

#### Modules (Stateful Access Control)
To embody the full spirit of the module pattern: grouping, state, and access control must be present.

Converting the example from the [previous section](#data-structures-stateful-grouping) from a simple data structure into a _classic module_ or _revealing module_ (a pattern that first emerged in the early 2000s):

```javascript
var Student = (function defineStudent(){
  var records = [
    { id: 14, name: "Kyle", grade: 86 },
    { id: 73, name: "Suzy", grade: 87 },
    { id: 112, name: "Frank", grade: 75 },
    { id: 6, name: "Sarah", grade: 91 }
  ];

  var publicAPI = {
    getName
  };

  return publicAPI;

  // ************************

  function getName(studentID) {
    var student = records.find(
      student => student.id == studentID
    );
    return student.name;
  }
})();

Student.getName(73);   // Suzy
```

`Student` is now an instance of module with a public API that contains a single method: `getName(..)`. `Student.getName(..)` is able to access the private `records` data.

`Student` is assigned the return value of the IIFE `defineStudent()`, which is the `publicAPI` object. This object contains a property referencing the inner `getName(..)` function. `getName(..)` maintains access to the inner `records` variable via closure.

By virtue of how lexical scope works, variables and functions defined within the outer module definition function are private by default. Only references returned by this outer function are accessible from outside of it.

The use of an IIFE implies the program only ever needs a single instance of the module (referred to as a _singleton_).

##### Module Factory (Multiple Instances)
To define a module that supports multiple instances, the code from the above example can be modified thusly:

```javascript
// factory function, not singleton IIFE
function defineStudent() {
  var records = [
    { id: 14, name: "Kyle", grade: 86 },
    { id: 73, name: "Suzy", grade: 87 },
    { id: 112, name: "Frank", grade: 75 },
    { id: 6, name: "Sarah", grade: 91 }
  ];

  var publicAPI = {
    getName
  };

  return publicAPI;

  // ************************

  function getName(studentID) {
    var student = records.find(
      student => student.id == studentID
    );
    return student.name;
  }
}

var fullTime = defineStudent();
fullTime.getName(73);            // Suzy
```

Rather than using `defineStudent()` in an IIFE, define it as a normal standalone function. In this context, the function is commonly referred to as a _module factory_. Calling the module factory returns an instance of the module.

[▲ Return to Sections](#sections)

| [Previous: Chapter 7 - Using Closures](../07/README.md) | [Table of Contents](../README.md#table-of-contents) |
