# Chapter 8: The Module Pattern
This chapter will explore one of the most important code organization patterns in all of programming: the module.

## Sections
* [Encapsulation and Least Exposure (PoLE)](#encapsulation-and-least-exposure-pole)
* [What Is a Module?](#what-is-a-module)
  * [Namespaces (Stateless Grouping)](#namespaces-stateless-grouping)
  * [Data Structures (Stateful Grouping)](#data-structures-stateful-grouping)
  * [Modules (Stateful Access Control)](#modules-stateful-access-control)
    * [Module Factory (Multiple Instances)](#module-factory-multiple-instances)
    * [Classic Module Definition](#classic-module-definition)
* [Node CommonJS Modules](#node-commonjs-modules)
* [Modern ES Modules (ESM)](#modern-es-modules-esm)

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

##### Classic Module Definition
What makes something a module?
* There must be an outer scope, typically from a module factory function running at least once.
* The module's inner scope must contain at least one piece of hidden information that represents state for the module.
* The module must return at least one function that has closure over the hidden module state.

[▲ Return to Sections](#sections)

## Node CommonJS Modules
CommonJS modules are file-based, with a single module per file:

```javascript
module.exports.getName = getName;

// ************************

var records = [
  { id: 14, name: "Kyle", grade: 86 },
  { id: 73, name: "Suzy", grade: 87 },
  { id: 112, name: "Frank", grade: 75 },
  { id: 6, name: "Sarah", grade: 91 }
];

function getName(studentID) {
  var student = records.find(
    student => student.id == studentID
  );
  return student.name;
}
```

The `records` and `getName` identifiers exist in the top-level scope of this module, but this is not the global scope. Everything in this file is private to the module by default.

In order to expose something to the module's public API, a property must be added to the provided empty object `module.exports`. In older, legacy code, there may be references to a bare `exports`, but for clarity this should always be prefixed with `module.`.

For style purposes place all exports at the top or bottom of the file.

Some developers may replace the default export object like so:

```javascript
// defining a new object for the API
module.exports = {
  // ..exports..
};
```

This is not recommended as there are quirks with this approach, including unexpected behavior if multiple such modules circularly depend on each other. Instead, to export multiple objects at once using the object literal notation, do this instead:

```javascript
Object.assign(module.exports,{
  // .. exports ..
});
```

The `Object.assign(..)` will perform a shallow copy of all of the properties of the object literal `{ .. }` onto the existing `module.exports` object. This is safer module behavior.

To import another module's API into the current module, use Node's `require(..)` method, providing the path to the module file being imported from:

```javascript
var Student = require("/path/to/student.js");

Student.getName(73);
// Suzy
```

The `Student` variable is now assigned the public API of the `/path/to/student.js` module file.

CommonJS modules behave as singleton instances. No matter how many times the public API of a module file is imported, all imports reference the same single shared module instance.

`require(..)` will return the entire public API of a target module. To access only part of the API, the typical approach is:

```javascript
var getName = require("/path/to/student.js").getName;

// or alternately:

var { getName } = require("/path/to/student.js");
```

Similar to the [classic module format](#modules-stateful-access-control), methods and variables on a module's public API hold closures over the internal module details.

**NOTE**: In Node, non-absolute paths provided to `require(..)` (such as `require("student")`) assume a `.js` file extension and search the _/node_modules_ directory to resolve the import.

[▲ Return to Sections](#sections)

## Modern ES Modules (ESM)
Like CommonJS, the ESM format is file-based, module instances are singletons, and everything is private by default. Unlike CommonJS, ESM files are always run in strict-mode without needing the `"use strict"` pragma.

ESM uses an `export` keyword (instead of the CommonJS `module.exports`) and an `import` keyword (instead of the CommonJS `require(..)`):

```javascript
export { getName };

// ************************

var records = [
  { id: 14, name: "Kyle", grade: 86 },
  { id: 73, name: "Suzy", grade: 87 },
  { id: 112, name: "Frank", grade: 75 },
  { id: 6, name: "Sarah", grade: 91 }
];

function getName(studentID) {
  var student = records.find(
    student => student.id == studentID
  );
  return student.name;
}
```

`export` must be used at the top-level scope, it cannot be inside of any function or block.

`export` allows some variations in its usage:

```javascript
export function getName(studentID) {
  // ..
}
```

The above is a standard function declaration that happens to be exported.

A _default export_ is available with different semantics from other exports - it provides a shorthand for consumers of the module when being imported, allowing a terser syntax when they only need a default API member:

```javascript
export default function getName(studentID) {
  // ..
}
```

Non-default exports are referred to as _named exports_.

The `import` keyword must also be used at the top-level scope of a module, and allows some variations in its usage. Below is an example of a _named import_:

```javascript
import { getName } from "/path/to/students.js";

getName(73);   // Suzy
```

This form imports only the specifically-named public API members from a module (skipping over anything not named explicitly). It adds these identifiers to the top-level scope of the current module.

A named import can also be renamed within the consuming module using the `as` keyword:

```javascript
import { getName as getStudentName }
  from "/path/to/students.js";

getStudentName(73);
// Suzy
```

To import a default export:

```javascript
import getName from "/path/to/students.js";

getName(73);   // Suzy
```

To import both the default export as well as named exports from a module:

```javascript
import { default as getName, /* .. others .. */ }
  from "/path/to/students.js";

getName(73);   // Suzy
```

Another variation on `import` is the _namespace import_:

```javascript
import * as Student from "/path/to/students.js";

Student.getName(73);   // Suzy
```

The `*` imports everything exported from the target module, both default and named, and stores it under the specified namespace identifier.

**NOTE**: At the time of this publication, browsers have supported ESM for some time. Node's support of ESM is relatively stable, but has been evolving for some time. The introduction of ESM to JS in ES6 created challenging compatibility concerns for Node's interop with CommonJS modules. Consult [Node's documentation](https://nodejs.org/api/esm.html) for the latest details.

[▲ Return to Sections](#sections)

| [Previous: Chapter 7 - Using Closures](../07/README.md) | [Table of Contents](../README.md#table-of-contents) | [Next: Appendix A - Exploring Furhter](../appendixA/README.md) |
