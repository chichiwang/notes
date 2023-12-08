# Chapter 8: The Module Pattern
This chapter will explore one of the most important code organization patterns in all of programming: the module.

## Sections
* [Encapsulation and Least Exposure (PoLE)](#encapsulation-and-least-exposure-pole)
* [What Is a Module?](#what-is-a-module)
  * [Namespaces (Stateless Grouping)](#namespaces-stateless-grouping)

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

[▲ Return to Sections](#sections)

| [Previous: Chapter 7 - Using Closures](../07/README.md) | [Table of Contents](../README.md#table-of-contents) |
