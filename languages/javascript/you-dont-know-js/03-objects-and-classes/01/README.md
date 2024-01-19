# Chapter 1: Object Foundations
> Everything in JavaScript is an object.

This is one of the most pervasive, but most incorrect, myths about JavaScript. JavaScript definitely has objects, but not all values are objects. Objects are arguably the most important and varied value type in the language.

Objects are the most flexible and powerful container in JavaScript. They are the foundation for the second of JavaScript's three pillars: the prototype. Prototypes, along with the `this` keyword, are how the JavaScript object system expresses the class design pattern.

## Sections
* [About This Book](#about-this-book)
* [Objects As Containers](#objects-as-containers)

[◂ Return to Table of Contents](../README.md)

## About This Book
[The first edition of this book](https://github.com/getify/You-Dont-Know-JS/blob/1st-ed/this%20&%20object%20prototypes/README.md#you-dont-know-js-this--object-prototypes) is titled _this & Object Prototypes_. That focus of that edition started with the `this` keyword: arguably one of the most confused topics in all of JavaScript. The book was mostly focused on the prototype system and advocating for the lesser-known "delegation" pattern over class designs. At the time of writing (2014), ES6 was still almost 2 years to completion.

A lot has changed in the JavaScript landscape in the 8 years since the publishing of that first edition. ES6 is old now, and at the time of this book's writing, JavaScript has seen 7 yearly updates since ES6 (ES2016 through ES2022).

This book will still talk about how `this` works. `class` actually operates mostly via the prototype chain under the covers. JavaScript developers in 2022 are almost never writing code to explicitly wire up prototypal inheritance anymore. Class design patterns are how the majority of data and behavior organization (data structures) in JavaScript are expressed.

This book reflects JavaScript's current reality: thus the new sub-title, new organization and focus of topics, and complete re-write of the previous edition's text.

[▲ Return to Sections](#sections)

## Objects As Containers
Objects are collections of key/value pairs. There are sub-types of object in JavaScript with specialized behaviors, such as:
* Arrays - numerically indexed
* Functions - callable

**NOTE**: Keys are often referred to as _property names_. A pairing of _property name_ and _value_ is called a _property_.

Regular JavaScript objects are typically declared with literal syntax:

```javascript
myObj = {
  // ..
};
```

**NOTE**: The above object literal syntax is more common and preferable to using the new object syntax (`myObj = new Object();`).

[▲ Return to Sections](#sections)

| [Table of Contents](../README.md#table-of-contents) |
