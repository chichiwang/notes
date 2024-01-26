# Chapter 2: How Objects Work
Objects are not just containers for multiple values. To fully understand the object mechanism in JavaScript, a closer look at a number of characteristics of objects and their properties is required.

The characteristics that define the underlying behavior of objects are collectively referred to, in formal terms, as the _[metaobject](https://en.wikipedia.org/wiki/Metaobject) proObject Foundationstocol_ (MOP). The MOP is useful for understanding how objects behave in JavaScript, as well as for understanding how to override the default behaviors of objects to better suit the needs of a given program.

## Sections
* [Property Descriptors](#property-descriptors)

[◂ Return to Table of Contents](../README.md)

## Property Descriptors
Each property on an object is described by a _property descriptor_, an object (metaobject) with properties dictcating how a given property behaves.

A property's property descriptor can be retrieved with `Object.getOwnPropertyDescriptor(..)`:

```javascript
myObj = {
  favoriteNumber: 42,
  isDeveloper: true,
  firstName: "Kyle"
};

Object.getOwnPropertyDescriptor(myObj,"favoriteNumber");
// {
//     value: 42,
//     enumerable: true,
//     writable: true,
//     configurable: true
// }
```

A property descriptor object can be used to create a new property on an object using `Object.defineProperty(..)`:

```javascript
anotherObj = {};

Object.defineProperty(anotherObj,"fave",{
  value: 42,
  enumerable: true,     // default if omitted
  writable: true,       // default if omitted
  configurable: true    // default if omitted
});

anotherObj.fave;          // 42
```

If an existing property hasn't been defined as non-configurable, with `configurable: false` in its descriptor, it can be redefined/overwritten using `Object.defineProperty(..)`.

**WARNING**: Sections in the [previous chapter](../01/README.md) refer to _copying_ or _duplicating_ properties. All of these operations perform simple `=` assignment, which ignores the underlying descriptor for a property.

Although less common, it is possible to define multiple properties with descriptors for a given object:

```javascript
anotherObj = {};

Object.defineProperties(anotherObj,{
  "fave": {
    // a property descriptor
  },
  "superFave": {
    // another property descriptor
  }
});
```

[▲ Return to Sections](#sections)

| [Previous: Chapter 1 - Object Foundations](../01/README.md) | [Table of Contents](../README.md#table-of-contents) |
