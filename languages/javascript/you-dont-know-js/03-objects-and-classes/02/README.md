# Chapter 2: How Objects Work
Objects are not just containers for multiple values. To fully understand the object mechanism in JavaScript, a closer look at a number of characteristics of objects and their properties is required.

The characteristics that define the underlying behavior of objects are collectively referred to, in formal terms, as the _[metaobject](https://en.wikipedia.org/wiki/Metaobject) proObject Foundationstocol_ (MOP). The MOP is useful for understanding how objects behave in JavaScript, as well as for understanding how to override the default behaviors of objects to better suit the needs of a given program.

## Sections
* [Property Descriptors](#property-descriptors)
  * [Accessor Properties](#accessor-properties)

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

#### Accessor Properties
A _property descriptor_ usually defines a `value` property, but it can also contain _accessor properties_ (getters/setters). A property descriptor containing accessor properties may not contain a `value` property, but may look like:

```javascript
{
  get() { .. },    // function to invoke when retrieving the value
  set(v) { .. },   // function to invoke when assigning the value
  // .. enumerable, etc
}
```

Property access on a property that has a getter defined may be invoked in the normal way (`obj.prop`), but under the hood the property's `get()` method is invoked. Property assignment on a property that has a setter defined may be invoked in the normal way (`obj.prop = value`), but under the hood the property's `set(..)` method is invoked as if `obj.prop.set(value)` had been called.

```javascript
anotherObj = {};

Object.defineProperty(anotherObj,"fave",{
  get() { console.log("Getting 'fave' value!"); return 123; },
  set(v) { console.log(`Ignoring ${v} assignment.`); }
});

anotherObj.fave;
// Getting 'fave' value!
// 123

anotherObj.fave = 42;
// Ignoring 42 assignment.

anotherObj.fave;
// Getting 'fave' value!
// 123
```

[▲ Return to Sections](#sections)

| [Previous: Chapter 1 - Object Foundations](../01/README.md) | [Table of Contents](../README.md#table-of-contents) |
