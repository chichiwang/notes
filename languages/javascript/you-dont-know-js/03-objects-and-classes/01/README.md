# Chapter 1: Object Foundations
> Everything in JavaScript is an object.

This is one of the most pervasive, but most incorrect, myths about JavaScript. JavaScript definitely has objects, but not all values are objects. Objects are arguably the most important and varied value type in the language.

Objects are the most flexible and powerful container in JavaScript. They are the foundation for the second of JavaScript's three pillars: the prototype. Prototypes, along with the `this` keyword, are how the JavaScript object system expresses the class design pattern.

## Sections
* [About This Book](#about-this-book)
* [Objects As Containers](#objects-as-containers)
* [Defining Properties](#defining-properties)
  * [Looks Like JSON?](#looks-like-json)
  * [Property Names](#property-names)
  * [Symbols As Property Names](#symbols-as-property-names)
  * [Concise Properties](#concise-properties)
  * [Consise Methods](#concise-methods)
  * [Object Spread](#object-spread)
  * [Deep Object Copy](#deep-object-copy)
* [Accessing Properties](#accessing-properties)
  * [Object Entries](#object-entries)

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

## Defining Properties
Within an object literal's curly braces, define a property (name and value) with `propertyName: propertyValue` pairs:

```javascript
myObj = {
  favoriteNumber: 42,
  isDeveloper: true,
  firstName: "Kyle"
};
```

The values assigned to properties can be literal values or computed properties:

```javascript
function twenty() { return 20; }

myObj = {
  favoriteNumber: (twenty() + 1) * 2,
};
```

The expression `(twenty() + 1) * 2` is evaluated immediately and the result (`42`) is assigned to the property.

JavaScript does not support [lazy expressions](https://en.wikipedia.org/wiki/Lazy_evaluation), so a lazy expression cannot be assigned as a property value. The only way to achieve this is to wrap an expression in a function for assignment:

```javascript
function twenty() { return 20; }
function myNumber() { return (twenty() + 1) * 2; }

myObj = {
  favoriteNumber: myNumber   // notice, NOT `myNumber()` as a function call
};
```

`favoriteNumber()` is not holding a value, but a function reference. The function reference must be executed to obtain the result.

#### Looks Like JSON?
JavaScript object literal syntax resembles a related syntax _JSON_ (JavaScript Object Notation):

```json
{
  "favoriteNumber": 42,
  "isDeveloper": true,
  "firstName": "Kyle"
}
```

The biggest differences between JSON and object literal notation is that in JSON objects:
1. Property names must be wrapped in double quotes (`"`).
2. Property values must be literals (primitives, objects, or arrays). They cannot be JavaScript expressions, functions, etc.
3. The syntax is stricter, JSON does not allow:
  * Comments
  * Trailing commas in object or array expressions

In a JavaScript program, an object literal does not require quotes around property names, although it is allowed. Some characters are valid in a property name, but only when wrapped in quotation marks (single `'` or double `"` quotes):

```javascript
myObj = {
  favoriteNumber: 42,
  isDeveloper: true,
  firstName: "Kyle",
  "2 nicknames": [ "getify", "ydkjs" ]
};
```

#### Property Names
Property names in object literals are almost always treated as or coerced to strings. An exception to this is integer (or "integer looking") property names.

```javascript
anotherObj = {
  42:       "<-- this property name will be treated as an integer",
  "41":     "<-- ...and so will this one",

  true:     "<-- this property name will be treated as a string",
  [myObj]:  "<-- ...and so will this one"
};
```

The property names `42` and `"41"` will be treated as integer property names (index). The `true` value will be coerced to the string `"true"`. The `myObj` identifier reference, _computed_ via the surrounding `[..]`, will coerce the object's value to a string (generally the default `[object Object]`).

**WARNING**: Never rely on computed string coercion for objects used as property names - the resultant string is unlikely to be what is expected, which can lead to bugs. Instead, use a `Map`(added in ES6), where objects used as property names are left as-is, instead of being coerced to string values.

Property names can be computed in object literals by enclosing them in `[..]`:

```javascript
anotherObj = {
  ["x" + (21 * 2)]: true
};
```

The expression `"x" + (21 * 2)` is computed immediately and the result `"x42"` is used as property name.

#### Symbols As Property Names
ES6 introduced a new primitive value type: [symbol](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol). Symbols are frequently used as special property names. Symbols are created via the `Symbol(..)` function, without the `new` keyword, which can be passed an optional description string used for debugging (this string is inaccessible to the JavaScript program, it is only useful for debug output).

```javascript
myPropSymbol = Symbol("optional, developer-friendly description");
```

**NOTE**: Symbol values are completely opaque to, and totally unique within, a JavaScript program. While symbols can be created and used, JavaScript obscures the underlying value, and it cannot be operated upon.

To define a property name as a symbol, use a computed property name:

```javascript
myPropSymbol = Symbol("optional, developer-friendly description");

anotherObj = {
  [myPropSymbol]: "Hello, symbol!"
};
```

Because symbols are globally unique within the program, there is no chance of accidental collision of the prperty name.

Symbols are also useful to hook into special default behaviors of objects ([covered in the next chapter](../02/README.md).

#### Concise Properties
When defining an object literal, it is common to use a property name that's the same as an in-scope identifier that holds the value to be assigned:

```javascript
coolFact = "the first person convicted of speeding was going 8 mph";

anotherObj = {
  coolFact: coolFact
};
```

**NOTE**: The above could have been defined as `"coolFact": coolFact`, but JavaScript developers rarely quote property names unless absolutely necessary. It is idiomatic to avoid quotes unless required.

In the situation that a property name and value expression identifier are the same, the property name can be omitted from the object literal definition as a _concise property_ definition:

```javascript
coolFact = "the first person convicted of speeding was going 8 mph";

anotherObj = {
  coolFact   // <-- concise property short-hand
};
```

This assigns the property `coolFact` to the object `anotherObj` with the value contained within the variable `coolFact`.

#### Concise Methods
A similar shorthand for defining functions/methods in an object literal:

```javascript
anotherObj = {
  // standard function property
  greet: function() { console.log("Hello!"); },

  // concise function/method property
  greet2() { console.log("Hello, friend!"); }
};
```

Generator functions can also be defined by shorthand:

```javascript
anotherObj = {
  // instead of:
  greet3: function*() { yield "Hello, everyone!"; }

  // concise generator method
  *greet3() { yield "Hello, everyone!"; }
};
```

Although uncommon, concise methods/generators can have quoted or computed names:

```javascript
anotherObj = {
  "greet-4"() { console.log("Hello, audience!"); },

  // concise computed name
  [ "gr" + "eet 5" ]() { console.log("Hello, audience!"); },

  // concise computed generator name
  *[ "ok, greet 6".toUpperCase() ]() { yield "Hello, audience!"; }
};
```

#### Object Spread
Another way to add properties to an object literal is via the `...` syntax. While not technically an operator, it seems like one. This syntax is referred to as _object spread_.

When `...` is used within an object literal, it will _spread_ out the contents (key/value pairs) of another object into the object being defined:

```javascript
anotherObj = {
  favoriteNumber: 12,

  ...myObj,   // object spread, shallow copies `myObj`

  greeting: "Hello!"
}
```

The spreading of `myObj`'s properties in the example above is _shallow_: the top level properties of `myObj` are copied over and the values assigned. If any of those values are references to other objects, those references are copied over - the values are not duplicated.

Property definition operations happen in the order they are encountered, top-to-bottom. In the above example, if `myObj` has a `favoriteNumber` property, it will overwrite the earlier assignment `favoriteNumber: 12`. If `myObj` has a `greeting` property, that value will be overwritten by the `greeting: "Hello!"` assignment.

**NOTE**: Object spread only copies _owned_ properties (those directly on the object) that are _enumerable_ (allowed to be enumerated/listed). It does not duplicate the property, it merely performs a simple assignment-style copy.

A common use of `...` object spread is for creating shallow object copies:

```javascript
myObjShallowCopy = { ...myObj };
```

The `...` syntax cannot be used to spread an object into an existing object value, it must be used within the brackets `{..}` of an object literal.

To perform a shallow copy with API rather than syntax, refer to the [Object Entries](#object-entries) section of this chapter.

To shallow copy object properties into an existing object refer to the [Assigning Properties](#assigning-properties) section of this chapter.

#### Deep Object Copy
Object spread (`...`) is only suitable for shallow copies - it is best suited for duplicating objects that hold simple, primtive values rather than references to other objects.

Deep object duplication is a complex and nuanced operation:
* What does it mean to duplicate a function?
  * A special kind of object, also held by reference.
* What does it mean to duplicate an external (not entirely in JavaScript) object reference?
  * ex: DOM Element
* What happens if an object has circular references?
  * A nested descendant object holds a reference back up to an outer ancestor object.

There are a variety of opinions on how these corner cases should be handled, and no single standard exists for deep object duplication.

The standard approaches have been:
1. Use a library utility that declares a specific opinion on how the duplication behaviors/nuances should be handled.
2. Use the `JSON.parse(JSON.stringify(..))` strategy. This only works if the target object contains no circular references, and it contains no objects that cannot be properly serialized with JSON (such as functions).

Recently a third option has emerged: [structuredClone](https://html.spec.whatwg.org/multipage/structured-data.html#structured-cloning). This is not a JavaScript feature, but a companion API provided by environments like web browsers. Objects can be deep copied using this utility:

```javascript
myObjCopy = structuredClone(myObj);
```

The underlying implementation of `structuredClone(..)` supports duplicating circular references, and many more types of values than the `JSON` trick. However, it still has limits including no support for cloning functions or DOM elements.

[▲ Return to Sections](#sections)

## Accessing Properties
Property access of an object is typically done with the `.` operator:

```javascript
myObj.favoriteNumber;    // 42
myObj.isDeveloper;       // true
```

If a property name contains characters that cannot appear in identifiers (leading numbers, whitespace), `[..]` brackets can be used instead of `.`:

```javascript
myObj["2 nicknames"];    // [ "getify", "ydkjs" ]
```

Even though numeric property names remain numbers, propery access via `[..]` will coerce a string representation to a number (`"42"` as the numeric `42` equivalent), and then access the associated numeric property name:

```javascript
anotherObj[42];          // "<-- this property name will..."
anotherObj["41"];        // "<-- this property name will..."
```

A computed property name can be accessed with the `[..]` brackets as well:

```javascript
propName = "41";
anotherObj[propName];
```

Any expression placed within the `[..]` brackets will be evaluated first, and the resulting value will be used in the property name lookup on the target object:

```javascript
function howMany(x) {
  return x + 1;
}

myObj[`${ howMany(1) } nicknames`];   // [ "getify", "ydkjs" ]
```

In the above example, the result of the expression within the brackets, `"2 nicknames"` is used as the property name access on the object `myObj`.

#### Object Entries
`Object.entries(..)` can be used to retrieve a list of the properties in an object, as an array of tuples containing the key/value pairs:

```javascript
myObj = {
  favoriteNumber: 42,
  isDeveloper: true,
  firstName: "Kyle"
};

Object.entries(myObj);
// [ ["favoriteNumber",42], ["isDeveloper",true], ["firstName","Kyle"] ]
```

`Object.entries(..)` (added in ES6) returns the enumerable properties (see the [Property Descriptors](../02/README.md#property-descriptors) section of the next Chapter) of a target object.

It is possible to create a new object from list of entries using `Object.fromEntries(..)` (added in ES2019):

```javascript
myObjShallowCopy = Object.fromEntries( Object.entries(myObj) );

// alternate approach to the earlier discussed:
// myObjShallowCopy = { ...myObj };
```

[▲ Return to Sections](#sections)

| [Table of Contents](../README.md#table-of-contents) |
