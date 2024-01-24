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
  * [Destructuring](#destructuring)
  * [Conditional Property Access](#conditional-property-access)
  * [Accessing Properties On Non-Objects](#accessing-properties-on-non-objects)
* [Assigning Properties](#assigning-properties)
* [Deleting Properties](#deleting-properties)
* [Determining Container Contents](#determining-container-contents)
  * [Better Existence Check](#better-existence-check)
  * [Listing All Container Contents](#listing-all-container-contents)
* [Temporary Containers](#temporary-containers)
* [Containers Are Collections Of Properties](#containers-are-collections-of-properties)

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

#### Destructuring
_Object destructuring_ (added in ES6) is another approach to accessing properties in an object. Destructuring can be thought of as defining a _pattern_ that describes what the object is supposed to look like structurally, then asking JavaScript to follow that pattern to access the contents of an object value.

The end result of object destructuring is one or more assignments to other targets (variable(s)) of the values from the source object.

The following variable assignments from an object's properties:

```javascript
myObj = {
  favoriteNumber: 42,
  isDeveloper: true,
  firstName: "Kyle"
};

const favoriteNumber = (
  myObj.favoriteNumber !== undefined ? myObj.favoriteNumber : 12
);
const isDev = myObj.isDeveloper;
const firstName = myObj.firstName;
const lname = (
  myObj.lastName !== undefined ? myObj.lastName : "--missing--"
);
```

Would look like the following, using declarative object destructuring syntax:

```javascript
myObj = {
  favoriteNumber: 42,
  isDeveloper: true,
  firstName: "Kyle"
};

const { favoriteNumber = 12 } = myObj;
const {
  isDeveloper: isDev,
  firstName: firstName,
  lastName: lname = "--missing--"
} = myObj;

favoriteNumber;   // 42
isDev;            // true
firstName;        // "Kyle"
lname;            // "--missing--"
```

In the example above, the `{..}` brackets resemble an object literal definition, but appear on the left side of the `=` assignment operator denoting it as a destructuring pattern rather than an object definition.

The `{ favoriteNumber } = myObj` destructuring assigns the value of a property in `myObj` named `favoriteNumber` to an identifier of the same name. The `= 12` provides a default value to the `favoriteNumber` identifier if a corresponding property name is not found in `myObj` (or the `favoriteNumber` property value is `undefined`).

`firstName: firstName` is providing a source and target for the value assignment, but it is redundant and unnecessary: a single `firstName` would have sufficed, and is generally preferred.

`lastName: lname = "--missing--"` combines source-target renaming and providing a default value.

The above examples combine object destructuring and variable declarations (`const` in this case), but object destructuring is not inherently a declaration mechanism. Destructuring is about access and assignment (source to target), so it operates well against existing targets:

```javascript
let fave;

// surrounding ( ) are required syntax here,
// when a declarator is not used
({ favoriteNumber: fave } = myObj);

fave;  // 42
```

Object destructuring syntax is preferred for its declarative style over the heavily imperative pre-ES6 variants.

#### Conditional Property Access
[Optional chaining](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Optional_chaining) was added to JavaScript in ES6. The primary form of optional chaining is a two-character compound operator: `?.`

In the expression `A?.B` the optional chaining operator will check if the left-hand side reference (`A`) is null'ish (`null` or `undefined`), and if so it will short-circuit the property access and return `undefined`. If no left-hand side references are null'ish in the property access chain, it will behave as normal.

```javascript
myObj?.favoriteNumber
```

In the above example, if `myObj` is `null` or `undefined`, then `undefined` will be returned, otherwise the result of accessing the `favoriteNumber` property will be returned. The optional chaining operator does not verify that `myObj` is an actual object, nor that the `favoriteNumber` property exists on it. All non-null'ish values in JavaScript can _safely_ be accessed via the `.` operator (no exceptions thrown).

Typically `?.` is used in nested property accesses:

```javascript
myObj?.address?.city
```

The equivalent retrieval without using `?.` looks like:

```javascript
(myObj != null && myObj.address != null) ? myObj.address.city : undefined
```

The `?.` operator should not be used everywhere properties are being accessed. Use of `?.` should be reserved for accessing properties on objects where the nature of the values accessed are subject to conditions that cannot be predicted/controlled.

In the previous example, it is unlikely that a chain property access would begin with a variable that may not even reference an object. It is more likely that the top level variable may be missing object properties depending on conditions. More likely the property access chain would look more like:

```javascript
myObj.address?.city
```

Another form of optional chaining is `?.[`, used when the property being accessed requires `[..]` brackets:

```javascript
myObj["2 nicknames"]?.[0];   // "getify"
```

**WARNING**: There is a third form of optional chaining, called [optional call](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Optional_chaining#optional_chaining_with_function_calls) whose operator looks like `?.(`. This is used to perform a non-null'ish check on a property before executing a function call on it: `myObj.someFunc?.(42)`.

The author recommends avoiding the optional call operator because it does not garuantee that a property is callable, just that it is non-null'ish. It is Kyle Simpson's opinion that this can lead to confusion, with users/readers assuming that the optional call ensures a property is callable when that is not what it does.

#### Accessing Properties On Non-Objects
Properties/methods can be accessed on values that are not objects:

```javascript
fave = 42;

fave;              // 42
fave.toString();   // "42"
```

If property access is performed on a non-object, non-null'ish value (like the primitive `42` in the above example), JavaScript will temporarily wrapp the value into an object-wrapped representation, allowing property access against the implicitly instantiated object. This process is typically called _boxing_ (putting the value inside a _box_, specifically an object container).

In the above snippet, JavaScript temporarily boxes the value `42` into a `Number` object container so that the method access `fave.toString()` can be performed on it.

`null` and `undefined` can be boxed manually by calling `Object(null)`/`Object(undefined)` to allow property access, however JavaScript will not automatically box these values - property access will fail.

**NOTE**: The counter-operation to _boxing_ is _unboxing_. JavaScript will unbox values to perform operations on them, for example `Number(42)` or `Object(42)` would be unwrapped when encountering mathetmatical operations on them (`+`, `*`, etc).
 
[▲ Return to Sections](#sections)

## Assigning Properties
As with any other variable assignment, object properties are assigned with the `=` operator:

```javascript
myObj.favoriteNumber = 123;
```

If `favoriteNumber` is not already defined as a property on `myObj`, the above statement will create the property and assign the value. If `favoriteNumber` already exists as a property on `myObj`, the above statement will re-assign it.

**WARNING** An `=` assignment of a property may fail (either silently or throwing an exception), or it may not directly assign a value but invoke a _setter_ function that may perform other operations.

It is possible to assign one or more properties at once, assuming the source properties (`key`/`value` pairs) are in another object, using the `Object.assign(..)` method:

```javascript
// shallow copy all (owned and enumerable) properties
// from `myObj` into `anotherObj`
Object.assign(anotherObj,myObj);

Object.assign(
  /*target=*/anotherObj,
  /*source1=*/{
    someProp: "some value",
    anotherProp: 1001,
  },
  /*source2=*/{
    yetAnotherProp: false
  }
);
```

`Object.assign(..)` takes the first object passed as argument as target, and each subsequent object as sources. Copying is done in the same manner described in the [Object Spread](#object-spread) section.
 
[▲ Return to Sections](#sections)

## Deleting Properties
The only way to remove a property from an object is with the `delete` operator:

```javascript
anotherObj = {
  counter: 123
};

anotherObj.counter;   // 123

delete anotherObj.counter;

anotherObj.counter;   // undefined
```

The `delete` operator does not directly perform any deallocation/freeing up of memory through garbage collection. It simply removes a property from an object. If the value in the property was a reference (to another object) and there are no other surviving references to that value after the property is removed, the value is likely eligible for for removal in a future sweep of the GC.

Calling `delete` on anything other than an object property will fail either silently (non-strict mode) or throw an exception (strict mode).

Deleting a property from an object is distinctly different than assigning that property a value of `null` or `undefined`. A property with value `null`/`undefined` is still present in the object and is enumerable.
 
[▲ Return to Sections](#sections)

## Determining Container Contents
To determine if an object has a specific property:

```javascript
myObj = {
  favoriteNumber: 42,
  coolFact: "the first person convicted of speeding was going 8 mph",
  beardLength: undefined,
  nicknames: [ "getify", "ydkjs" ]
};

"favoriteNumber" in myObj;            // true

myObj.hasOwnProperty("coolFact");     // true
myObj.hasOwnProperty("beardLength");  // true

myObj.nicknames = undefined;
myObj.hasOwnProperty("nicknames");    // true

delete myObj.nicknames;
myObj.hasOwnProperty("nicknames");    // false
```

The `in` operator checks the target specified and its `[[PROTOTYPE]]` chain for a specific property. `hasOwnProperty(..)` only checks the target object.

`hasOwnProperty(..)` is defined as a built-in method on `Object.prototype`, which is inherited by all normal objects.

#### Better Existence Check
ES2022 introduces a new feature: `Object.hasOwn(..)` which does the same thing as `hasOwnProperty(..)` but it is invoked as a static helper external to the object value rather than being accessed through the object's `[[PROTOTYPE]]`. This is safer for consistent usage:

```javascript
// instead of:
myObj.hasOwnProperty("favoriteNumber")

// we should now prefer:
Object.hasOwn(myObj,"favoriteNumber")
```

A simple polyfill for this feature:

```javascript
// simple polyfill sketch for `Object.hasOwn(..)`
if (!Object.hasOwn) {
  Object.hasOwn = function hasOwn(obj,propName) {
    return Object.prototype.hasOwnProperty.call(obj,propName);
  };
}
```

#### Listing All Container Contents
Using `Object.entries(..)` to retrieve the properties of an object [was discussed in another section of this chapter](#object-entries). Similarly, `Object.keys(..)` can be used to retrieve a list of the enumerable property names of an object, and `Object.values(..)` will return a list of the enumerable property values.

`Object.getOwnProperyNames(..)` can be used to retrieve all the keys in an object, enumerable or not. This method will not return any symbol property names, though - symbol property names are treated as special locations on an object. `Object.getOwnPropertySymbols(..)` will return that list of symbol property names.

All of the above methods only return properties (names, values) on an object itself, and will not traverse the prototype chain. The `in` operator will potentially traverse the entire chain to check for the existence of a property, and a `for .. in` loop will traverse the chain and list any enumerable properties.

There is no built-in API to traverse an entire prototype chain and return a list of the combined set of both owned and inherited contents.
 
[▲ Return to Sections](#sections)

## Temporary Containers
Objects can be used as temporary transport mechanisms, such as when passing multiple values to a function via a single argument, or when a function returns multiple values:

```javascript
function formatValues({ one, two, three }) {
  // the actual object passed in as an
  // argument is not accessible, since
  // we destructured it into three
  // separate variables

  one = one.toUpperCase();
  two = `--${two}--`;
  three = three.substring(0,5);

  // this object is only to transport
  // all three values in a single
  // return statement
  return { one, two, three };
}

// destructuring the return value from
// the function, because that returned
// object is just a temporary container
// to transport us multiple values
const { one, two, three } =

  // this object argument is a temporary
  // transport for multiple input values
  formatValues({
    one: "Kyle",
    two: "Simpson",
    three: "getify"
  });

one;     // "KYLE"
two;     // "--Simpson--"
three;   // "getif"
```

The argument passed to `formatValues(..)` is parameter destructured, so the function only deals with the individual variables `one`, `two`, and `three`. The return value of `formatValues(..)` is also destructured so the calling scope is dealing with individual variables as well.
 
[▲ Return to Sections](#sections)

## Containers Are Collections Of Properties
The most common usage of objects is as containers for multiple values, created and managed by:
* defining properties (named locations), either at object creation time or later
* assigning values, either at object creation time or later
* accessing values later, using location names (property names)
* deleting properties using `delete`
* determining container contents with utilities such as `in`, `hasOwnProperty(..)`/`hasOwn(..)`, `Object.entries(..)`, `Object.keys(..)`, etc.

There is a lot more to objects that just static collections of property names and values. The next chapter will explore how they actually work.
 
[▲ Return to Sections](#sections)

| [Table of Contents](../README.md#table-of-contents) | [Next: Chapter 2 - How Objects Work](../02/README.md) |
