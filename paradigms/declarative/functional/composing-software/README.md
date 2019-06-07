# Notes: Composing Software
**Author**: [Chi-chi Wang](https://github.com/chichiwang)

Notes on high level functional programming concepts in the context of JavaScript, taken from the book [Composing Software](https://leanpub.com/composingsoftware) by [Eric Elliott](https://leanpub.com/u/_ericelliott).

Notes ported over from Google Docs, built between November 7, 2018 and April 20, 2019.

## Table of Contents
* [Functional Composition](#functional-composition)
* [Object Composition](#object-composition)
* [The Rise of Functional Programming](#the-rise-of-functional-programming)
* [The Fall of Functional Programming](#the-fall-of-functional-programming)
* [The Rise of Functional Programming (again)](#the-rise-of-functional-programming-again)
* [Why Learn Functional Programming in JavaScript?](#why-learn-functional-programming-in-javascript)
* [JavaScript Mechanics](#javascript-mechanics)
* [Higher Order Functions](#higher-order-functions)
* [Reduce](#reduce)
* [Functors and Categories](#functors-and-categories)
* [Functional Mixins](#functional-mixins)
* [Factory Functions](#factory-functions)
* [Composition With Classes](#composition-with-classes)
* [JavaScript Monads](#javascript-monads)
  * [Terms](#terms)
  * [Example](#example)
  * [What Monads Are Made Of](#what-monads-are-made-of)
  * [The Monad Laws](#the-monad-laws)
* [Resources](#resources)

## Functional Composition
Function composition is the process of applying a function to the output of another function.

Examples of functional composition:
* Promise chains
* Chaining function calls
* Passing the return value of one function into the input of another function
* Calling two methods in sequence (passing the context object)

[Pipe](https://en.wikipedia.org/wiki/Pipeline_(software)): a pipeline of functions, passing the output of one function into the input of the next. No need for intermediary variables.

[Point-free style](https://en.wikipedia.org/wiki/Tacit_programming): writing functions without mention of the arguments.
* Done by calling a function that returns a new function
* Leads to more concise code expression
  * Leads to enhanced comprehension leading to easier-to-understand code
* Reduced surface area of error

## Object Composition
Any time you build any non-primitive data structure, you’re performing some kind of object composition.

**Composite pattern**: a specific type of recursive object composition which allows you to treat individual components and aggregated composites identically. This is only one form of object composition.

Three types of object compositional relationships catalogued by the Gang of Four:
* Delegation
  * Used in state, strategy, and visitor patterns
* Acquaintance
  * An object knows about another object by reference, generally in a *uses a* relationship
* Aggregation
  * Child objects form part of a parent object, in a *has a* relationship

**Class Inheritance** is just one kind of composite object construction. All classes produce composite objects, but not all composite objects are produced by classes or class inheritance.

Gang of Four recommends to “Favor object composition over class inheritance.” This means to form composite objects from small composite parts rather than inheriting all properties from an ancestor in a class hierarchy.

**Class inheritance causes the following problems in OOD**:
* **Tight coupling**: child classes are dependent on the implementation of the parent class leading to the tightest coupling available in OOD
* **Fragile base class**: because of coupling, changes to a base class can potentially break a large number of descendant classes
* **Inflexible hierarchy**: with single ancestor taxonomies all class taxonomies are eventually wrong for new use cases
* **Duplication by necessity**: due to inflexible hierarchies new use cases are often implemented by duplication rather than extension
* **Gorilla/banana**: *“…the problem with object-oriented languages is they’ve got all this implicit environment that they carry around with them. You wanted a banana but what you got was a gorilla holding the banana and the entire jungle.”* ~ Joe Armstrong

The most common form of object composition in JavaScript: object concatenation (mixin composition).

### Conclusion: Composition
Despite the paradigm (FP vs OOP) you cannot escape composing functions and data structures.

No matter how you write software, you should compose it well. **The essence of software development is composition.**

## The Rise of Functional Programming
Alonzo Church and Alan Turing produced two different, but equivalent, universal models of computation.

Alonzo Church invented lambda calculus, a universal model of computation based on function application.

Alan Turing invented the turing machine, a universal model of computation that defines a theoretical device that manipulates symbols on a strip of tape.

They collaborated together to show that lambda calculus and the turing machine are functionally equivalent.

Lambda calculus is all about function composition. There are **three unique attributes** key to lambda calculus:
* **Functions are always anonymous**
* **Functions are unary**
  * They only accept a single input
  * If you need more than one parameter, the function will take the first input and return another function that takes the next, etc.
  * The transformation from an n-ary function to a unary function is known as **currying**
* **Functions are first-class**
  * Functions can be taken as inputs and functions can be returned from functions

**Classic function composition** - takes the output of one function and uses it as the input for another function. Note: *Function execution in most notations occurs right to left*.

`f . g` can be written as `f => g => x => f(g(x))`

When software is composed, it can be represented by a graph of function compositions.

Lambda calculus was highly influential on software design. Prior to 1980 many influential icons of computer science were building software using function composition. Lisp, created in 1958, and the 2nd oldest language still in popular use today, was heavily influenced by lambda calculus.

## The Fall of Functional Programming
**Between 1970 and 1980** the way software was composed drifted away from simple algebraic math to a list of linear instructions for the computer to follow (K&R C in 1978 and BASIC interpreters of early home computers of the 1970s and early 1980s).

**In 1982 Alan Kay’s Smalltalk was formalized**, which pushed forward the idea of objects as the atomic unit of composition. **In the 1980s and 1990s** the ideas of component encapsulation and message passing got distorted in C++ and Java into ideas about inheritance hierarchies and is-a relationships for feature reuse.

> For most of us, creating software was a bit of a nightmare for 30 years. Dark times. ;)

## The Rise of Functional Programming (again)
**Around 2010, something great began to happen: JavaScript exploded**. People started whispering in the shadows about this cool new thing called “functional programming”.

By 2015, the idea of building software with function composition was popular again.

**The truth: functional programming has always been alive and well**
* Smalltalk was a popular enterprise software solution at Fortune 500 companies like JPMorgan Chase
* Lisp was used at NASA JPL to program the Mars Rover
* Lisp has always been used at Stanford Research Institute (SRI) for artificial intelligence and bioinformatics research
* Hacker News was written in Ark, a dialect of Lisp.
* Clojure, a dialect of Lisp, was created by Rich Hickey in 2007 came into popular use at companies like Amazon, Apple, and Facebook
* Erlang, a popular functional language, was developed by Ericsson for use in telephone exchanges
  * Still used by T-Mobile
  * Amazon uses Erlang for cloud technologies: SimpleDB, Elastic Compute Cloud (EC2)

## Why Learn Functional Programming in JavaScript?
JavaScript has the most important features needed for functional programming
* **First class functions**: the ability to use functions as data values
* **Anonymous functions and lambda syntax**: makes it easier to work with higher order functions
* **Closures**: the bundling of a function with its lexical environment

Features that some functional languages have that JavaScript does not
* **Purity**: some languages prevent expressions with side effects
  * In JavaScript this must be achieved by discipline and convention
* **Immutability**: some languages disable mutations to data structures
  * JavaScript lacks efficient, immutable [trie-based](https://en.wikipedia.org/wiki/Trie) data structures
  * Libraries can help: immutable.js, Mori, etc.
* **Recursion-only**: in some languages, recursion is the **only** way to iterate
  * JavaScript supports recursion but lacks tail-call optimizations. Without tail-call recursion it isn’t safe to use recursion for large iterations

While it may not be absolutely ideal for every programming style, JavaScript is unapologetically a general-purpose language designed to be usable by various people with various programming styles and backgrounds.

According to Brendan Eich, this was intentional from the beginning. Netscape had to support two kinds of programmers:
> “…the component authors, who wrote in C++ or (we hoped) Java; and the ‘scripters’, amateur or pro, who would write code directly embedded in HTML.”

The ideas in Brendan Eich’s head from the beginning:
* Scheme in the browser
* Look like Java

> “Apps ate the world, the web ate apps, and JavaScript ate the web.”

JavaScript is now the most popular programming language in the world.

JavaScript is not the ideal tool for functional programming, but it’s a great tool for building large applications on very large, distributed teams, where different teams may have different ideas about how to build an application.

In JavaScript all approaches to building software can co-exist. The true strength of JavaScript is diversity of thought and users in the ecosystem.

As-is, JavaScript is already a *good enough* functional programming language, meaning that people are building all kinds of useful and interesting things in JavaScript, using functional programming techniques.
* Netflix (and every app built with Angular 2+) uses functional utilities based on RxJS
* Facebook uses the concepts of pure functions, higher-order functions, and higher order components in React to build Facebook and Instagram
* PayPal, KhanAcademy, and Flipkart use Redux for state management

“Why JavaScript?” Because JavaScript is the language that most real companies are using to build real software. Love it or hate it, JavaScript has stolen the title of “most popular functional programming language” from Lisp, which was the standard bearer for decades.

## JavaScript Mechanics
An expression is a chunk of code that evaluates to a value.

JavaScript supports the following variable declarations: `var`, `let`, and `const`.

The strictest being `const`, which can’t be reassigned - the final value must be defined at declaration time.

The variable declaration `let` is block-scoped. Variable assignments using `let` are scoped to the block they are declared in.

The variable declaration `var` is function-scoped. **These variable definitions are not block scoped.**

Mutating an existing object rather than creating a new object is usually a bug. At the very least, it is error-prone.

Both objects and arrays support destructuring, meaning that you can extract values from them and assign them to named variables.

JavaScript has function expressions, which can be assigned to names. In JavaScript, the value of a function expression is the function itself. If you want to apply a function to some arguments, you must invoke it with a function call. A function call applies a function to its arguments and evaluates to a return value.

Functions have **signatures**, which consist of:
* An *optional* function name
* A list of parameter types, in parentheses. The parameters may optionally be named
* The type of the return value
  * Type signatures don’t need to be specified in JavaScript. The JavaScript engine will figure out the types at runtime

JavaScript supports default parameter values. JavaScript functions can take object literals as arguments and use destructuring assignment in the parameter signature in order to achieve the equivalent of named arguments.

A common feature of functions in JavaScript is the ability to gather together a group of remaining arguments in the functions signature using the rest operator: `…`

JavaScript lacks a built-in autocurry mechanism, but you can import one from a library (like Lodash) or build your own.

## Higher Order Functions
A **higher order function** is a function that takes a function as an argument, or returns a function. Higher order function is in contrast to **first order functions**, which don’t take a function as an argument or return a function as output.

JavaScript has **first-class** functions meaning functions can be:
* Assigned as an identifier (variable) value
* Assigned to object property values
* Passed as arguments
* Returned from functions

You can use higher order functions to make a function polymorphic. Higher order functions can be a whole lot more reusable and versatile than their first order cousins.

## Reduce
Also known as: fold, accumulate, inject

Reduce is commonly used in functional programming that lets you iterate over a list, applying a function to an accumulated value and the next item in the list, until the iteration is complete and the accumulated value gets returned. Frequently, **it’s the most elegant way to do any non-trivial processing on a collection of items**.

Reduce takes a reducer function and an initial value, and returns the accumulated value.

Normally, `reduce()` works left to right. In JavaScript, we also have `[].reduceRight()`, which works right to left.

Reduce is versatile. It’s easy to define `map()`, `filter()`, `forEach()` and lots of other interesting things using reduce.

**Map**: our accumulated value is a new array with a new element for each value in the original array. The new values are generated by applying the passed in mapping function to each element in the array argument.

**Filter**: works in much the same way as map, except that we take a predicate function and conditionally append the current value to the new array if the element passes the predicate check.

**Compose**: create a chain of function invocations that takes an argument and passes it to the innermost function. The return value is then passed to the next innermost function as input and so on. A compose function can be built using `reduceRight()`.

**Pipe**: a reverse-compose useful if you want to represent a sequence of events rather than mathematical function composition, from the inside-out. A pipe function can be built using `reduce()`.

## Functors and Categories
A **functor data type** is something you can map over. It’s a container which has an interface which can be used to apply a function to the values inside it. When you see a functor, you should think *“mappable”*.

Functor types are typically represented as an object with a `.map()` method that maps from inputs to outputs while preserving structure. In practice, “preserving structure” means that the return value is the same type of functor (though values inside the container may be a different type).

An array is a good example of a functor, but many other kinds of objects can be mapped over as well, including promises, streams, trees, objects, etc…

**Functors are really about applying a function in a specific context.**

**Functor laws**:
* Identity
  * If you pass the identity function `(x => x)` into `f.map()` where `f` is any functor, the result should be equivalent to `f`.
* Composition
  * Functors must obey the composition law: `F.map(x => f(g(x)))` is equivalent to `F.map(g).map(f)`

Functors are a mapping between categories and therefore must respect identity and composition.

**Foundation of category theory**:

* A category is a collection of objects and arrows between objects (where “object” can mean literally anything).
* Arrows are known as morphisms. Morphisms can be thought of and represented in code as functions.
* For any group of connected objects, `a -> b -> c`, there must be a composition which goes directly from `a -> c`.
* All arrows can be represented as compositions (even if it’s just a composition with the object’s identity arrow). All objects in a category have identity arrows.

Composition is associative. Basically that means that when you’re composing multiple functions (morphisms if you’re feeling fancy), you don’t need parenthesis.

**Endofunctors**: a functor that maps from a category back to the same category.
* A functor can map from category to category: `X -> Y`
* An endofunctor maps from a category to the same category: `X -> X`
* A **monad** is an endofunctor

**Example of a Functor**

```javascript
const Identity = value => ({
  map: fn => Identity(fn(value))
});
```

To test the adherence to functor laws of `Identity` we create a simple first order function:
```javascript
// trace() is a utility to let you easily inspect
// the contents.
const trace = x => {
  console.log(x);
  return x;
};
```

Test - law of identity:
```javascript
const u = Identity(2);

// Identity law
u.map(trace);             // 2
u.map(x => x).map(trace); // 2
```

Test - law of composition:
```javascript
const f = n => n + 1;
const g = n => n * 2;

// Composition law
const r1 = u.map(x => f(g(x)));
const r2 = u.map(g).map(f);

r1.map(trace); // 5
r2.map(trace); // 5
```

**Why Functors?**

Functors are great higher-order abstractions that allow you to create a variety of generic functions that will work for any data type.

## Functional Mixins
Functional mixins:
* Composable factory functions
* Don’t require a base factory or constructor
* Provides data privacy/encapsulation
* Inheritance of private state
* Inherit from multiple sources
* No diamond problem (property collision ambiguity)
* Last in wins
  * No base class requirement

**Motivation**

The atomic units of composition are 1 of 2 things: functions or data structures. Application structure is defined by the composition of these atomic units.

Class inheritance:
* Leads to is-a thinking
* Is the tightest coupling available in object-oriented design
  * Child classes are dependent on the implementation of their parent classes
* Changes to a base class can potentially break a large number of dependent classes
* Inflexible hierarchy
  * With single ancestor taxonomies, given enough time and evolution, all class taxonomies are eventually wrong for new use-cases.
* New cases are often implemented by duplication, rather than extension. Once duplication sets in, it’s not obvious which class new classes should descend from, or why.
* *“…the problem with object-oriented languages is they’ve got all this implicit environment that they carry around with them. You wanted a banana but what you got was a gorilla holding the banana and the entire jungle.”* ~ Joe Armstrong

**Mixins**

> “Favor object composition over class inheritance”

The Gang of Four, *“Design Patterns: Elements of Reusable Object Oriented Software”*

Mixins are a form of object composition, where component features get mixed into a composite object so that properties of each mixin become properties of the composite object.

You start with an empty object and mix in features to extend it. JavaScript supports dynamic object extension and objects without classes making mixins trivially easy in this language.

**Functional Inheritance**

Functional inheritance is the process of inheriting features by applying an augmenting function to an object instance. Because child functions are heavily coupled to parent functions, you opt into most of the common problems of object inheritance.

**Functional Mixins**

Functional mixins are composable functions which mix new properties or behaviors with properties from a given object. Functional mixins don’t depend on or require a base factory or constructor: Simply pass any arbitrary object into a mixin, and it will be extended.

“You should always use the simplest possible abstraction to solve the problem you’re working on. Start with a pure function. If you need an object with persistent state, try a factory function. If you need to build more complex objects, try functional mixins.”

Good use-cases for functional mixins:
* Application state management
* Cross-cutting concerns and services (ie: centralized logger)
* Composable, functional data types
  * JS array type implements Semigroup, Functor, Foldable

**Caveats**

Avoid pitfalls of functional mixins:
* Use the simplest practical implementation. Start on the left and move to the right only as needed: pure functions > factories > functional mixins > classes.
* Avoid the creation of **is-a** relationships between objects, mixins, or data types.
* Avoid implicit dependencies between mixins  wherever possible, functional mixins should be self-contained, and have no knowledge of other mixins.
* “Functional mixins” doesn’t mean “functional programming”.
* There may be side-effects when you access a property using `Object.assign()` or object spread syntax (`{...}`). You’ll also skip any non-enumerable properties. ES2017 added `Object.getOwnPropertyDescriptors()` to get around this problem.

**Stamps**

At larger, enterprise level applications, rather than functional mixins you may want to look at [stamps](https://github.com/stampit-org/stamp-specification). The Stamp Specification is a standard for sharing and reusing composable factory functions, with built-in mechanisms to deal with property descriptors, prototype delegation, and so on.

**Class Inheritance**

Class inheritance is rarely (if ever) the best approach in JavaScript. Third party libraries may use them and in these cases it can be practical to use classes provided the library:
* Does not require you to extend your own classes
* Does not require you to use the `new` keyword

In some browsers, classes may provide JavaScript engine optimizations that are not available otherwise. In almost all cases these optimizations will not have a significant impact on your app’s performance.

Library maintainers may want to look into performance optimizations provided by classes in JavaScript. Most of the time, however, you should optimize for clean, flexible code instead of worrying about performance.

**Implicit Dependencies**

It is valid to require a lifted data type for a functional mixin to act on, but if that’s the case, the API contract should be made explicitly clear in the function signature and API documentation.

If you’re using TypeScript or Flow, it’s probably better to declare an explicit interface for your object requirements.

**Tips for functional mixins and functional programming**

* “Functional” in the context of functional mixins does not always have the same purity connotations as “functional programming”
* If you need to return the object instance, always return this instead of a reference to the instance object in the closure -- in functional code, chances are those are not references to the same objects.
* Always assume that the object instance will be copied by assignment.
* If you set non-enumerable properties, they will probably not work on the final object.
* If you’re using functional mixins that you didn’t create in your functional code, don’t assume the code is pure. Assume that the base object may be mutated, and assume that there may be side-effects & no referential transparency guarantees, i.e., it is frequently unsafe to memoize factories composed of functional mixins.

## Factory Functions
A factory function is any function, which is not a class or constructor, that returns a new object. In JavaScript when a function returns a new object using the `new` operator it is a factory function.

Functional composition is a way to build new objects up from scratch. Factory functions are a way to wrap a friendly API around the implementation details of building these objects up from scratch.

> “Sometimes, the elegant implementation is just a function. Not a method. Not a class. Not a framework. Just a function.”

~ John Carmack

## Composition With Classes
What the `new` keyword actually does:
* Creates a new object and binds `this` to it in the constructor function
* Implicitly returns `this`
* Sets the instance `[[prototype]]` to `Constructor.prototype`
* Sets the `instance.constructor === Constructor`

Composing functional mixins with classes is much more complex than with factory functions and the additional costs are often not worth the extra effort.

Classes automatically wire up a delegate prototype link. Factory functions do not.

The prototype link is a convenient way to conserve memory if you have millions of functions or to squeeze a micro-performance boost (if you need to access tens of thousands of properties on an object within a 16ms render loop cycle).

If you don’t need to micro-optimize memory or performance, the `[[Prototype]]` link can do more harm than good. The prototype chain powers the `instanceof` operator and the `instanceof` operator lies:
* The Constructor.prototype link is configurable in ES5 and can be overwritten
* The `instanceof` operator fails across different execution contexts (iframes)
* It uses a nominal type check, rather than structural

**The .constructor property**

The `.constructor` property can be very useful in JavaScript and it is a good idea to include it on object instances. It is unsafe to use it for type checking for the same reasons `instanceof` is unsafe for type checking.

JavaScript currently does not support the `.of()` specification which places an `.of()` method on factories and constructors, which would make constructors easier to work with. Today you have to manually provide support in your factory functions.

**Factories VS Classes**

Factories allow for increased flexibility:
* Decouple instantiation details from calling code
* Allow you to return arbitrary objects
* Don’t pretend to provide any guarantees
  * Discourages use of `instanceof` or other unreliable type-checking methods
* Extending an object instance with composition using factories is easier

Code That Requires new Violates The Open/Closed Principle. Our APIs should be open to extension, but closed to breaking changes.

Classes offer two kinds of performance optimizations:
* Shared memory for properties stored on the delegate prototype
* Property lookup optimizations

Factories can benefit from the delegate prototype as well by setting an object’s `__proto__` property or using `Object.create(proto)`.

**Classes Can Be Used Safely**
* Avoid `instanceof`
* Avoid `extends`
* Avoid exporting your class
  * Use `class` internally for performance gains
* Avoid `new`
  * Do not use it directly if you can help it
  * Never force your users to use it

**It’s ok to use classes if**
* You ask users to create classes but handle the instantiation for them
  * Think React and Angular
* You never inherit from your own classes or components
* You need to optimize performance

## JavaScript Monads
To begin learning monads, you need to know:
* Function composition `compose(f, g)(x) = (f ° g)(x) = f(g(x))`
* Functor basics: An understanding of the `Array.map()` function

Monads are simple. The lingo is difficult.

A monad is a way of composing functions that require context (ex: computation, branching, I/O) in addition to the return value. Monads type lift, flatten and map so that the types line up for lifting functions `a => M(b)`, making them composable. It’s a mapping from some type `a` to some type `b` along with some computational context hidden in the implementation details of lift, flatten, and map.

**Functions map**: `a => b`

**Functors map with context**: `Functor(a) => Functor(b)`

**Monads flatten and map with context**: `Monad(Monad(a)) => Monad(b)`

### Terms
**Map**

Apply a function to an `a` and return a `b`. Given some input, return some output.

**Context**

Computational detail of the monad’s composition. The Functor/Monad API and its workings supply the context which allows you to compose the monad with the rest of the application.

The point of functors and monads is the abstract the context away so we don’t have to worry about it while we’re composing things.


Mapping inside the context means you apply a function from `a => b` to the value inside the context, and return a new value `b` wrapped inside the same kind of context.

**Type Lift**

Lift a type into a context, wrapping the value inside of an API that you can use to compute from that value, trigger contextual computations, etc. `a => F(a)`

Monads are a kind of functor.

**Flatten**

Unwrap a value from its context `F(a) => a`

### Example
```javascript
const x = 20;                 // Some data of type `a`
const f = n => n * 2;         // A function from `a` to `b`
const arr = Array.of(x);      // The type lift.

// JS has type lift sugar for arrays: [x]
// .map() applies the function f to the value x
// in the context of the array.
const result = arr.map(f);    // [40]
```

`Array` is the context, `x` is the value we’re mapping over.

You can flatten arrays in JavaScript with `.concat()`:
```javascript
[].concat.apply([], [[1], [2, 3], [4]]); // [1, 2, 3, 4]
```

### What monads are made of
A monad is based on a simple symmetry  A way to wrap a value into a context, and a way to unwrap the value from the context:
* **Lift/Unit**: A lift from some type into the monad context `a => M(a)`
* **Flatten/Join**: Unwrapping the type from the context `M(a) => a`

And since monads are functors, they can also map:
* **Map**: Map with context preserved `M(a) -> M(b)`

Combine flatten with map and you get chain - function composition for monad-lifting functions. Also known as *Kleisli composition*, named for [Heinrich Kleisli](https://en.wikipedia.org/wiki/Heinrich_Kleisli).
* **Flatmap/Chain**: Flatten + Map `M(M(a)) => M(b)`

For monads, `.map()` methods are often omitted from the public API. You can make map if you can lift and chain:
```javascript
const MyMonad = value => ({
  // <... insert arbitrary chain and of here ...>
  map (f) {
    return this.chain(a => this.constructor.of(f(a)));
  }
});
```

The lift is the factory/constructor and/or `constructor.of()` method. In category theory, it's called “unit”. All it does is lift the type into the context of the monad. It turns an `a` into a Monad of `a`.

That flattening process (without the map in `.chain()`) is usually called `flatten()` or `join()`. Frequently (but not always), `flatten()`/`join()` is omitted completely because it's built into `.chain()`/`.flatMap()`. Flattening is often associated with composition, so it's frequently combined with mapping. Remember, unwrapping + map are both needed to compose `a => M(a)` functions.

### The Monad Laws
Three laws all monads should satisfy:
* **Left identity**: `unit(x).chain(f) ==== f(x)`
* **Right identity**: `m.chain(unit) ==== m`
* **Associativity**: `m.chain(f).chain(g) ==== m.chain(x => f(x).chain(g))`

**Identity Laws**

A monad is a functor. A functor is a morphism between categories, `A -> B`. The morphism is represented by an arrow.

In addition to the arrow we explicitly see between objects, each object in a category also has an arrow back to itself. In other words, for every object `X` in a category, there exists an arrow `X -> X`. That arrow is known as the identity arrow, and it's usually drawn as a little circular arrow pointing from an object and looping back to the same object.

![Identity Arrow Diagram](./images/identity-arrow.png)

**Associativity**

Associativity just means that it doesn’t matter where we put the parentheses when we compose. For example, if you’re adding, `a + (b + c)` is the same as `(a + b) + c`. The same holds true for function composition: `(f ° g) ° h = f ° (g ° h)`.

The same holds true for Kleisli composition. You just have to read it backwards. When you see the composition operator (`chain`), think `after`:
`h(x).chain(x => g(x).chain(f)) ==== (h(x).chain(g)).chain(f)`

**Proof for Identity Monad**
```javascript
{ // Identity monad
  const Id = value => ({
    // Functor mapping
    // Preserve the wrapping for .map() by 
    // passing the mapped value into the type
    // lift:
    map: f => Id.of(f(value)),

    // Monad chaining
    // Discard one level of wrapping
    // by omitting the .of() type lift:
    chain: f => f(value),

    // Just a convenient way to inspect
    // the values:
    toString: () => `Id(${ value })`
  });

  // The type lift for this monad is just
  // a reference to the factory.
  Id.of = Id;

  const g = n => Id(n + 1);
  const f = n => Id(n * 2);

  // Left identity
  // unit(x).chain(f) ==== f(x)
  trace('Id monad left identity')([
    Id(x).chain(f),
    f(x)
  ]);
  // Id monad left identity: Id(40), Id(40)

  // Right identity
  // m.chain(unit) ==== m
  trace('Id monad right identity')([
    Id(x).chain(Id.of),
    Id(x)
  ]);

  // Id monad right identity: Id(20), Id(20)
  // Associativity
  // m.chain(f).chain(g) ====
  // m.chain(x => f(x).chain(g)  
  trace('Id monad associativity')([
    Id(x).chain(g).chain(f),
    Id(x).chain(x => g(x).chain(f))
  ]);
  // Id monad associativity: Id(42), Id(42)
}
```

> “Once you understand monads, you immediately become incapable of explaining them to anyone else”

Lady Monadgreen’s curse ~ Gilad Bracha (used famously by Douglas Crockford)

## Resources
* [https://medium.com/javascript-scene/composing-software-an-introduction-27b72500d6ea](https://medium.com/javascript-scene/composing-software-an-introduction-27b72500d6ea)
* [https://medium.com/javascript-scene/the-rise-and-fall-and-rise-of-functional-programming-composable-software-c2d91b424c8c](https://medium.com/javascript-scene/the-rise-and-fall-and-rise-of-functional-programming-composable-software-c2d91b424c8c)
* [https://medium.com/javascript-scene/why-learn-functional-programming-in-javascript-composing-software-ea13afc7a257](https://medium.com/javascript-scene/why-learn-functional-programming-in-javascript-composing-software-ea13afc7a257)
* [https://medium.com/javascript-scene/a-functional-programmers-introduction-to-javascript-composing-software-d670d14ede30](https://medium.com/javascript-scene/a-functional-programmers-introduction-to-javascript-composing-software-d670d14ede30)
* [https://hackernoon.com/js-var-let-or-const-67e51dbb716f](https://hackernoon.com/js-var-let-or-const-67e51dbb716f)
* [https://medium.com/javascript-scene/higher-order-functions-composing-software-5365cf2cbe99](https://medium.com/javascript-scene/higher-order-functions-composing-software-5365cf2cbe99)
* [https://medium.com/javascript-scene/reduce-composing-software-fe22f0c39a1d](https://medium.com/javascript-scene/reduce-composing-software-fe22f0c39a1d)
* [https://medium.com/javascript-scene/functors-categories-61e031bac53f](https://medium.com/javascript-scene/functors-categories-61e031bac53f)
* [https://medium.com/javascript-scene/functional-mixins-composing-software-ffb66d5e731c]
* [https://medium.com/javascript-scene/javascript-factory-functions-with-es6-4d224591a8b1]
* [https://medium.com/javascript-scene/why-composition-is-harder-with-classes-c3e627dcd0aa]
* [https://medium.com/javascript-scene/javascript-monads-made-simple-7856be57bfe8]
