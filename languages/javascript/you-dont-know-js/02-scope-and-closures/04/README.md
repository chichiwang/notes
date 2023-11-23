# Chapter 4: Around The Global Scope
The global scope of a JavaScript program is a rich topic, with more utility and nuance than one might assume. This chapter explores how the global scope is (still) useful and relevant to writing JavaScript programs today. It looks at the differences in _where_ and _how_ to access the global scope in different JavaScript environments.

Fully understanding the global scope is critical to mastery of using lexical scope to structure programs.

## Sections
* [Global Scope](#global-scope)
* [Where Exactly is this Global Scope?](#where-exactly-is-this-global-scope)
  * [Browser "Window"](#browser-window)
  * [Globals Shadowing Globals](#globals-shadowing-globals)
  * [DOM Globals](#dom-globals)
  * [What's in a (Window) Name?](#whats-in-a-window-name)
  * [Web Workers](#web-workers)
  * [Developer Tools Console/REPL](#developer-tools-consolerepl)
  * [ES Modules (ESM)](#es-modules-esm)
  * [Node](#node)
* [Global This](#global-this)

[◂ Return to Table of Contents](../README.md)

## Global Scope
JavaScript programs are often comprised of multiple individual `.js` files. The JavaScript engine stitches these separate files together into a single runtime context in three main ways.

1. **ES Modules**: In the case of direct ES modules (not transpiled into some other module-bundle format) the JavaScript environment loads the files individually. These separate module files then interact with each other using the `import` keyword without any need for a shared outer scope.

2. **Bundlers**: When bundlers are used in a build process, a program comprised of many JavaScript files are often concatenated into a single file to be loaded into the runtime environment. Even when bundled into a single file, there needs to be a way for separate files to refer to and access each other. In some setups the entire output file is wrapped in a single enclosing scope (wrapper function, UMD, etc). Each source file is registered under a local variable in the shared scope:

```javascript
(function wrappingOuterScope(){
  var moduleOne = (function one(){
    // ..
  })();

  var moduleTwo = (function two(){
    // ..

    function callModuleOne() {
      moduleOne.someMethod();
    }

    // ..
  })();
})();
```

In the contrived example above, a wrapping scope for each file is registered to a local variable (`moduleOne`, `moduleTwo`) within the shared scope `wrappingOuterScope()`. `wrappingOuterScope()` is a function and not the environment's true global scope, however for the entire application it acts as such: it is where top-level identifiers for the application can be stored.

3. **Environment Global Scope**: Whether a bundler or ES Modules are used, if the files ar loaded individually (via either `<script>` tags or other resource-loading mechanism), if there is no single encompassing scope wrapping all of the pieces of the application, the **global scope** is then the only way for the pieces to interact with each other.

A bundled file of this nature may look something like this:

```javascript
var moduleOne = (function one(){
  // ..
})();

var moduleTwo = (function two(){
  // ..

  function callModuleOne() {
    moduleOne.someMethod();
  }

  // ..
})();
```

Without a wrapper scope the declarations for `moduleOne` and `moduleTwo` occur on the global scope. This is effectively the same as if they were declared in two separate files, without concatenation.

`module1.js`:

```javascript
var moduleOne = (function one(){
  // ..
})();
```

`module2.js`:

```javascript
var moduleTwo = (function two(){
  // ..

  function callModuleOne() {
    moduleOne.someMethod();
  }

  // ..
})();
```

If these files are loaded separately as normal standalone `.js` files in a browser environment, each top-level variable declaration ends up a global variable. The separate files are separate programs from the perspective of the JavaScript engine.

The JavaScript global scope is where:
* JavaScript exposes its built-ins:
  * Primitives: `undefined`, `null`, `Infinity`, `NaN`
  * Natives: `Date()`, `Object()`, `String()`, etc.
  * Global functions: `eval()`, `parseInt()`, etc.
  * Namespaces: `Math`, `Atomics`, `JSON`
  * Friends of JavaScript: `Intl`, `WebAssembly`
* The environment hosting the JavaScript engine exposes its own built-ins:
  * `console` (and its methods)
  * The DOM (`window`, `document`, etc)
  * Timers (`setTimeout(..)`, etc)
  * Web platform APIs: `navigator`, `history`, geolocation, WebRTC, etc.

**NOTE**: Node.js also exposes several elementals "globally", but they're technically not in the `global` scope: `require()`, `__dirname`, `module`, `URL`, etc.

Most developers agree the global scope should not be a dumping ground for every variable in an application: that's a lot of bugs waiting to happen (name conflicts, etc). The global scope is an important _glue_ for practically every JavaScript application.

[▲ Return to Sections](#sections)

## Where Exactly is this Global Scope?
Different JavaScript environments handle the scope of programs, especially the global scope, differently. It is common for JavaScript developers to harbor misconceptions without realizing it.

#### Browser "Window"
A standalone `.js` file loaded in a browser is the most _pure_ environment JavaScript code can be run in with respect to the treatment of the global scope. "Pure" does not mean that nothing is automatically added (a lot may be added), it means there is minimal instrusion of the code or interference with its expected global scope behavior.

Using the following code as example:

```javascript
var studentName = "Kyle";

function hello() {
  console.log(`Hello, ${ studentName }!`);
}

hello();
// Hello, Kyle!
```

Whether the code is loaded into the browser via an inline `<script>` tag, a `<script source=..>` tag, or a dynamically injected `<script>` tag: the `studentName` and `hello` identifiers are declared in the global scope.

This means that the global object (commonly `window` in the browser) will contain properties with the same names:

```javascript
var studentName = "Kyle";

function hello() {
  console.log(`Hello, ${ window.studentName }!`);
}

window.hello();
// Hello, Kyle!
```

This is the default behavior expected based on a reading of the JavaScript specification: the outer scope _is_ the global scope and `studentName` is legitimately created as a global variable.

This behavior is not always true for every JavaScript environment - that is often surprising to JavaScript developers.

#### Globals Shadowing Globals
Within the global scope a global object property can be shadowed by a global variable:

```javascript
window.something = 42;

let something = "Kyle";

console.log(something);
// Kyle

console.log(window.something);
// 42
```

The `let` declaration assigns a global variable `something` that is not mirrored by a global object property. This creates a lexical identifier `something` that shadows the `something` global object property.

It is a bad idea to create a divergence between the global object and the global scope: this will invariably lead to confusion and unexpected behaviors. One way to avoid this is to always use `var` for globals and reserve `let` and `const` for block scopes.

#### DOM Globals
Although the browser-hosted JavaScript environment has the most _pure_ global scope behavior, it is not entirely _pure_.

One surprising behavior of the global scope in browser-based JavaScript applications is that a DOM element created with an `id` attribute automatically has a global variable created to reference it:

```html
<ul id="my-todo-list">
  <li id="first">Write a book</li>
  ..
</ul>
```

These elements with `id`s can be referenced from JavaScript:

```javascript
first;
// <li id="first">..</li>

window["my-todo-list"];
// <ul id="my-todo-list">..</ul>
```

If the `id` attribute is a valid lexical name like `first` then a lexical variable is created. Otherwise it can only be accessed through the global object (`window[..]`).

This auto-registration of `id`-bearing DOM elements as global variables is an old legacy browser behavior. It is advised to never use these global variables, even though they will always be silently created.

#### What's in a (Window) Name?
Another browser-based oddity in browser-based JavaScript:

```javascript
var name = 42;

console.log(name, typeof name);
// "42" string
```

`window.name` is a pre-defined "global" property in a browser context. It seems like a normal global variable but it isn't.

The `var` declaration in the snippet above **does not shadow** the pre-defined `name` global property - the `var` declaration is effectively ignored. However, retrieving the value of `name` after assigning the number `42` to it reveals it to be the string `"42"`. This strange behavior is because `name` is actually pre-defined as a setter/getter on the `window` object that enforces that the value is a string.

With a few exceptions like DOM element IDs and `window.name`, a JavaScript file running standalone in a browser environment is about as _pure_ as global scope behavior gets.

#### Web Workers
Web Workers are a web platform extension which allows a JavaScript file to run in a completely separate OS thread than the one that's running the main JavaScript program.

Since Web Worker programs run on a separate thread they're restricted in their communications with the main application thread to avoid/limit race conditions and other complications. The DOM and the global `window` object are inaccessible to a Web Worker, but some web APIs (such as `navigator`) are accessible.

A Web Worker is treated as a wholly separate program so it does not share the global scope with the main application. In a Web Worker the global object reference is typically made using `self`:

```javascript
var studentName = "Kyle";
let studentID = 42;

function hello() {
  console.log(`Hello, ${ self.studentName }!`);
}

self.hello();
// Hello, Kyle!

self.studentID;
// undefined
```

Just as with the main JavaScript program: `var` and `function` declared variables at the top-most scope create mirrored properties on the global object (`self`) where block-scoped declarations (`let`, `const`) do not.

#### Developer Tools Console/REPL
Developer tools process JavaScript code but are not a completely specification-adherent JavaScript environment - they will lean in favor of being developer friendly (developer experience, DX). There are observable differences in code behavior between running a snippet in a developer environment versus processing a full JavaScript program (ex: error conditions may be relaxed and not displayed in a developer tool).

In regards to scope, observable differences may include:
* The behavior of the global scope.
* Hoising
* Block-scoping declarators (`let`/`const`) when used in the outer-most scope.

Statements entered into the outer-most scope in a console/REPL may seem to be processed in the actual global scope but this is not entirely true. These tools typically emulate the global scope position to an extent, without strict adherence. Given that these tools prioritize developer experience, at times observed behavior may deviate from the JavaScript specification.

Developer tools are not suitable environments to determine/verify explicit and nuanced behaviors of an actual JavaScript program context.

[▲ Return to Sections](#sections)

#### ES Modules (ESM)
One of the more obvious impacts of the ESM pattern is how it changes the behavior of the top-level scope in a file.

Using this code as example:

```javascript
var studentName = "Kyle";

function hello() {
  console.log(`Hello, ${ studentName }!`);
}

hello();
// Hello, Kyle!

export hello;
```

If the above code loaded as an ES Module it will run the same but the observable effects from the application perspective will be different. Despite being declared in the file's outer-most scope, `studentName` and `hello` are not global variables. They belong to the module's top-most scope bucket, or "module-global".

In a module there's no implicit "module-wide scope object" for top-level declarations to be added to as properties. Within ES Modules global variables are not created by declaring variables in the top-level scope of a module. A module's scope is descended from the global scope (as if the entire contents of a module are wrapped in a function) so all variables that exist in the global scope are available as lexical identifiers within a module.

ESM encourages minimization of reliance on a global scope, instead encouraging explicit importing whatever is needed from other modules.

#### Node
One aspect of Node that often catches JavaScript developers off guard is that every `.js` file it loads (including the one used to start the Node process with) is treated as a module (ES Module or CommonJS module). The top level of any Node program is never actually the global scope.

Node has added support for ES Modules, but has supported a module format called "CommonJS" since its inception. CommonJS looks like the following snippet:

```javascript
var studentName = "Kyle";

function hello() {
  console.log(`Hello, ${ studentName }!`);
}

hello();
// Hello, Kyle!

module.exports.hello = hello;
```

Before processing, Node effectively wraps the file contents in a function to give it its own isolated scope.

The code above can be visualized as being seen by Node like the following (illustrative, not actual):

```javascript
function Module(module,require,__dirname,...) {
  var studentName = "Kyle";

  function hello() {
    console.log(`Hello, ${ studentName }!`);
  }

  hello();
  // Hello, Kyle!

  module.exports.hello = hello;
}
```

Node defines a number of "globals" like `require()` but they're not actually identifiers in the global scope (nor properties on the global object). They are injected into the top-level scope of each module (a bit like parameters in the `Module(..)` function in the above example).

The only way to create global variables in a Node program is to define properties on an automatically provided global object called `global` - a reference to the real global scope object (similar to `window` in a browser).

[▲ Return to Sections](#sections)

## Global This
Reviewing the JavaScript environments covered so far, a program may or may not:
* Declare a global variable in the top-level scope with `var` or `function` declarations - or `let`, `const`, or `class`.
* Automatically adds global variables declarations (using `var` and `function`) as properties of the global scope object
* Expose the global scope object as `window`, `self`, or `global`.

Another trick for obtaining a reference to the global scope object is the following:

```javascript
const theGlobalScopeObject =
  (new Function("return this"))();
```

**NOTE**: A function dynamically constructed from code stored in a string with the `Function()` constructor will automatically be run in non-strict-mode (for legacy reasons), when invoked normally its `this` will point at the global object.

As of ES2020 JavaScript defined a standardized reference to the global object called `globalThis`. `globalThis` can be used in place of all other approaches if the runtime enviroment is sure to be recent enough to support it.

A polyfill, like the one below, could be used to ensure a reference to the global object:

```javascript
const theGlobalScopeObject =
  (typeof globalThis != "undefined") ? globalThis :
  (typeof global != "undefined") ? global :
  (typeof window != "undefined") ? window :
  (typeof self != "undefined") ? self :
  (new Function("return this"))();
```

It is advised, in order to reduce confusion, that if a reference to the global object is required, to rename `globalThis` to something with clearer semantic meaning (such as `theGlobalScopeObject` as in the example above). `globalThis` could easily be misunderstood to be a reference to some sort of global/default `this` binding.

[▲ Return to Sections](#sections)

| [Previous: Chapter 3 - The Scope Chain](../03/README.md) | [Table of Contents](../README.md#table-of-contents) |
