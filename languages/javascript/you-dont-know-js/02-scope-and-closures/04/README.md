# Chapter 4: Around The Global Scope
The global scope of a JavaScript program is a rich topic, with more utility and nuance than one might assume. This chapter explores how the global scope is (still) useful and relevant to writing JavaScript programs today. It looks at the differences in _where_ and _how_ to access the global scope in different JavaScript environments.

Fully understanding the global scope is critical to mastery of using lexical scope to structure programs.

## Sections
* [Global Scope](#global-scope)

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

| [Previous: Chapter 3 - The Scope Chain](../03/README.md) | [Table of Contents](../README.md#table-of-contents) |
