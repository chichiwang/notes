# Chapter 1: What is JavaScript?

## Sections
* [What's With That Name?](#whats-with-that-name)
* [Language Specification](#language-specification)
  * [The Web Rules Everything (JS)](#the-web-rules-everything-js)
  * [Not All (Web) JS](#not-all-web-js)
  * [It's Not Always JS](#its-not-always-js)
* [Many Faces](#many-faces)
* [Backwards and Forwards](#backwards-and-forwards)
  * [Jumping the Gaps](#jumping-the-gaps)
  * [Filling the Gaps](#filling-the-gaps)
* [What's in an Interpretation?](#whats-in-an-interpretation)
  * [Web Assembly (WASM)](#web-assembly-wasm)
* [Strictly Speaking](#strictly-speaking)
* [Defined](#defined)

[◂ Return to Table of Contents](../README.md)

## What's With That Name?
When Brendan Eich first conceived of the language he code-named it Mocha. The name LiveScript was used internally at Netscape. The name JavaScript was used publicly because it was originally designed to appeal to mostly Java programmers and the word "script" was popular at that time to refer to lightweight programs. These scripts would be the first to embed inside the pages of the web. The name "JavaScript" was a marketing ploy to position the language as an alternative to Java on the web.

There are some superficial similarities between JavaScript and Java that come from both languages targeting developers with assumed syntax expectations from C and C++:
* `{` is used to denote a block of code and `}` denotes the end of a block
* `;` is used to punctuate the end of a statement

The company Oracle (via Sun) owns the official trademark for the name "JavaScript" via Netscape. This trademark is almost never enforced (and likely couldn't be at this point). The official name of the language specified by TC39 and formalized by the ECMA standards body is **ECMAScript**. Since 2016 the official language name has also been suffixed by the revision year, for example: **ECMAScript 2019** (abbreviated as **ES2019**).

The JavaScript that runs inside of Node or the browser is an implementation of ECMAScript.

> "Java is to JavaScript as ham is to hamster" -- Jeremy Keith, 2009

[▲ Return to Sections](#sections)

## Language Specification
The primary task of TC39, the technical steering committee that manages JS, is managing the official specification of the language. They meet to vote on agreed changes and then submit those changes to ECMA, the standards organization. JS syntax and behavior are defined in the ES specification.

ES2019 is the 10th major numbered specification/revision since JS's inception in 1999 so the official URL contains "10.0": [https://www.ecma-international.org/ecma-262/10.0/](https://www.ecma-international.org/ecma-262/10.0/).

The TC39 committee is comprised of 50-100 people from a broad section of web-invested companies such as browser makers (Mozilla, Google, Apple) and device makers (Samsung, etc). All of the members of the committee are volunteers although they may receive compensation from the companies they are employed by for their duties on the committee.

All TC39 proposals process through a [five-stage process](https://tc39.es/process-document/): Stage 0 through Stage 4. A Stage 0 proposal means that a member of TC39 plans to champion and work on it. Developers can propose changes/additions through informal channels (social media, blog posts) but must get a TC39 member to champion the idea for it to reach Stage 0. At Stage 4 a proposal is eligible to be included in the next yearly revision of the language. It can take anywhere from several months to several years for a proposal to make its way through all of the stages. All proposals are publicly managed in TC39's Github repository: [https://github.com/tc39/proposals](https://github.com/tc39/proposals).

Anyone can participate in the public discussions and processes for working on the proposals but only TC39 members can attend meetings and vote on the proposals and changes.

All major browser and device makers have committed to keeping their JS implementations compliant with this central specification maintained by TC39 and ECMA. Different engines may implement new features at different times but they should never implement a specified feature differently or incompatibly from the standard.

#### The Web Rules Everything (JS)
While the array of environments running JS is constantly expanding (browsers, servers, robots, refridgerators, etc) the one environment that rules JS is the web: the only implementation of JS that really matters is in web browsers.

For the most part the JS specification and implmentation in browser-based engines is the same. There are some differences that must be considered:
* JS engines refuse to conform to specification-dictated changes that would break pre-existing web content
  * In these cases TC39 may backtrack and conform the specification to the reality of the web
    * Example: TC39 planned to add a `contains(..)` method for Arrays but this name conflicted with old JS frameworks still in use on some sites so they changed it to a non-conflicting `includes(...)`
    * Example: A planned `flatten(..)` method was eventually renamed `flat(..)`
  * Occasionally TC39 will decide the specification should remain even if web-based engines are unlikely to conform to it
    * [Appendix B: "Additional ECMAScript Features for Web Browsers"](https://262.ecma-international.org/10.0/#sec-additional-ecmascript-features-for-web-browsers) is included in the specification to detail any known mismatches between the official JS specification and the reality of JS on the web. In other words these exceptions are allowed _only_ for web-based JS; other JS environments must conform to the standard.
    * Section B.1 and B.2 cover _additions_ to JS (syntax and APIs) that web JS includes for historical reasons, but that TC39 does not plan to formally specify to the core of JS.
      * Examples: `0`-prefixed octal literals, the global `escape(..)`/`unescape(..)` utilities, String helpers like `anchor(..)` and `blink()`, and the RegExp `compile(..)` method
    * Section B.3 covers some conflicts where code may run with different behaviors in web and non-web JS engines. Most of the listed changes involve situations that are labeled as early errors when the code is running in strict mode.

Wherever possible adhere to the JS specifications and don't rely on behaviors that are only applicable in certain JS environments.

#### Not All (Web) JS
Various JS environments (browser, Node.js) add APIs to the global scope that provide environment-specific capabilities:

```javascript
alert("Hello, JS!");
```

The above code may or may not be a JS program depending on perspectve. The `alert(..)` function is not included in the JS specification but it _is_ implemented in all web JS environments. However it is not listed in Appendix B. A wide range of APIs (`fetch(..)`, `getCurrentLocation(..)`, `getUserMedia(..)`, etc) are all web APIs that look like JS. Node.js contains hundreds of API methods from built-in modules like `fs.write(..)`.

Another example is `console.log(..)` and all other `console.*` methods. These are not specified in JS but because of their universal utility are defined by pretty much every JS environment according to a roughly agreed-upon consensus.

These environment-defined APIs are functions and object methods that obey JS syntax rules. While they are JS they are not part of the language specification. These APIs are a common source of complaint of JavaScript's inconsistency, but they are not part of the actual language.

#### It's Not Always JS
The console/REPL (Read-Eval-Print-Loop) in the browser's Developer Tools or Node feels like a pretty straightforward JS environment at first glance but it isn't. Developer Tools prioritize DX (Developer Experience) and it is not their goal to purely reflect the nuances of strict-spec JS behavior. These tools vary in behavior from browser to browser and change quite frequently. Quirks and inconsistencies with certain behaviors may be experienced around:
* Whether a `var` or `function` declaration in the top-level global scope of the console actually creates a global variable (mirrored in the `window` property and vice-versa)
* What happens with multiple `let` and `const` declarations in the top-level global scope
* Whether `"use strict";` on one-line entry enables strict mode for the duration of the console session the same way it would on the first line of a .js file
* Whether `"use strict";` can be used beyond the "first line" and still enable strict mode for the session
* How non-strict mode `this` default-binding works for function calls
* Whether the "global object" used will contain expected global variables
* How hoisting works across multiple line entries
* ...and more.

The developer console is not mimicking a JS compiler that will handle code exactly as the JS engine would handle a .js file - it is trying to allow a developer to quickly enter a few lines of code and see the results immediately. Do not trust the behavior of the developer console as representing exact to-the-letter JS semantics (for that read the specification).

[▲ Return to Sections](#sections)

## Many Faces
The term "paradigm" in the context of programming languages refers to a broad mindset and approach in structuring code. Typical paradigm-level code categories include:
* Procedural: organizes code in a top-down, linear progression through a pre-determined set of operations, usually collected together in related units called procedures.
* Object-Oriented: organizes code by collecting logic and data together into units called classes.
* Functional Programming: organizes code into functions (pure computations as opposed to procedures), and the adaptations of those functions as values.

Some languages are heavily slanted toward one paradigm or another: C is procedural, Java/C++ are almost entire class oriented, and Haskell is Functional.

Many languages support code patterns that can come from different paradigms. These "multi-paradigm" languages can even allow two or more expressions of different paradigms to sit side-by-side. JavaScript is one of these multi-paradigm languages that allows for proceduarl, object-oriented, or functional style code.

[▲ Return to Sections](#sections)

## Backwards and Forwards
One of the most foundational principles that guides JavaScript is the preservation of _backwards compatibility_. Many confuse this concept with a related, but different, term: _forwards compatibility_.

_Backwards compatibility_ means that once something is accepted as valid JS there will never be a change to the language that causes that code to become invalid. Code written in 1995 should still work today. The idea is that JS developers can write code with the confidence that their code will not stop working because a browser update is released.

The cost of sticking to this principle creates a very high bar to changing or extending the language: any decision becomes effectively permanent, mistakes included. There are some small exceptions to this rule: when some backwards-incompatible changes have been introduced TC39 studied existing code on the web (via browser data gathering) to estimate the impact of these changes, and browsers voted on whether they were willing to break the behavior of code for the benefits of fixing or improving some aspect of the language. These kinds of changes are rare and almost always in corner cases of usage that are unlikely to result in observably breaking many sites.

_Forwards compatibility_, on the other hand, means any new additions of the language will continue to work on an older JS engine. **JS is not fowards-compatible**.

HTML and CSS, by contrast, are forwards-compatible but not backwards-compatible. If a new feature is loaded onto an old browser, the page is not broken. The unrecognized HTML/CSS is skipped over while the rest of the HTML/CSS is processed. However, older features may not work (or work the same) in newer browsers.

#### Jumping the Gaps
Since JS is not forwards-compatible there could be a gap between JS being written and the oldest engine the site or application needs to support.

If a newer syntax is used in an older engine the program will likely fail to run at all throwing a syntax error. If a newer API is utilized in an older engine the program will likely run up to a point but may throw a runtime exception when the unknown API is encountered. JS developers need to take special care to address this gap.

To address incompatible syntax the solution is the use of a transpiler. Transpiling is a contrived and community-invented term to describe converting source code from a program from one form to another form of source code. Usually forwards-compatibility issues are solved by using a transpiler (most commonly [Babel](https://babeljs.io/)) to convert newer JS syntax to an equivalent older syntax.

For example a snippet of code like the following:

```javascript
if (something) {
  let x = 3;
  console.log(x);
} else {
  let x = 4;
  console.log(x);
}
```

Babel might transpile to something like:

```javascript
var x$0, x$1;
if (something) {
  x$0 = 3;
  console.log(x$0);
} else {
  x$1 = 4;
  console.log($x$1);
}
```

The first snippet relied on `let` to create block-scoped `x` variables in both the `if` and `else` clauses. An equivalent solution that doesn't use the `let` declaration just chooses two different variable names in the function or global scope.

The `let` declaration was introduced in ES6 (in 2015). This example only applies if the application needs to support any engines released prior to ES6. The "target" oldest version engine for transpilation is a sliding window that shifts upwards as decisions are made for a site/application to stop supporting some older browsers/engines.

Why bother converting newer syntax into older syntax at all? Why not just create applications in the older syntax? It is strongly recommneded for developers to use the latest version of JS so that their code is clean, communicates ideas effectively, and make future maintenance easier as time goes on.

#### Filling the Gaps
If the forwards-compatibility issue is an API not present in older engines then the solution is to provide a definition for that missing API method that provides the same API and behavior. This pattern is called a polyfill or a shim.

Using the following snippet as example:

```javascript
// getSomeRecords() returns us a promise for some
// data it will fetch
var pr = getSomeRecords();

// show the UI spinner while we get the data
startSpinner();

pr
.then(renderRecords)   // render if successful
.catch(showError)      // show an error if not
.finally(hideSpinner)  // always hide the spinner
```

The above code uses the ES2019 feature `finally(..)` method on the promise prototype. If this code is run in a pre-ES2019 engine an error would occur.

A polyfill for `finally(..)` would look something like this:

```javascript
if (!Promise.prototype.finally) {
  Promise.prototype.finally = function f(fn){
    return this.then(
      function t(v){
        return Promise.resolve( fn() )
          .then(function t(){
            return v;
          });
      },
      function c(e){
        return Promise.resolve( fn() )
          .then(function t(){
            throw e;
          });
      }
    );
  };
}
```

**Note**: This is a simple illustration of a basic, non-spec-compliant implementation of a `finally(..)` polyfill. Do not use this polyfill. Instead always use a robust, official polyfill wherever possible.

The polyfill will only define the method if the environment it is running in has not already defined it. Transpilers like Babel typically detect which polyfills a codebase needs and automatically provides them. Occasionally a polyfill may need to be included explicitly.

Always write code using the most appropriate features to communicate ideas and intent effectively. In general this means using the most recent, stable version of JS. Avoid negatively impacting the readability of code by manually adjusting for syntax/API gaps - this is what tools are for.

[▲ Return to Sections](#sections)

## What's in an Interpretation?
Whether JavaScript is an interpreted script or a compiled program is a long-debated topic. The majority opinion seems to be that it is an interpreted (scripting) language. The truth is more complicated.

For much of the history of programming languages "interpreted" and "scripting" languages have been looked down on as inferior to their compiled counterparts. The reasons are numerous including the perception that there is a lack of performance optimization, and a dislike of certain language characteristics such as scripting languages generally using dynamic typing instead of the "more mature" statically typed languages.

Languages regarded as "compiled" usually produced a portable, binary executable. The distribution model for a program's "executable" form has become drastically more varied and less relevant over the last few decades: it doesn't matter so much anymore what form of a program is distributed.

The real reason it matters whether JS is interpreted or compiled relates to error handling: historically, scripted or interpreted languages were executed in a top-down, line-by-line fashion. There typically was not an initial pass through the program to process it before execution.

In scripted/interpreted languages an error on line 5 won't throw an exception until lines 1-4 have already executed. The error on line 5 may be a runtime error or it may be a malformed statement/command on that line. Depending on what the error is, deferring error handling until execution may be a desirable or undesirable effect.

This model differs from languages that go through a processing step (called parsing) before any execution occurs. In the processing model an invalid command (such as broken syntax) on line 5 would be caught during the parsing phase before any execution has begun. For syntax (or otherwise static) errors it is generally preferred to know about them before any doomed, partial execution.

All compiled languages are parsed. In classic compilation theory the last step after parsing code is code generation: producing an executable form.

Once any source code has been fully parsed it's common that the subsequent execution will include a translation from the parsed form of the program (usually called an Abstract Syntax Tree, _AST_) to the executable form. Parsed languages usually also perform code generation before execution so in spirit they're compiled languages.

JS source code is parsed prior to execution. The specification requires this - it calls for "early errors" (statically determined errors in code such as a duplicate parameter name) to be reported before code execution begins. These errors cannot be recognized without the code having been parsed.

**JS is a parsed language** but is it compiled? The answer is closer to yes than no. The parsed JS is converted to an optimized (binary) form which is subsequently executed. The engine does not switch back to line-by-line execution after parsing - that would be highly inefficient. To be specific this "compilation" produces a binary byte code of sorts which is then handed to the "JS virutal machine" to execute. Some like to say that the VM is "interpreting" the byte code, but that means Java, and a dozen other JVM-driven languages, are interpreted rather than compiled. That contradicts the typical assertion that Java/JVM-languages are compiled languages.

Additionally the JS engine can employ multiple passes of JIT (just-in-time) processing/optimization on the generated code post-parsing. This could reasonably be labeled either "compilation" or "interpretation" depending on perspective.

Consider the flow of a JS source program:
1. After the source code is done being worked on it undergoes a number of build processes (transpilation by Babel, bundling by Webpack, etc) and gets delivered in a very different form to a JS engine.
2. The JS engine parses the code to an AST.
3. The engine converts the AST to a kind-of byte code, a binary intermediate representation (IR), which is then refined/converted even further by the optimizing JIT compiler.
4. The JS VM executes the program.

If not in practice, then in spirit **JS is a compiled language**. The reason this matters is that as a compiled language JS throws static errors (syntax, etc) before any code is executed.

#### Web Assembly (WASM)
One dominating concern that has driven a significant amount of JavaScript's evolution is performance: both the speed of parsing/compilation and the speed of execution.

In 2013, engineers from Mozilla Firefox demonstrated a port of the Unreal 3 game engine from C to JS (also see: [QuakeJS](http://www.quakejs.com/)). This code is able to in a browser JS engine at full 60fps performance due to utilizing a subset of the JS language named ASM.js. This subset is valid JS, written in ways uncommon in normal coding, signal important typing information to the engine that allow it to make key optimizations. ASM.js was introduced as a way of addressing the pressures on the runtime performance of JS. ASM.js was never intended to code authored by developers, but rather a representation of a program transpiled from another language (such as C), where these typing "annotations" were inserted automatically by the tooling.

Several years later another group of engineers (also, initially, from Mozilla) released Web Assembly (WASM). WASM is similar to ASM.js in that its original intent was to provide a way for non-JS programs (C, etc.) to be converted to run in the JS engine. Unlike ASM.js, WASM additionally got around some of the inherent delays in JS parsing/compilation before a program can execute. WASM is a representation format more akin to Assembly that can be processed by the JS engine by skipping the parsing/compilation that the JS engine normally does. The parsing/compilation of a WASM program happens ahead-of-time (AOT): the distributable is a binary-packed program ready for the JS engine to execute with minimal processing.

Another goal of WASM is to bring parity for non-JS languages to the web platform. For example: if a language like Go supports threaded programming, WASM offers the potential for a Go program to be converted to JS without needing threading features in the JS language itself. WASM relieves the pressure to add features to JS that are intended to be used by transpiled programs from other languages. This allows JS feature development to be judged by TC39 without being skewed by interests or demands in other language ecosystems while allowing those languages a viable path to the web.

WASM is also evolving to become a cross-platform virtual machine (VM) of sorts, where a program can be compiled once and run in a variety of different system environments. WASM isn't only for the web and it also isn't JS. Even though WASM runs in the JS engine JavaScript is one of the least suitable languages to source WASM programs with because WASM relies havily on static typing information. Even TypeScript (TS) is not quite suitable to transpile to WASM although language variants like AssemblyScript are attempting to bridge the gap between JS/TS and WASM.

Some have suggested WASM points to a future where JS is no longer the dominant language of the web. This largely comes from a place of disliking JavaScript and wanting it replaced. WASM will not replace JS. WASM augments what the web can accomplish and that is orthogonal to whether some will use it as a way to avoid writing JS.

[▲ Return to Sections](#sections)

## Strictly Speaking
In 2009, with the release of ES5, JS added _strict mode_ as an opt-in mechanism. The goal of strict mode is to guide better programming practices in a direction where the JS engine has the best chance of optimizing and efficiently running the code.

Most strict mode controls are in the form of _early errors_: errors that aren't strictly syntax errors but are still thrown at compile-time, before the code is run. For example: strict mode disallows naming two function parameters the same, resulting in an early error. Some strict mode errors occur at runtime (such as how `this` defaults to `undefined` rather than the global object).

Strict mode is switched on per file with a special pragma, with nothing allowed before it except comments and whitespace:
```javascript
// only whitespace and comments are allowed
// before the use-strict pragma
"use strict";
// the rest of the file runs in strict mode
```

Strict mode can be turned on per-function scope:
```javascript
function someOperations() {
  // whitespace and comments are fine here
  "use strict";

  // all this code will run in strict mode
}
```

If a file has strict mode turned on, the function-level strict mode pragmas are not allowed. The only valid reason to use per-function strict mode is when converting a non-strict mode file over to strict mode a little at a time.

Virtually all transpiled code ends up in strict mode. ES6 modules also assume strict mode, so all code in modules are automatically defaulted to strict mode. While strict mode is opt-in, it is largely de facto default.

[▲ Return to Sections](#sections)

## Defined
JavaScript is an implementation of the ECMAScript standard which is guided by the TC39 committee and hosted by ECMA. It runs in browsers and other JS environments like Node.js.

JS is a multi-paradigm language with syntax and capabilities that allow developers to mix and match concepts from various paradigms: procedural, object-oriented, functional.

JS is a compiled language. The tools (including the JS engine) process and verify a program before it executes.

[▲ Return to Sections](#sections)

| [Table of Contents](../README.md#table-of-contents) | [Next: Chapter 2 - Surveying JS](../02/README.md) |
