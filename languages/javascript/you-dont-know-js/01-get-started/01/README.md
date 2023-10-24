# Chapter 1: What is JavaScript?

## Sections
* [What's With That Name?](#whats-with-that-name)
* [Language Specification](#language-specification)
  * [The Web Rules Everything (JS)](#the-web-rules-everything-js)
  * [Not All (Web) JS](#not-all-web-js)
  * [It's Not Always JS](#its-not-always-js)
* [Many Faces](#many-faces)
* [Backwards and Forwards](#backwards-and-forwards)

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

[▲ Return to Sections](#sections)

| [Table of Contents](../README.md#table-of-contents) |
