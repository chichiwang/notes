# Chapter 1: What is JavaScript?

## Sections
* [What's With That Name?](#whats-with-that-name)
* [Language Specification](#language-specification)
* [The Web Rules Everything (JS)](#the-web-rules-everything-js)

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

[▲ Return to Sections](#sections)

## The Web Rules Everything (JS)
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

[▲ Return to Sections](#sections)

| [Table of Contents](../README.md#table-of-contents) |
