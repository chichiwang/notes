# Chapter 1: What is JavaScript?

## Sections
* [What's With That Name?](#whats-with-that-name)
* [Language Specification](#language-specification)

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

| [Table of Contents](../README.md#table-of-contents) |
