# Chapter 1: What's the Scope?
Creating and working with variables is one of the most foundational actions programmers perform. A set of well-defined rules called _scope_ determine how the JavaScript engine knows which variables are accessible by any given statement and how to handle two variables of the same name.

## Sections
* [About This Book](#about-this-book)
* [Compiled vs. Interpreted](#compiled-vs-interpreted)

[◂ Return to Table of Contents](../README.md)

## About This Book
The focus of this book will be on the first of the three pillars of JavaScript: [the scope system and its function closures](../../01-get-started/04/README.md#pillar-1-scope-and-closure). It will also dive into the power of the module design pattern.

Prior to execution, during parse/compile, the JavaScript engine analyzes the code's placement of variables, functions, and blocks according to the rules of scope. The resulting scope structure is generally unaffected by runtime conditions.

JavaScript functions are _first-class values_, and are assigned and passed around like any other value. Regardless of where a function is eventually executed, it retains its original scope. This is called _closure_.

_Modules_ are a code organization pattern characterized by public methods that have privileged access (via closure) to private variables and functions.

[▲ Return to Sections](#sections)

## Compiled vs. Interpreted
_Code compilation_ is a set of steps that process the text of the code and turn it into a list of instructions that the computer can understand. Typically the entire source code is processed at once and the resulting instructions are saved as output (usually a file) that can later be executed.

_Code interpretation_ is similar to compilation in that it transforms code into machine-executable instructions, but the processing model is different. Interpreted source code is transformed line-by-line (rather than all at once) and each line or statement is executed before immediately proceeding to processing the next line of the source code.

Generally the two models are mutually exclusive, however it can be more nuanced than that in reality: interpretation can take on other forms besides simply operating line-by-line on the source code. Modern JavaScript engines employ numerous variations of both compilation and interpretation.

[▲ Return to Sections](#sections)

| [Table of Contents](../README.md#table-of-contents) |
