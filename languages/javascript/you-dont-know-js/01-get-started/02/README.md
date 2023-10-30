# Chapter 2: Surveying JS
The best way to learn JS is to start writing JS.

## Sections
* [Each File is a Program](#each-file-is-a-program)

[◂ Return to Table of Contents](../README.md)

## Each File is a Program
Almost every web site/application is comprised of many different JS files (typically with the `.js` extension). JS does not see the whole application as a single program: each standalone file is its own separate program.

The reason it is important to recognize this is mainly because of how error handling works. If one file fails (during parse/compile or execution) that will not necessarily prevent the next file from being processed. It is important to ensure that each file works properly and handles failures in other files as gracefully as possible. The only way multiple standalone JS files act as a single program is by sharing state and access to public functionality via the "global scope".

Many projects use build process tools to combine many separate project files into a single output file to be delivered to a web page. In these cases JS treats this single combined file as the entire program.

Since ES6 JavaScript has also supported a file-based module format. If a file is loaded via a module-loading mechanism (`import` statement or `<script type="module">` tag) all of its code is treated as a single module. JS still treats each module separtely: similar to how "global" scope allows standalone files to interoperate at runtime, importing one module into another allows runtime interoperation.

Regardless of code organization pattern and loading mechanisms, each file should be thought of as its own (mini) program that cooperate with other (mini) programs to perform the functions of the overall application.

[▲ Return to Sections](#sections)

| [Previous: Chapter 1 - What is JavaScript?](../01/README.md) | [Table of Contents](../README.md#table-of-contents) |
