# Practical Common Lisp - WIP
**Author**: [Chi-chi Wang](https://github.com/chichiwang)

Notes from the book [Practical Common Lisp](http://www.gigamonkeys.com/book/) written by Peter Seibel.

Date: WIP

## Table of Contents
### Getting Started
* [Installing Common Lisp](#installing-common-lisp)
  * [Vim Plugins](#vim-plugins)
  * [Lisp in a Box](#lisp-in-a-box)
* [Using the REPL](#using-the-repl)
* [Additional Resources](#additional-resources)

### Notes
* 01: [Why Lisp?](./01/README.md)
* 02: [A Tour of the REPL](./02/README.md)
* 03: [A Simple Database](./03/README.md)
* 04: [Syntax and Semantics](./04/README.md)
* 05: [Functions](./05/README.md)
* 06: [Variables](./06/README.md)
* 07: [Macros: Standard Control Constructs](./07/README.md)
* 08: [Macros: Custom Defined](./08/README.md)
* 09: [Building a Unit Test Framework](./09/README.md)
* 10: [Numbers, Characters, and Strings](./10/README.md)

## Installing Common Lisp
For the purposes of this set of notes, the [sbcl](http://www.sbcl.org/) Common Lisp implementation will be used. To install `sbcl` follow the instructions in [The Common Lisp Cookbook - Getting Started](https://lispcookbook.github.io/cl-cookbook/getting-started.html).

### Vim Plugins
[Vlime](https://github.com/vlime/vlime) is a Common Lisp development environment for Vim (and Neovim). It provides a REPL integration, omni-completions, cross reference utilities, inspector, debugger, and many other great facilities.

[Slimv](https://github.com/vim-scripts/slimv.vim), alternatively, is a SWANK client for vim, similar to [SLIME](https://common-lisp.net/project/slime/) for Emacs.

For instructions on installing either plugin, or using them, please follow [Susam Pal's guide](https://susam.in/blog/lisp-in-vim-with-slimv-or-vlime/).

### Lisp in a Box
[Lispbox](https://common-lisp.net/project/lispbox/) is an IDE for Common Lisp development. It is a pre-configured packaging of the [Emacs](https://en.wikipedia.org/wiki/Emacs) editing environment, [SLIME](http://common-lisp.net/project/slime/), [Quicklisp](http://www.quicklisp.org/), and [Clozure Common Lisp](https://ccl.clozure.com/). Lispbox is designed to get programmers new to Common Lisp up and running in a Lisp development environment with minimu hassle.

## Using the REPL
To use the REPL, use the executable `sbcl` in your console:
```console
$ sbcl
This is SBCL 2.0.1.debian, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
* 
```

To exit the REPL enter the command `(quit)`.

## Additional Resources
* [Common Lisp the Language, 2nd Edition](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node1.html)
* [Getting Started | Common Lisp](https://lisp-lang.org/learn/getting-started/)
* [The Common Lisp Cookbook - Getting Started](https://lispcookbook.github.io/cl-cookbook/getting-started.html)
* [Vlime](https://github.com/vlime/vlime)
* [Lisp in Vim with Slimv or Vlime](https://susam.in/blog/lisp-in-vim-with-slimv-or-vlime/)
* [The Common Lisp Cookbook](https://lispcookbook.github.io/cl-cookbook)
* [Common Lisp the Language, 2nd Edition](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node1.html)
