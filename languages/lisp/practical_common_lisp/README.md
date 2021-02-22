# Practical Common Lisp - WIP
**Author**: [Chi-chi Wang](https://github.com/chichiwang)

Notes from the book [Practical Common Lisp](http://www.gigamonkeys.com/book/) written by Peter Seibel.

Date: WIP

## Table of Contents
### Getting Started
* [Installing Common Lisp](#installing-common-lisp)
  * [Vim Plugins](#vim-plugins)
* [Using the REPL](#using-the-repl)

### Notes
* 01: [Why Lisp?](./01/README.md)
* [Additional Resources](#additional-resources)

## Installing Common Lisp
For the purposes of this set of notes, the [sbcl](http://www.sbcl.org/) Common Lisp implementation will be used. To install `sbcl` follow the instructions in [The Common Lisp Cookbook - Getting Started](https://lispcookbook.github.io/cl-cookbook/getting-started.html).

### Vim Plugins
[Vlime](https://github.com/vlime/vlime) is a Common Lisp development environment for Vim (and Neovim). It provides a REPL integration, omni-completions, cross reference utilities, inspector, debugger, and many other great facilities.

[Slimv](https://github.com/vim-scripts/slimv.vim), alternatively, is a SWANK client for vim, similar to [SLIME](https://common-lisp.net/project/slime/) for Emacs.

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
* [Getting Started | Common Lisp](https://lisp-lang.org/learn/getting-started/)
* [The Common Lisp Cookbook - Getting Started](https://lispcookbook.github.io/cl-cookbook/getting-started.html)
* [Vlime](https://github.com/vlime/vlime)
