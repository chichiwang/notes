# Smash into Vim - WIP
**Author**: [Chi-chi Wang](https://github.com/chichiwang)

These notes follow along with the [PluralSight Course](https://app.pluralsight.com/library/courses/smash-into-vim/table-of-contents) of the same title.

Notes completed: ongoing

## Table of Contents
* [Vim](#vim)
  * [Common Conventions](#common-conventions)

## Vim
The [vim](https://en.wikipedia.org/wiki/Vim_(text_editor)) text editor was specifically built to help programmers edit text more efficiently, because of its origins in the terminal. It can be daunting, but it is not as hard as it looks.

The benefits of learning vim include:
* It is available on every platform
* It is highly configurable
* It is 100% keyboard-driven
* It is extendable with plugins
* It allows for a high degree of precision in text editing
* It provides powerful editing capabilities
* It has great documentation
* It allows editing of files both locally and remotely

Some drawbacks of vim include:
* Many defaults are undesirable
* It has a clunky scripting language
  * Places an upper bound on functionalities that can be scripted
* Destructive tasks are too easy to execute
  * Such as overwriting the paste buffer
* It default regular expression syntax is inconsistent

Vim was created in 1991 by [Bram Moolenaar](https://en.wikipedia.org/wiki/Bram_Moolenaar) as an improvement over [Bill Joy's](https://en.wikipedia.org/wiki/Bill_Joy) [vi](https://en.wikipedia.org/wiki/Vi) (created in 1976). The name vim is a contraction of **Vi IMproved**.

While it looks complex, Vim is built on two simple ideas:
1. Modal Editing
2. Operators

Vim's primary philosophy is centered around editing text efficiently. The goal of vim is to make navigation and editing as easy as, or even easier than, typing the text the first time. Most software prioritizes text input over editing. In the spirit of efficiency, the operations most frequently performed are available on the home row of the keyboard.

What a particular key does in vim depends on the mode. The letter `j` for example has three different functions depending on the mode:
* Command Mode: Move the cursor down one line
* Insert Mode: Add the letter 'j' to the text at the cursor
* Visual Mode: Move selection down one full line

There are five main modes in vim:
* Normal (Command)
* Insert
* Visual
* Replace
* Command-Line

There are anumber of different command types, including:
* Operator
* Count
* Motion

In the example `d2w`, `d` is the operator command (delete), `2` is the count command, and `w` is the motion command (word). This chain of commands will delete 2 words from the cursor position.

To get help on any command or topic use `:h search-term`, such as `:h movement`. To exit help, type `:bd` for _buffer delete_.

### Common Conventions
Uppercased commands are often super-sized versions of their lower-case counterparts. Some examples include:
* `i` inserts text at the cursor
* `I` inserts text at the beginning of the line that the cursor is sitting on
* `w` moves the cursor foward by one word
* `W` treats continguous code as one word and jumps forward to the next meaningful code word

Adding `!` to the end of a command will force it to happen without confirmation. To quit vim, `:q` is used. But vim won't quit if there are unsaved changes in the file. `:q!` will force vim to exit without saving.
