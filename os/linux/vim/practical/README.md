# Practical Common Lisp - WIP
**Author**: [Chi-chi Wang](https://github.com/chichiwang)

Notes from the book [Practical Vim, Second Edition](https://pragprog.com/titles/dnvim2/practical-vim-second-edition/) written by Drew Neil.

Date: WIP

## Table of Contents
### Notes
* [Notation](#notation)
  * [Key Sequences](#key-sequences)
  * [Key Combinations](#key-combinations)
  * [Placeholders](#placeholders)
  * [Special Keys](#special-keys)
  * [Switching Modes](#switching-modes)
  * [Command Line](#command-line)

### Chapters

## Notation
Special notation will be used in this book to denote key-presses in Vim. The notation used will be described below.

### Key Sequences
In `Normal Mode` commands are composed by typing one or more keystrokes in sequence. Most of these sequences involve two or three keystrokes, but some are longer. These commands appear as follows:

| Notation | Meaning                               |
| -------- | ------------------------------------- |
| `x`      | Press `x` once                        |
| `dw`     | In sequence, press `d`, then `w`      |
| `dap`    | In sequence, press `d`, `a`, then `p` |

### Key Combinations
`<C-p>` does not mean to press `<`, then `C`, then `-`, and so on. `<C-p>` represents `Ctrl+p` (while holding down the `<Ctrl>` key, press `p`). Vim's own documentation uses this notation to denote key combinations (`:h key-notation` for more). This notation is useful to represent key combinations within a sequence, such as:

| Notation     | Meaning                                                               |
| ------------ | --------------------------------------------------------------------- |
| `<C-n>`      | While holding `<Ctrl>` press `n`                                      |
| `g<C-]>`     | Press `g`, then while holding `<Ctrl>` press `]`                      |
| `<C-r>0`     | While holding `<Ctrl>` press `r`, then release `<Ctrl>` and press `0` |
| `<C-w><C-=>` | While holding `<Ctrl>` press `w` then `=`                         |

### Placeholders
Some commands in Vim must be followed by a particular kind of keystroke, while other commands can be followed by any key on the keyboard. Curly braces are used to denote the set of valid keystrokes that can follow a command. Some examples:

| Notation          | Meaning                                                                 |
| ----------------- | ----------------------------------------------------------------------- |
| `f{char}`         | Press `f`, followed by any other character                              |
| `` `{a-z} ``      | Press `` ` ``, followed by any lowercase letter                         |
| `m{a-zA-Z}`       | Press `m`, followed by any lowercase or uppercase letter                |
| `d{motion}`       | Press `d`, followed by any motion command                               |
| `<C-r>{register}` | While holding `<Ctrl>` press `r`, followed by the address of a register |

### Special Keys
Some keys are called by name. The following table shows a selection of them:

| Notation  | Meaning                                                 |
| --------- | ------------------------------------------------------- |
| `<Esc>`   | Press the Escape key                                    |
| `<CR>`    | Press the carriage return key (also known as `<Enter>`) |
| `<Ctrl>`  | Press the Control key                                   |
| `<Tab>`   | Press the Tab key                                       |
| `<Shift>` | Press the Shift key                                     |
| `<S-Tab>` | While holding `<Shift>` press `<Tab>`                   |
| `<Up>`    | Press the up arrow key                                  |
| `<Down>`  | Press the down arrow key                                |
| `⎵`       | Press the space bar                                     |

### Switching Modes
It is common to switch between Normal and Insert mode while using Vim. Each keystroke could mean something different depending on which mode is active. An alternative style will be used to represent keystrokes entered in Insert mode, to differentiate them from Normal mode keystrokes.

Consider this example:
`cw`replacement&lt;Esc&gt;. The Normal mode `cw` command deletes to the end of the current word and switches to Insert mode. Then the sequence types out the word "replacement" before pressng the `<Esc>` key to switch back to Normal mode again.

The Normal mode keystroke notation is also used for Visual mode keystrokes. The Insert mode styling will be used to indicate keystrokes entered in Command-Line mode and Replace mode. Which mode is active should be clear from context.

### Command Line
Some tips will execute a command-line, either in the shell or from inside Vim. The context of the command line will be differentiated by the prompt preceeding the command.

| Prompt | Meaning                                                   |
| ------ | --------------------------------------------------------- |
| $      | Enter the command line in an external shell               |
| :      | Use Command-Line mode to execute an Ex command            |
| /      | Use Command-Line mode to perform a forward search         |
| ?      | Use Command-Line mode to perform a backward search        |
| =      | Use Command-Line mode to evaluate a Vim script expression |

Any time an Ex command is listed inline (such as `:write`), following it up by pressing the `<CR>` key is implicit in the tip.

However, Vim's search command allows a preview of the first match before pressing `<CR>`. When a search command is listed inline (such as /pattern`<CR>`) the `<CR>` keystroke is listed explicitly. If it is missing from a search command, the `<CR>` is meant to be excluded.
