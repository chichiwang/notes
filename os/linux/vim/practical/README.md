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
