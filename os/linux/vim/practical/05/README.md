# Command-Line Mode
Vim traces its ancestry back to vi where the modal editing paradigm was conceived. Vi traces its ancestry back to a line editor called ex, which is why ex commands exist. The DNA of early Unix text editors still exists within modern Vim. Ex commands are still the best tool for some line-oriented tasks.

Command-Line mode exposes to users the vestiges of ex.

## Sections
* [Meet Vim's Command Line](#meet-vims-command-line)

## Meet Vim's Command-Line
Command-Line mode prompts the user to enter an Ex command, a search pattern, or an expression. For historical reasons the commands executed in Command-Line mode are called _Ex commands_.

The `:` key switches Vim into Command-Line mode. This mode has some resemblance to the command line used in the shell. A command is typed into this prompt and excuted by pressing `<CR>`. Command-Line mode can be exited into Normal Mode by pressing `<Esc>` at any time. Command-Line mode is also enabled by pressing `/` to bring up the search prompt or `<C-r>=>` to access the expression register.

Examples of Ex commands that operate on the text in a buffer:

| Command                                       | Effect                                                                      |
| --------------------------------------------- | --------------------------------------------------------------------------- |
| :[range]delete [x]                            | Delete specified lines [into register x]                                    |
| :[range]yank [x]                              | Yank specified lines [into register x]                                      |
| :[line]put [x]                                | Put the text from register x after the specified line                       |
| :[range]copy {address}                        | Copy the specified lines to below the line specified by {address}           |
| :[range]move {address}                        | Move the specified lines to below the line specified by {address}           |
| :[range]join                                  | Join the specified lines                                                    |
| :[range]normal {commands}                     | Execute Normal mode {commands} on each specified line                       |
| :[range]substitute/{pattern}/{string}/[flags] | Replace occurrences of {pattern} with {string} on each specified line       |
| :[range]global/{pattern}/[cmd]                | Execute the Ex command [cmd] on all specified lines where {pattern} matches |

Ex commands can be used to read (`:edit`) or write (`:write`) files, create tabs (`:tabnew`), split windows (`:split`), interact with the argument list (`:prev`/`:next`), or the buffer list (`:bprev`/`:bnext`). Vim has an Ex command for just about everything (`:h ex-cmd-index`).

The above table shows a selection of the most useful Ex commands that operate on text in a buffer.

#### On the Etymology of Vim (and Family)
ed was the original Unix text editor, created at a time when video displays were uncommon. Source code was printed onto a roll of paper and edited on a teletype terminal. Commands entered at the terminal were sent to a mainframe computer for processing and the output of each command would be printed.

ed went through several generations of improvements including em ("editor for mortals"), en, and finally ex. Eventually video terminals were more common and ex added a feature that turned the terminal screen into an interactive window that showed the contents of a file. This screen-editing mode was activated by entering the `:visual` command, or `:vi` for short. This is where the name vi comes from.

Vim stands for _vi improved_. `:h vi-differences` lists the Vim features that are unavailable in vi.

#### Special Keys in Vim's Command-Line Mode
In Command-Line mode most button presses simply enter a character. Control key chords can be used to trigger commands.

Some commands are shared with Insert mode: `<C-w>` and `<C-u>` delete backward to the start of the previous word or start of the line. `<C-v>` or `<C-k>` are used to insert characters not found on the keyboard. `<C-r>{register}` is used to insert the contents of any register at the command line.

[▲ Return to Sections](#sections)

| [Previous: 04 - Visual Mode](../04/README.md) | [Table of Contents](../README.md#table-of-contents) |
