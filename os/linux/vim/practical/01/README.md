# The Vim Way
Vim is optimized for repetition. Its efficiency comes from the way it tracks and allows the replay of the most recent actions. Mastering how to craft actions so that they perform a useful unit of work when replayed is the key to becoming effective with Vim.

## Sections
* [The Dot Command](#the-dot-command)

[◂ Return to Table of Contents](../README.md)

## The Dot Command
The dot command is the most powerful and versatile command in Vim. It allows the user to repeat the last change. Vim's documentation states that the dot command "repeats the last change" (`:h .`). Within this statement is the kernel of what makes Vim's modal editing model so effective.

The "last change" could be one of many thing. A change can act at the level of individual characters, entire lines, or even the whole file.

To demonstrate using this snippet of text:

**[the_vim_way/0_mechanics.txt](../code/the_vim_way/0_mechanics.txt)**
<pre lang="text">
Line one
Line two
Line three
Line four
</pre>

Using the `x` command deletes the character under the cursor. Repeating this action with `.` will tell Vim to once again delete the character under the cursor:

| Keystrokes | Buffer Contents                                                       |
| ---------- | --------------------------------------------------------------------- |
| {start}    | <ins>L</ins>ine one<br />Line two<br />Line three<br />Line four      |
| `x`        | <ins>i</ins>ne one<br />Line two<br />Line three<br />Line four       |
| `.`        | <ins>n</ins>e one<br />Line two<br />Line three<br />Line four        |
| `..`       | <ins>&nbsp;</ins>one<br />Line two<br />Line three<br />Line four     |

The `dd` command also performs a deletion but operates on the current line as a whole. Using the dot command following a `dd` command instructs Vim to delete the current line:

| Keystrokes | Buffer Contents                                                  |
| ---------- | ---------------------------------------------------------------- |
| {start}    | <ins>L</ins>ine one<br />Line two<br />Line three<br />Line four |
| `dd`       | <ins>L</ins>ine two<br />Line three<br />Line four               |
| `.`        | <ins>L</ins>ine three<br />Line four                             |

The `>G` command increases the indentation from the current line until the end of the file. Repeating this command with the dot command tells Vim to increase the indentation level from the current cursor position to the end of the file:

| Keystrokes | Buffer Contents                                                                                                                          |
| ---------- | ---------------------------------------------------------------------------------------------------------------------------------------- |
| {start}    | Line one<br /><ins>L</ins>ine two<br />Line three<br />Line four                                                                         |
| `>G`       | Line one<br />&nbsp;&nbsp;<ins>L</ins>ine two<br />&nbsp;&nbsp;Line three<br />&nbsp;&nbsp;Line four                                     |
| `j`        | Line one<br />&nbsp;&nbsp;Line two<br />&nbsp;&nbsp;<ins>L</ins>ine three<br />&nbsp;&nbsp;Line four                                     |
| `.`        | Line one<br />&nbsp;&nbsp;Line two<br />&nbsp;&nbsp;&nbsp;&nbsp;<ins>L</ins>ine three<br />&nbsp;&nbsp;&nbsp;&nbsp;Line four             |
| `j.`       | Line one<br />&nbsp;&nbsp;Line two<br />&nbsp;&nbsp;&nbsp;&nbsp;Line three<br />&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<ins>L</ins>ine four |

The `x`, `dd`, and `>` commands are all executed from Normal mode but a change is also created each time Insert mode is dipped into. From the moment Insert mode is entered (by pressing `i` for example) until Normal mode is returned to (by pressing `<Esc>`) Vim records every keystroke. After making a change like this, the dot command will replay all keystrokes.

| [Table of Contents](../README.md#notes) |
