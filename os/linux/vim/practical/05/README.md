# Command-Line Mode
Vim traces its ancestry back to vi where the modal editing paradigm was conceived. Vi traces its ancestry back to a line editor called ex, which is why ex commands exist. The DNA of early Unix text editors still exists within modern Vim. Ex commands are still the best tool for some line-oriented tasks.

Command-Line mode exposes to users the vestiges of ex.

## Sections
* [Meet Vim's Command Line](#meet-vims-command-line)
* [Execute a Command on One or More Consecutive Lines](#execute-a-command-on-one-or-more-consecutive-lines)
* [Duplicate or Move Lines](#duplicate-or-move-lines)
* [Run Normal Mode Commands Across a Range](#run-normal-mode-commands-across-a-range)

[◂ Return to Table of Contents](../README.md)

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

#### Ex-Commands Strike Far and Wide
There are situations where the same changes can be made faster with Ex commands than with Normal mode. Normal mode commands tend to act on the current character or current line whereas Ex commands can be executed anywhere without moving the cursor. Ex commands can also be executed across many lines at the same time.

[▲ Return to Sections](#sections)

## Execute a Command on One or More Consecutive Lines
_Many Ex commands can be given a [range] of lines to act upon. The start and end of a range can be speficied with a line number, a mark, or a pattern._

Using the following as example:

**[cmdline_mode/practical-vim.html](../code/cmdline_mode/practical-vim.html)**
```html
<!DOCTYPE html>
<html>
  <head><title>Practical Vim</title></head>
  <body><h1>Practical Vim</h1></body>
</html>
```

The following sections will demonstrate the syntax to operate on a range of lines using the `:print` command. Replace the `:print` command in these examples with `:delete`, `:join`, `:substitute`, or `:normal` to get a feel for how useful Ex commands can be.

#### Use Line Numbers as an Address
An Ex command consisting of only a number is interpreted as an address and Vim will move the cursor to the specified line. Jump to the top of the file with the following command:

```
=> :1
=> :print
<= 1 <!DOCTYPE html>
```

The sample file only contains 5 lines. To move the cursor to the last line use `:5` or the special symbol `:$` instead:

```
=> :$
=> :p
<= 5 </html>
```

In the above example `:p` is the abbreviated form or `:print`. Instead of splitting up the two commands they can be rolled into a single command:

```
=> :3p
<= 3 <head><title>Practical Vim</title></head>
```

The above command moves the cursor to line 3 and then echoes the contents of that line. To move the cursor to line 3 and delete the entire line use `:3d` instead. To achieve the same result in Normal mode two distinct commands would need to be used: `3G` followed by `dd`.

#### Specify a Range of Lines by Address
The above section shows how to specify an address as a single line number, but a range of lines can also be specified:

```
=> 2,5p
<= 2 <html>
   3   <head><title>Practical Vim</title></head>
   4   <body><h1>Practical Vim</h1></body>
   5 </html>
```

This prints each line from 2 to 5, inclusive. After running this command the cursor will be placed on line 5. In general a range takes the form `:{start},{end}`. Both `{start}` and `{end}` are addresses. So far the examples have used line numbers for addresses, but a pattern or a mark could also be used.

The `.` symbol is used as an address representing the current line. It is easy to compose a range representing everything from the current line to the end of the file:

```
=> :2
=> :.,$p
<= 2 <html>
   3   <head><title>Practical Vim</title></head>
   4   <body><h1>Practical Vim</h1></body>
   5 </html>
```

The `%` symbol represents all of the lines in the current file:

```
=> :%p
<= 1 <DOCTYPE html>
   2 <html>
   3   <head><title>Practical Vim</title></head>
   4   <body><h1>Practical Vim</h1></body>
   5 </html>
```

This is the equivalent of `:1,$p`. Using the `:%` shorthand with the `:substitute` command is common :`:%s/Practical/Pragmatic/`. That command would replace the first occurence of "Practical" with "Pragmatic" on every line of the file.

#### Specify a Range of Lines by Visual Selection
Instead of addressing a range of lines by number a visual selection could be used instead. Using the command `2G` followed by `VG` would make the following visual selection:

<pre lang="text">
&lt;!DOCTYPE html&gt;
<b>&lt;html&gt;
  &lt;head&gt;&lt;title&gt;Practical Vim&lt;/title&gt;&lt;/head&gt;
  &lt;body&gt;&lt;h1&gt;Practical Vim&lt;/h1&gt;&lt;/body&gt;
&lt;/html&gt;</b>
</pre>

Pressing the `:` key after making the selection will prepopulate the command-line prompt with the range `'<,'>`. This is a range standing for the visual selection. Specifying the Ex command using this range will execute it on every selected line:

```
=> :'<,'>p
<= 2 <html>
   3   <head><title>Practical Vim</title></head>
   4   <body><h1>Practical Vim</h1></body>
   5 </html>
```

This method of selecting a range can be useful if the `:substitute` command needs to be run on a subset of the current file.

`'<` and `'>` are marks that stand for the first and last lines of a visual selection. These marks persist even after leaving Visual mode. Running `:'<,'>p` from Normal mode will always act on the lines that most recently formed a Visual mode selection.

#### Specify a Range of Lines by Patterns
Vim also accepts a pattern as an address for an Ex command:

```
=> :/<html>/,/<\/html>/p
<= 2 <html>
   3   <head><title>Practical Vim</title></head>
   4   <body><h1>Practical Vim</h1></body>
   5 </html>
```

The `{start}` address is the line containing the text that matches the pattern `/<html>/` and the `{end}` address is the line containing the text that matches the pattern `/<\/html>/`. The same result could be achieved with `:2,5p` but the pattern approach is a bit less brittle: it does not rely on specifying the exact line numbers.

#### Modify an Address Using an Offset
In order to run the Ex command on every line inside the html block instead:

```
=> :/<html>/+1,/<\/html>/-1p
<= 3   <head><title>Practical Vim</title></head>
   4   <body><h1>Practical Vim</h1></body>
```

The general form for an offset is `:{address}+n`. If `n` is omitted it defaults to 1. `{address}` can be a number, mark, or pattern.

To execute a command on a particular number of lines starting with the current line:

```
=> :2
=> :.,.+3p
```

Since `.` stands for the current line `:.,.+3` is equivalent to `:2,5` in this example.

#### Discussion
The syntax for defining a range is very flexible: numbers, marks, and patterns can be mixed and matched. The following table summarizes a few of the symbols that can be used to create addresses and ranges for Ex commands:

| Symbol | Address                                   |
| ------ | ----------------------------------------- |
| 1      | First line of the file                    |
| $      | Last line of the file                     |
| 0      | Virtual line above first line of the file |
| .      | Line where the cursor is placed           |
| 'm     | Line containing mark _m_                  |
| '&lt;  | Start of visual selection                 |
| '&gt;  | End of visual selection                   |
| %      | The entire file (shorthand for `:1,$`)    |

Line 0 doesn't really exist but it can be useful as an address in certain contexts. For example it can be used as the final argument in the `:copy {address}` and `:move {address}` commands when copying or moving a range of lines to the top of a file.

A specified `[range]` always represents a set of contiguous lines. It is also possible to execute an Ex command on a set of noncontiguous lines using the `:global` command (covered in Chapter 15).

[▲ Return to Sections](#sections)

## Duplicate or Move Lines
The Ex command `:copy` (and its shorthand `:t`) duplicate one or more lines from one part of the document to another. The `:move` command will move one or more lines from one part of the document to another.

Using the following as example:

**[cmdline_mode/shopping-list.todo](../code/cmdline_mode/shopping-list.todo)**
```
Shopping list
    Hardware Store
        Buy new hammer
    Beauty Parlor
        Buy nail polish remover
        Buy nails
```

#### Duplicate Lines with the `:t` Command

To copy the line "Buy nails" and place it under the "Hardware Store" portion of the list:

| Keystrokes | Buffer Contents                                                                                                                                                                                                                                                                                                                                                                                |
| ---------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| {start}    | Shopping list<br />&nbsp;&nbsp;&nbsp;&nbsp;<ins>H</ins>ardware Store<br />&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Buy new hammer<br />&nbsp;&nbsp;&nbsp;&nbsp;Beauty Parlor<br />&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Buy nail polish remover<br />&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Buy nails                                                                |
| `:6copy.`  | Shopping list<br />&nbsp;&nbsp;&nbsp;&nbsp;Hardware Store<br />&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<ins>B</ins>uy nails<br />&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Buy new hammer<br />&nbsp;&nbsp;&nbsp;&nbsp;Beauty Parlor<br />&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Buy nail polish remover<br />&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Buy nails |

The format for the copy command (see `:h :copy`) is `:[range]copy {address}`. In the above example the `[range]` is line 6 and the `{address}` is `.` (shorthand for the current line). The `:6copy.` command copies line 6 and places it below the current line.

The shortened versions of the `:copy` command are `:co` and `:t` (think of it as _copy TO_). A few examples of the `:t` command in action:

| Command    | Effect                                                       |
| ---------- | ------------------------------------------------------------ |
| `:6t.`     | Copy line 6 to below the current line                        |
| `:t6`      | Copy the current line to below line 6                        |
| `:t.`      | Duplicate the current line (similar to `yyp` in Normal mode) |
| `:t$`      | Copy the current line to the end of the file                 |
| `:'<,'>t0` | Copy the visually selected lines to the start of the file    |

`:t.` duplicates the current line much like `yyp` in Normal mode. The difference is that `:t.` doesn't use a register, so it is preferable to `yyp` when copied content in the register needs to be preserved.

When duplicating distant lines (lines further from the cursor) `:t` is preferable to `yy`.

#### Move Lines with the `:m` Command
The `:move` command (see `:h :move`) has similar syntax to the `:copy` command: `:[range]move {address}`. The shortened version of the `:move command` is `:m`.

Using the example, to move the "Hardware Store" section of the "Shopping list" to the end:

| Keystrokes | Buffer Contents                                                                                                                                                                                                                                                                                                                                                                                       |
| ---------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| {start}    | Shopping list<br />&nbsp;&nbsp;&nbsp;&nbsp;<ins>H</ins>ardware Store<br />&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Buy nails<br />&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Buy new hammer<br />&nbsp;&nbsp;&nbsp;&nbsp;Beauty Parlor<br />&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Buy nail polish remover<br />&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Buy nails        |
| `Vjj`      | Shopping list<br />&nbsp;&nbsp;&nbsp;&nbsp;<b>Hardware Store<br />&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Buy nails<br />&nbsp;&nbsp;&nbsp;&nbsp;<ins>&nbsp;&nbsp;</ins>&nbsp;&nbsp;Buy new hammer</b><br />&nbsp;&nbsp;&nbsp;&nbsp;Beauty Parlor<br />&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Buy nail polish remover<br />&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Buy nails |
| `:'<,'>m$` | Shopping list<br />&nbsp;&nbsp;&nbsp;&nbsp;Beauty Parlor<br />&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Buy nail polish remover<br />&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Buy nails<br />&nbsp;&nbsp;&nbsp;&nbsp;Hardware Store<br />&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Buy nails<br />&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<ins>B</ins>uy new hammer        |

After making the visual selection the command `:'<,'>m$` moves the selection to the end of the file. Aternatively `dGp` could be used in Normal mode: `d` will cut the visual selection, `G` to move the cursor to the last line in the file, and `p` to paste the contents of the register to the next line.

After running the above commands, another visual selection could be made and then repeating the `:'<,'>m$` command would move the new selection to the end of the file. Repeating the last Ex command can be done by pressing `@:`. This method is more easily reproducible than using Normal mode commands.

[▲ Return to Sections](#sections)

## Run Normal Mode Commands Across a Range
The `:normal` command can be used to run a Normal mode codmmand on a series of consecutive lines. Used in combination with the dot command or a macro repetitive commands can be performed with very little effort.

In a [previous example](../01/README.md#dont-repeat-yourself) the dot command was used to append a semicolon to the end of every line easily. The example was only across 3 consecutive lines. Supposing it was 50 lines instead `j` would have to be pressed 50 times to complete the task. There is a better way.

Using the following as example:

**[cmdline_mode/foobar.js](../code/cmdline_mode/foobar.js)**
```javascript
var foo = 1
var bar = 'a'
var baz = 'z'
var foobar = foo + bar
var foobarbaz = foo + bar + baz
```

Start by changing the first line:

| Keystrokes | Buffer Contents                                                                                                                |
| ---------- | ------------------------------------------------------------------------------------------------------------------------------ |
| {start}    | <ins>v</ins>ar foo = 1<br />var bar = 'a'<br />var baz = 'z'<br />var foobar = foo + bar<br />var foobarbaz = foo + bar + baz  |
| `A;<Esc>`  | var foo = 1<ins>;</ins><br />var bar = 'a'<br />var baz = 'z'<br />var foobar = foo + bar<br />var foobarbaz = foo + bar + baz |

Instead of executing `.` on each following line one by one use the `:normal` Ex command to execute the dot command across a range of lines:

| Keystrokes       | Buffer Contents                                                                                                                       |
| ---------------- | ------------------------------------------------------------------------------------------------------------------------------------- |
| `jVG`            | var foo = 1;<br /><b>var bar = 'a'<br />var baz = 'z'<br />var foobar = foo + bar<br /><ins>var</ins> foobarbaz = foo + bar + baz</b> |
| `:'<,'>normal .` | var foo = 1;<br />var bar = 'a';<br />var baz = 'z';<br />var foobar = foo + bar;<br />var foobarbaz = foo + bar + baz<ins>;</ins>    |

The `:'<,'>normal .` command executes the Normal mode command `.` across every line in the visual selection. This technique works just as well for five lines or fifty lines - the lines just need to be selected in Visual mode. Any Normal mode command can be executed this way.

Appending a semicolon to the end of every line in a file can be achieved with `:%normal A;`. Making this change involves switching to Insert mode but Vim automatically reverts to Normal mode afterwards. Before executing the specified command on each line Vim moves the cursor to the beginning of the line so `:%normal i//` could be used to comment out an entire JavaScript file.

It is possibe to use `:normal` with any normal command but it is most powerful when used in combination with one of Vim's repeat commands: `:normal .` for simple tasks or `:normal @q` for complex tasks. The `:normal` command combines the expressive nature of Vim's Normal mode with the range of Ex commands.

[▲ Return to Sections](#sections)

| [Previous: 04 - Visual Mode](../04/README.md) | [Table of Contents](../README.md#table-of-contents) |
