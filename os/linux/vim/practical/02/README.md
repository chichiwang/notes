# Normal Mode
Normal mode is Vim's default state. Other text editors spend most of their time in something that resembles Vim's Insert mode. Many Normal mode commands can be executed with a count, causing them to be run multiple times. Much of Normal mode's power stems from the way that operator commands can be combined with motions.

Programmers spend just a fraction of their time composing code. More of their time is spent thinking, reading, and navigating from one part of a codebase to another. When making a change, they do not necessarily use Insert mode: they may reformat existing code, duplicate chunks of code, move code around, or delete chunks. Normal mode provides a lot of tools to execute these tasks.

## Sections
* [Chunking Undos](#chunking-undos)
* [Compose Repeatable Changes](#compose-repeatable-changes)
* [Use Counts to Do Simple Arithmetic](#use-counts-to-do-simple-arithmethic)
* [Don't Count If You Can Repeat](#dont-count-if-you-can-repeat)
* [Combine and Conquer](#combine-and-conquer)
* [Meet Operator-Pending Mode](#meet-operator-pending-mode)


## Chunking Undos
In other text editors invoking the undo command after typing a few words may revert the last typed word or character. In Vim the granularity of the undo command can be controlled.

The `u` key triggers the undo command which reverts the most recent change: this can be anything that modifies the text in the document including commands triggered from Normal, Visual, and Command-Line modes. A change could also encompass any text entered or deleted in Insert mode. `i` (insert some text) `<Esc>` constitutes a change. From the time Insert mode is entered until it is exited, all text changes constitute a single change. The undo command can be used to operate on words, sentences, or paragraphs simply by moderating the use of the `<Esc>` key.

**Note**: If the `<Up>`, `<Down>`, `<Left>`, `<Right>` cursor keys are used while in Insert mode, a new undo chunk is created. This has implications on the operation of the dot command.

[▲ Return to Sections](#sections)

## Compose Repeatable Changes
Vim is optimized for repetition. In order to capitalize on this, be mindful of how changes are composed.

The most obvious metric to measure which approach to doing something in Vim is best is efficiency: which technique requires the fewest number of keystrokes ([VimGolf](https://www.vimgolf.com/)). In the event of a tie, the winning technique should be the one that is the most repeatable.

For example suppose the cursor is positioned over the "h" at the end of this line of text and the goal is the delete the word "nigh":

**[normal_mode/the_end.txt](../code/normal_mode/the_end.txt)**
<pre lang="text">
The end is nigh
</pre>

There are three different approaches that can be taken to accomplish this task:

**Delete Backward**

Since the cursor is already at the end of the word, deleting backward could accomplish the task:

| Keystrokes | Buffer Contents             |
| ---------- | --------------------------- |
| {start}    | The end is nig<ins>h</ins>  |
| `db`       | The end is <ins>h</ins>     |
| `x`        | The end is <ins>&nbsp;</ins> |

Pressing `db` deletes from the cursor's starting position to the beginning of the word but it leaves the final "h" intact. This character is then deleted with `x` giving this sequence a Vim golf score of 3.

**Delete Forward**

The sequence to delete forward instead looks like:

| Keystrokes | Buffer Contents             |
| ---------- | --------------------------- |
| {start}    | The end is nig<ins>h</ins>  |
| `b`        | The end is <ins>n</ins>igh  |
| `dw`       | The end is <ins>&nbsp;</ins> |

Using `b` to maneuver the cursor into position at the start of the word "nigh", `dw` can be used to remove the entire word. This sequence also has a Vim golf score of 3.

**Delete an Entire Word**

Both of the previous solutions involve some kind of preparation or cleanup. A more surgical approach may be taken using the `aw` text object instead of a motion (see `:h aw`).

| Keystrokes | Buffer Contents            |
| ---------- | -------------------------- |
| {start}    | The end is nig<ins>h</ins> |
| `daw`      | The end i<ins>s</ins>      |

The `daw` command can be remembered using the mnemonic _delete a word_. This approach also results in a Vim golf score of 3.

**Which is the most repeatable?**

All three techniques to remove the word "nigh" (`dbx`, `bdw`, and `daw`) all required three keystrokes in Normal mode. Given that, which can be said to be the best approach?

Given that Vim is optimized for repetition, the technique that lends itself to repetition with the dot command should be considered the best approach.

The _Delete Backward_ approach is comprised of two operations: `db` deletes to start of the word and then `x` deletes the final remaining character. If the dot command is then invoked, it repeats the character deletion.

The _Delete Forward_ approach also involves two operations: `b` to position the cursor at the first character of the word followed by `dw` which deletes the word under the cursor. The dot command here will repeat `dw` deleting from the current cursor position to the beginning of the next word. Since the cursor is already at the end of the line, this context for the dot command is similarly not useful.

The _Delete an Entire Word_ solution utilizes a single operation: `daw` does not only remove a word, but surrounding whitespace characters as well. As a result, the cursor ends up on the last character of the previous word "is." Invoking the dot command from here will repeat the operation `daw` deleting another word. This makes it a far more useful operation.

The `daw` technique imparts the most power onto subsequent dot commands and so should be considered the better approach to removing a word in this example.

Making effective use of the dot command often requires some forethought. If the same small change needs to be made in a number of places, composing changes in a way that can be repeated with the dot command can be advantageous. Recognizing opportunities can take practice, but developing a habit of making changes repeatable will be more rewarding.

[▲ Return to Sections](#sections)

## Use Counts to Do Simple Arithmetic
Most Normal mode commands can be executed with a count. This feature can be exploited to do simple arithmetic. Normal mode commands, when prefixed with a count, will execute the specified number of times (see `:h count`).

The `<C-a>` and `<C-x>` commands perform addition and subtraction on numbers. When run without a count they increment or decrement a number by one. If prefixed by a number, however, these commands will add or subtract by any whole number:

| Keystroke | Buffer Contents |
| --------- | --------------- |
| {start}   | <ins>5</ins>    |
| `10<C-a>` | 1<ins>5</ins>   |

If the cursor is not already positioned on a numeric digit, the `<C-a>` command will look ahead ofr a digit on the current line. If it finds one it will jump the cursor straight to that number (see `:h ctrl-a`).


Using this snippet of CSS as example:

**[normal_mode/sprite.css](../code/normal_mode/sprite.css)**
```css
.blog, .news { background-image: url(/sprite.png); }
.blog { background-position: 0px 0px }
```

Suppose the following task:
1. Duplicate the last line
2. Replace the word "blog" with "news" on the duplicated line
3. Change "0px" to "-180px" on the duplicated line

For steps 1 and 2: `yyp` can be used to duplicate the target line, and `cW` can be used to change the first word. For step 3 one approach could be to jump to the "0px" using `f0` and then switching to Insert mode to manually change the value: `i-18<Esc>`. It would, however, be quicker just to run `180<C-x>` since it saves the step of moving the cursor into the correct position:

| Keystrokes           | Buffer Contents                                                                                                                                          |
| -------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------- |
| {start}              | .blog, .news { background-image: url(/sprite.png); }</br><ins>.</ins>blog { background-position: 0px 0px }                                               |
| `yyp`                | .blog, .news { background-image: url(/sprite.png); }</br>.blog { background-position: 0px 0px }</br><ins>.</ins>blog { background-position: 0px 0px }    |
| `cW`.news&lt;Esc&gt; | .blog, .news { background-image: url(/sprite.png); }</br>.blog { background-position: 0px 0px }</br>.new<ins>s</ins> { background-position: 0px 0px }    |
| `180<C-x>`           | .blog, .news { background-image: url(/sprite.png); }</br>.blog { background-position: 0px 0px }</br>.news { background-position: -18<ins>0</ins>px 0px } |

Suppose the last line needed to be copied ten times, subtracting 180 from each successive line. Using the `180<C-x>` command the workflow becomes identical for each successive line. The keystrokes could even be recorded in a macro and then played back as many times as needed.

#### A Note on Number Formats
Vim, prior to version 8.0, will interpret numbers with leading 0s to be in octal notation rather than decimal. If you attempt to run `<C-a>` on `007` for example you would get `010` which appears to be a decimal 10 but is actually an octal 8. To avoid this, the setting `set nrformats-=octal` can be added to the _vimrc_ file.

As of version 8.0 of Vim, the _nrformats_ setting excludes octal by default which avoids this confusion.

[▲ Return to Sections](#sections)

## Don't Count If You Can Repeat
Providing a count can minimize the number of keystrokes required to perform a certain action, but that is not always preferred. Consider the pros and cons of counting versus repeating.

Suppose the buffer contains the following text:
<pre>
Delete <ins>m</ins>ore than one word
</pre>

From the current cursor position deleting 2 words so that the text reads "Delete one word" can be approached a number of ways: both `d2w` and `2dw` will work. With `d2w` the `d` invokes the delete command and then `2w` is provided as the motion - this could be read as "delete two words." In `2dw` the count applies to the delete command and can be read as "delete a word two times." Semantics aside, the result is the same. Another option, `dw.` can be read as "delete a word and then repeat."

All three options presented require 3 keystrokes, but which is the better approach?

For both `d2w` and `2dw`, the `u` key can be used to undo the change, returning both removed words to the buffer. Following either command with a dot command will delete a further two words.

`dw.` is a slightly different scenario: here the change was `dw` so in order to restore two words `uu` or `2u` would have to be used. Following `dw.` with another dot command would delete just another word rather than two.

In this case, `dw.` is probably the superior option because it grants greater granularity to subsequent commands. To delete seven words either `d7w` or `d......` could be used and while `d7w` is far fewer keystrokes, it requires more of the user to count the exact number of words. If the count was incorrect and 8 words needed to be deleted, following `d7w` with a repeat or undo would not achieve the desired outcome. In this case `dw.` provides a greater level of control and granularity in subsequent commands.

`dw.` in this instance is more in line with the mantra [Act, Repeat, Reverse](../01/README.md#act-repeat-reverse).

#### Use a Count When It Matters
Suppose instead the text "I have a couple of questions." needed to be changed to read "I have some more questions." This can be accomplished in the following way:

| Keystrokes                | Buffer Contents                          |
| ------------------------- | ---------------------------------------- |
| {start}                   | I have <ins>a</ins> couple of questions. |
| `c3w`some more&lt;Esc&gt; | I have some mor<ins>e</ins> questions.   |

In this scenario it makes less sense to use the dot command. While the words "a couple of" could be removed with `dw..`, to add the new words Insert mode would have to be used. In this instance, staying in Normal mode feels more fluid and so `c3w` is preferred.

These nuances in approach are a matter of personal preference and each user should determine how much they value a clean undo history and how tiresome it is to use counts.

[▲ Return to Sections](#sections)

## Combine and Conquer
_Much of Vim's power stems from the way that operators and motions can be combined._

#### Operator + motion = Action

The `d{motion}` command can operate on a single character (`dl`), a complete word (`daw`), or an entire paragraph (`dap`). The reach of the command is defined by the motion. The same applies to `c{motion}`, `y{motion}`, and a handful of others. Collectively these commands are called _operators_ (see `:h operator` for the complete list).

Below is a summary of some of the more common operators:

| Trigger | Effect                                            |
| ------- | ------------------------------------------------- |
| `c`     | Change                                            |
| `d`     | Delete                                            |
| `y`     | Yank into register                                |
| `g~`    | Swap case                                         |
| `gu`    | Make lowercase                                    |
| `gU`    | Make uppercase                                    |
| `>`     | Shift right                                       |
| `<`     | Shift left                                        |
| `=`     | Autoindent                                        |
| `!`     | Filter {motion} lines through an external program |

The `g~`, `gu`, and `gU` commands are invoked by two keystrokes. The `g` can be considered to be a prefix that modifies the behavior of the subsequent keystroke (See [Meet Operator-Pending Mode](#meet-operator-pending-mode)).

The combination of operators with motions forms a kind of grammar: an action is composed from an operator followed by a motion. Learning new operators and motions is learning new vocabulary in Vim. More ideas can be expressed as vocabulary grows.

In the action `daw`, the `d` is a delete operator and `aw` is a motion for a full word. Knowing this: the motion `aw` can be combined with another operator `gU` in the form of `gUaw` to make an entire word uppercase. Combining the motion `ap` (a paragraph) with `gU` will form the action `gUap` to make an entire paragraph uppercase.

In Vim, whenever an operator command is invoked in duplicate it acts upon the current line. `dd` will delete the current line while `>>` indents the entire line. The `gU` command, being two keystrokes, is a special case: `gUgU` can be used to uppercase an entire line, or `gUU` can be used as shorthand for this.

### Extending Vim's Combinatorial Powers
#### Custom Operators Work with Existing Motions
The number of actions that can be performed using Vim's default set of operators and motions is vast, but these can be extended even further by rolling custom motions and operators.

While the standard set of operators that ships with Vim is relatively small, it is possible to define new ones. Tim Pope's [commentary.vim plugin](https://github.com/tpope/vim-commentary) is a good example: it adds an operator for commenting and uncommenting lines of code in all languages supported by Vim. This plugin adds the operator `gc{motion}` which toggles comments for the specified lines. All motins will work with this custom operator: `gcap` will toggle commenting for the current paragraph, `gcG` will toggle commenting from the current line to the end of the file, `gcc` will toggle the current line.

To learn more about creating custom operators, start by reading `:h :map-operator`.

#### Custom Motions Work with Existing Operators
Similary, Vim's standard set of motions is comprehensive but new motions and text objects can also be defined.

Kana Natsuno's [textobj-entire plugin](https://github.com/kana/vim-textobj-entire) is a good example: it adds the text objects `ie` and `ae` which act on the entire file. Without the help of the plugin, to autoindent an entire file `gg=G` would be used: `gg` to move the cursor to the top of file, `=G` to autoindent to the end of file. With Kana's plugin `=ae` can be used instead and it would not matter where the cursor was in the file at the time the action was invoked.

To learn more about creating custom motions, start by reading `:h omap-info`.

[▲ Return to Sections](#sections)

## Meet Operator-Pending Mode
Normal, Insert, and Visual modes are readily identified in Vim, but there are other modes that are easy to overlook. Operator-Pending mode is one such mode. In inputting the keystrokes for the action `dw` Operator-Pending mode is triggered, lasting between the interval when the `d` and `w` keys are pressed.

If Vim is thought of as a finite-state machine then Operator-Pending mode is a state that only accepts motion commands. This mode is activated when an operator is invoked and then nothing happens until a motion is supplied completing the operation. When Operator-Pending mode is active &lt;Esc&gt; will abort the operator and return Vim to Normal mode.

There are commands in Vim that are invoked by two or more keystrokes (see `:h g`, `:h z`, `:h ctrl-w`, or `:h [`). In most of these cases the first keystroke merely acts as a prefix for the second and will not initiate Operator-Pending mode. These can be thought of namespaces that expand the number of available command mappings. Only operator commands initiate Operator-Pending mode.

The reason an entire mode exists between inputting an operator and completing the action with a motion, whereas namespaced commands are an extension of Normal mode, is to allow custom mappings that initiate or target Operator-Pending mode. It allows Vim's vocabulary to be extended by the creation of custom operators and commands.

[▲ Return to Sections](#sections)

| [Previous: 01 - The Vim Way](../01/README.md) | [Table of Contents](../README.md#table-of-contents) | [Next: 03 - Insert Mode](../03/README.md) |
