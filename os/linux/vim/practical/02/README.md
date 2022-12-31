# Normal Mode
Normal mode is Vim's default state. Other text editors spend most of their time in something that resembles Vim's Insert mode. Many Normal mode commands can be executed with a count, causing them to be run multiple times. Much of Normal mode's power stems from the way that operator commands can be combined with motions.

Programmers spend just a fraction of their time composing code. More of their time is spent thinking, reading, and navigating from one part of a codebase to another. When making a change, they do not necessarily use Insert mode: they may reformat existing code, duplicate chunks of code, move code around, or delete chunks. Normal mode provides a lot of tools to execute these tasks.

## Sections
* [Chunking Undos](#chunking-undos)
* [Compose Repeatable Changes](#compose-repeatable-changes)


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


| [Previous: The Vim Way](../01/README.md) | [Table of Contents](../README.md#table-of-contents) |
