# The Vim Way
Vim is optimized for repetition. Its efficiency comes from the way it tracks and allows the replay of the most recent actions. Mastering how to craft actions so that they perform a useful unit of work when replayed is the key to becoming effective with Vim.

## Sections
* [Meet the Dot Command](#meet-the-dot-command)
* [Don't Repeat Yourself](#dont-repeat-yourself)
* [Two for the Price of One](#two-for-the-price-of-one)
* [Take One Step Back, Then Three Forward](#take-one-step-back-then-three-forward)
* [Act, Repeat, Reverse](#act-repeat-reverse)
* [Find and Replace by Hand](#find-and-replace-by-hand)
* [Meet the Dot Formula](#meet-the-dot-formula)

[◂ Return to Table of Contents](../README.md)

## Meet the Dot Command
_The dot command lets us repeat the last change. It is the most powerful and versatile command in Vim._

The dot command allows the user to repeat the last change. Vim's documentation states that the dot command "repeats the last change" (`:h .`). Within this statement is the kernel of what makes Vim's modal editing model so effective.

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

#### The Dot Command is a Micro Macro
[Chatper 11 - Macros](../11/README.md) will describe how Vim can record any arbitrary number of keystrokes to be played back later. This allows for the capturing of repetitive workflows to be played back at a keystroke. The dot command can be thought of as a miniature macro.

[▲ Return to Sections](#sections)

## Don't Repeat Yourself
_For such a common use case as appending a semicolon at the end of a series of lines, Vim provides a dedicated command that combines two steps into one._

Taking a snippet of Javascript code as example:

**[the_vim_way/2_foo_bar.js](../code/the_vim_way/2_foo_bar.js)**
<pre>
<b>var</b> foo = 1
<b>var</b> bar = 'a'
<b>var</b> foobar = foo + bar
</pre>

A semicolon needs to be appended to the end of each line. This requires moving the cursor to the end of the line, then switching to Insert mode to make the change. The `$` command will handle the motion and then `a`;&lt;Esc&gt; will make the change. To complete the task on the remaining lines `j$.` could be run twice.

The `j` command moves the cursor down one line, the `$` command moves the cursor to the end of the line, and then `.` will apply the semicolon.

#### Reduce Extraneous Movement
While the `a` command appends after the current cursor position, the `A` command appends at the end of the current line regardless of where on the line the cursor sits. `A` squashes `$a` into a single keystroke. Below is a refinement of the previous example:

| Keystrokes      | Buffer Contents                                                          |
| --------------- | ------------------------------------------------------------------------ |
| {start}         | <ins>v</ins>ar foo = 1<br />var bar = 'a'<br />var foobar = foo + bar    |
| `A`;&lt;Esc&gt; | var foo = 1<ins>;</ins><br />var bar = 'a'<br />var foobar = foo + bar   |
| `j`             | var foo = 1;<br />var bar = '<ins>a</ins>'<br />var foobar = foo + bar   |
| `.`             | var foo = 1;<br />var bar = 'a'<ins>;</ins><br />var foobar = foo + bar  |
| `j.`            | var foo = 1;<br />var bar = 'a';<br />var foobar = foo + bar<ins>;</ins> |

By using `A` instead of `$a` the cursor just has to be anywhere on the next line to change, rather than at the end of it. The change can now be repeated on consecutive lines by typing `j.` as many times as it takes. One keystroke (`j`) to move the cursor and another (`.`) to apply the semicolon to the end of the line.

While this formula works well for this small example, it would be cumbersome to apply to a large number of consecutive lines. For an alternative approach, consider running Normal mode commands across a range (outlined in [Chapter 5 - Command-Line Mode](../05/README.md#run-normal-mode-commands-across-a-range)).

[▲ Return to Sections](#sections)

## Two for the Price of One
Many of Vim's single-key commands can be see as a condensed version of two or more other commands. The table below shows an approximation of some examples:

| Compound Command | Equivalent in Longhand |
| ---------------- | ---------------------- |
| `C`              | `c$`                   |
| `s`              | `cl`                   |
| `S`              | `^C`                   |
| `I`              | `^i`                   |
| `A`              | `$a`                   |
| `o`              | `A<CR>`                |
| `O`              | `ko`                   |

Recognize that the `O` command can be used in place of `ko` (or worse `k$a<CR>`). Also note that all of these examples also switch from Normal to Insert mode. Think about how that might affect the dot command.

[▲ Return to Sections](#sections)

## Take One Step Back, Then Three Forward
_We can pad a single character with two spaces (one in front, the other behind) by using an idiomatic Vim solution. At first it might look slightly odd, but the solution has the benefit of being repeatable, which allows us to complete the task effortlessly._

Taking the following line of code as example:

**[the_vim_way/3_concat.js](../code/the_vim_way/3_concat.js)**
<pre lang="text">
<b>var</b> foo = "method("+argument1+","+argument2+")";
</pre>

Padding each + sign with spaces could make that line of code easier on the eyes:
<pre lang="text">
<b>var</b> foo = "method(" + argument1 + "," + argument2 + ")";
</pre>

#### Make the Change Repeatable
The idiomatic approach solves this problem:

| Keystrokes  | Buffer Contents                                                               |
| ----------- | ----------------------------------------------------------------------------- |
| {start}     | <ins>v</ins>ar foo = "method("+argument1+", "+argument2+")";                  |
| `f+`        | var foo = "method("<ins>+</ins>argument1+","+argument2+")";                   |
| `s`⎵+⎵<Esc> | var foo = "method("&nbsp;+<ins>&nbsp;</ins>argument1+","+argument2+")";       |
| `;`         | var foo = "method(" + argument1<ins>+</ins>","+argument2+")";                 |
| `.`         | var foo = "method(" + argument1&nbsp;+<ins>&nbsp;</ins>","+argument2+")";     |
| `;.`        | var foo = "method(" + argument1 + ","&nbsp;+<ins>&nbsp;</ins>argument2+")";   |
| `;.`        | var foo = "method(" + argument1 + "," + argument2&nbsp;+<ins>&nbsp;</ins>")"; |

The `s` command compounds two steps into one: it deletes the character under the cursor and then enters Insert mode. After deleting the + sign and entering Insert mode, `⎵+⎵` is typed in its place and then Insert mode is escaped. This allows the process to be repeated with the dot command so long as the cursor is moved to the next + sign.

#### Make the Motion Repeatable
There's another trick in the above example: the `f{char}` command tells Vim to the cursor directly to the next occurrence of the specified character if found (`:h f`). Therefore `f+` will move the cursor straight to the next + symbol.

The `;` command will repeat the last search that the `f` command performed, so instead of typing `f+` four times the first instance of it can be followed by three usages of the `;` command.

[▲ Return to Sections](#sections)

## Act, Repeat, Reverse
_When facing a repetitive task, we can achieve an optimal editing strategy by making both the motion and the change repeatable. Vim has a knack for this. It remembers our actions and keeps the most common ones within close reach so that we can easily replay them. In this tip, we'll introduce each of the actions that Vim can repeat and learn how to reverse them._

While the dot command repeats the last change some commands can be repeated by other means. For example `@:` can be used to repeat any Ex command (as discussed in [Chapter 5 - Command-Line Mode](../05/README.md#repeat-the-last-ex-command)). The last `:substitute` command can be repeated by pressing `&` ([Chapter 14 - Substitution](../14/README.md#repeat-the-previous-substitute-command)).

Knowing how to repeat actions without having to spell them out every single time is the key to efficiency: first act, then repeat. Whenever Vim makes it easy to repeat an action or a motion it always provides some way of backing out of it. In the case of the dot command, the `u` key can be used to undo the last change. For the `f{char}` command, if the `;` key is pressed too many times, the `,` key will repeat the `f{char}` command in the reverse direction ([Chapter 8 - Navigate Inside Files with Motions](../08/README.md#find-by-character)).

The following table summarizes Vim's repeatable commands along with their corresponding reverse key:

| Intent                           | Act                   | Repeat | Reverse |
| -------------------------------- | --------------------- | ------ | ------- |
| Make a change                    | {edit}                | `.`    | `,`     |
| Scan line for next character     | `f{char}`/`t{char}`   | `;`    | `,`     |
| Scan line for previous character | `F{char}`/`T{char}`   | `;`    | `,`     |
| Scan document for next match     | /pattern`<CR>`        | `n`    | `N`     |
| Scan document for previous match | ?pattern`<CR>`        | `n`    | `N`     |
| Perform substitution             | :s/target/replacement | `&`    | `u`     |
| Execute a sequence of changes    | `qx{changes}q`        | `@x`   | `u`     |

[▲ Return to Sections](#sections)

## Find and Replace by Hand
_Vim has a :substitute command for find-and-replace tasks, but with this alternative technique, we'll change the first occurrence by hand and then find and replace every other match one by one. The dot command will save us from labor, but we'll meet another nifty one-key command that makes jumping between matches a snap._

In the following file the word "content" appears on every line:

**[the_vim_way/1_copy_content.txt](../code/the_vim_way/1_copy_content.txt)**
<pre lang="text">
...We're waiting for content before the site can go live...
...If you are content with this, let's go ahead with it...
...We'll launch as soon as we have the content...
</pre>

Imagine the word "copy" (as in "copywriting") is preferred over the word "content". The substitute command could be used to replace all occurrences of "content" with "copy":

```
:%s/content/copy/g
```

However, this approach is not suitable for this text since it will create the phrase "If you are copy with this". The problem is that the word "content" has two meanings: one synonymous with the word "copy" and the other with "happy". In order to tackle this challenge, manual approval of each substitution must be provided. While the substitute command is capable of this ([Chapter 14 - Substitution](../14/README.md#eyeball-each-substitution)) there is an alternative approach that better fits the theme of this chapter.

#### Be Lazy: Search Without Typing
The `*` command executes a search for the word under the cursor (`:h *`). The word "content" can be searched for by using the search prompt (`/content`) or simply placing the cursor over the word and hitting `*`.

Consider this workflow:

| Keystrokes          | Buffer Contents                                                                                                                                                                                                    |
| ------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| {start}             | ...We're waiting for content before the site can go live...<br />...If you are <ins>c</ins>ontent with this, let's go ahead with it...<br />...We'll launch as soon as we have the content...                      |
| `*`                 | ...We're waiting for <b>content</b> before the site can go live...<br />...If you are <b>content</b> with this, let's go ahead with it...<br />...We'll launch as soon as we have the <b><ins>c</ins>ontent</b>... |
| `cw`copy&lt;Esc&gt; | ...We're waiting for <b>content</b> before the site can go live...<br />...If you are <b>content</b> with this, let's go ahead with it...<br />...We'll launch as soon as we have the cop<ins>y</ins>...           |
| `n`                 | ...We're waiting for <b><ins>c</ins>ontent</b> before the site can go live...<br />...If you are <b>content</b> with this, let's go ahead with it...<br />...We'll launch as soon as we have the copy...           |
| `.`                 | ...We're waiting for cop<ins>y</ins> before the site can go live...<br />...If you are <b>content</b> with this, let's go ahead with it...<br />...We'll launch as soon as we have the copy...                     |

The cursor starts positioned over the word "content" and then the `*` command is executed. Two things will happen here:
1. The cursor will jump foward to the next match.
2. All occurrences will be highlighted (`:set hls` may need to be run if the highlighting does not appear, see [Chapter 13 - Search](../13/README.md#highlight-search-matches)).

Once a search is executed, the cursor can be moved to the next match by pressing the `n` key.

#### Make the Change Repeatable
With the cursor positioned at the start of the word "content", the `cw` command will delete to the end of the word and then enter Insert mode. The word "copy" is written in Insert mode before escaping back to Normal mode. Since Vim records keystrokes until Insert mode is escaped, pressing `.` will now delete to the end of the current word under the cursor and replace it with "copy".

#### All Together Now
Every time `n` is pressed, the cursor advances to the next occurrence of the word "content" and it can be assessed for replacement with "copy" using the dot command.

[▲ Return to Sections](#sections)

## Meet the Dot Formula
_We've considered three simple editing tasks so far. Even though each problem was different, we found a solution using the dot command for each one. In this tip, we'll compare each solution and identify a common pattern - an optimal editing strategy tht I call the Dot Forumula._

#### The Ideal: One Keystroke to Move, One Keystroke to Execute
In all of the examples in this chapter ([Don't Repeat Yourself](#dont-repeat-yourself), [Take One Step Back, Then Three Forward](#take-one-step-back-then-three-forward), and [Find and Replace by Hand](#find-and-replace-by-hand)) the dot command repeats the last change and a single keystroke is all that's required to move the cursor to its next target. This is the ideal solution: one keystroke to move and one to execute. This pattern will be referred to as the _Dot Forumula_.

[▲ Return to Sections](#sections)

| [Table of Contents](../README.md#table-of-contents) |
