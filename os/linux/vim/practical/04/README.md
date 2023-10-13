# Visual Mode
Vim's Visual mode allows a selection of text to be defined and then operated upon. There are three variants of visual mode involving working with characters, lines, or rectangular blocks of text.

The dot command can be used to repeat Visual mode commands - it is especially effective when working on line-wise regions but can fall short of expectations when used with character-wise selections.

Visual-Block mode allows operations on rectangular columns of text. There are many uses for this feature.

## Sections
* [Grok Visual Mode](#grok-visual-mode)
* [Meet Select Mode](#meet-select-mode)
* [Define a Visual Selection](#define-a-visual-selection)
* [Repeat Line-Wise Visual Commands](#repeat-line-wise-visual-commands)
* [Prefer Operators to Visual Commands Where Possible](#prefer-operators-to-visual-commands-where-possible)
* [Edit Tablular Data with Visual-Block Mode](#edit-tablular-data-with-visual-block-mode)
* [Change Columns of Text](#change-columns-of-text)

## Grok Visual Mode
_Visual mode allows the user to select a range of text and then operate upon it. Vim's perspective of selecting text is different than other text editors._

Visual mode is just another mode: each key performs a different function. Many of the commands from Normal mode work the same way in Visual mode. `h`, `j`, `k`, and `l` still navigate the cursor, `f{char}` will still jump to a character on the current line. Each time the cursor is moved in Visual mode, the bounds of the selection is changed.

Some Visual mode commands are a slight variation of their Normal mode counterparts. For example: the `c` command is consistent in both modes in that it deletes the specified text and then switches to Insert mode. The difference is how the range on which to act is specified. In Normal mode, the change operator command is first triggered followed by a motion to specify the range. In Visual mode the range on which to act is first selected, and then the change operator command is triggered. This inversion of control can be generalized for all operator commands.

For example, to change a word "March" to "April", hover the cursor over the word "March" and press `viw` to select the word in Visual mode. Pressing `c` will now delete the selected word and drop Vim into Insert mode, allowing editing of the text.

[▲ Return to Sections](#sections)

## Meet Select Mode
In a typical text editing environment selected text is deleted when the user types any printable character. Vim's Visual mode doens't follow this convention but Select mode does. According to Vim's built-in documentation it "resembles the selection mode in Microsoft Windows." (see `:h Select-mode`) Printable characters will cause the selection to be deleted and then Vim enters Insert mode and the typed character is inserted.

To toggle between Visual and Select modes press `<C-g>`. The only visible difference between these modes is the message at the bottom of the screen (which will read "-- VISUAL --" in Visual mode and "-- SELECT --" in Select mode.

Those happy with the modal nature of Vim should find little use for Select mode.

[▲ Return to Sections](#sections)

## Define a Visual Selection
Vim has three variations of Visual mode:
1. **Character-wise Visual mode** allows selections from a single character to a range of characters within a line or spanning multiple lines. This is suitable for operating on individual words or phrases.
2. **Line-wise Visual mode** allows selections of entire lines.
3. **Block-wise Visual mode** allows selections of columnar regions of the document.

#### Enabling Visual Modes

| Command | Effect                             |
| ------- | ---------------------------------- |
| `v`     | Enable character-wise Visual mode  |
| `V`     | Enable line-wise Visual mode       |
| `<C-v>` | Enable block-wise Visual mode      |
| `gv`    | Reselect the last visual selection |

`gv` is a useful shortcut: it reselects the range of text that was last selected in a Visual mode. It will reselect the previous range regardless of which Visual mode the previous selection was made in. The only case where this command may fail is if the last selection has since been deleted.

#### Switching Between Visual Modes

From Normal mode, pressing `v` will switch into Character-wise Visual mode. From Character-wise Visual mode pressing `v` will return to Normal mode. In this way `v`, `V`, and `<C-v>` behave like toggles between Normal mode and the corresponding Visual modes. From Character-wise Visual mode, however, pressing `V` will switch into Line-wise Visual mode.

| Command         | Effect                                                                                          |
| --------------- | ----------------------------------------------------------------------------------------------- |
| `<Esc>`/`<C-[>` | Switch to Normal mode                                                                           |
| `v`/`V`/`<C-v>` | Switch to Normal mode (when used from character-, line-, or block-wise Visual mode respectively |
| `v`             | Switch to character-wise Visual mode                                                            |
| `V`             | Switch to line-wise Visual mode                                                                 |
| `<C-v>`         | Switch to block-wise Visual mode                                                                |
| `o`             | Move cursor to the other end of highlighted text                                                |

#### Toggling the Free End of a Selection

The _range_ of a Visual mode selection is marked by two ends: one end remains fixed while the other moves freely with the cursor. The `o` key can be used to toggle which end is the free end. This is particularly useful when the starting bound of the range turns out incorrect - rather than leaving Visual mode and starting fresh `o` can be used to redefine the bounds of the selection.

| Keystrokes | Buffer Contents                            |
| ---------- | ------------------------------------------ |
| {start}    | Select from here to <ins>h</ins>ere        |
| `vbb`      | Select from **<ins>h</ins>ere to h**ere    |
| `o`        | Select from <b>here to <ins>h</ins></b>ere |
| `e`        | Select from **here to her<ins>e</ins>**    |

[▲ Return to Sections](#sections)

## Repeat Line-Wise Visual Commands
_When the dot command is used to repeat a change made to a visual selection, it repeats the change on the same range of text._

After executing a command from Visual mode Vim returns to Normal mode and the previously selected range of text is unselected. What if the same operation needs to be repeated on the same range of text?

Using the following as example:

**[visual_mode/fibonacci-malformed.py](../code/visual_mode/fibonacci-malformed.py)**
<pre lang="python">
def fib(n):
    a, b = 0, 1
    while a < n:
print a,
a, b = b, a+b
fib(42)
</pre>

This code sample uses four spaces per indentation. In order to match this style so that the `<` and `>` commands work correctly, the `shiftwidth` and `softtabstop` options should be set to `4` and the `expandtab` option should be enabled. The following command will set the correct options: `:set shiftwidth=4 softtabstop=4 expandtab`.

To fix the indentation in the above code sample, the two lines below the _while_ command should be indented a further two levels. To fix this visually select the two offending lines and use `>` to indent them one level. After using this command, however, Vim will drop back into Normal mode, requiring the operation to be repeated. One way to do this is to use `gv` to reselect the same lines and use `>` again. However, the dot command is a more elegant approach:

| Keystrokes | Buffer Contents                                                                                                                                                                                                                                    |
| ---------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| {start}    | def fib(n):<br/>&nbsp;&nbsp;&nbsp;&nbsp;a, b = 0, 1<br/>&nbsp;&nbsp;&nbsp;&nbsp;while a < n;<br/><ins>p</ins>rint a,<br/>a, b = b, a+b<br/>fib(42)                                                                                                 |
| `Vj`       | def fib(n):<br/>&nbsp;&nbsp;&nbsp;&nbsp;a, b = 0, 1<br/>&nbsp;&nbsp;&nbsp;&nbsp;while a < n;<br/><b>print a,<br/><ins>a</ins>, b = b, a+b</b><br/>fib(42)                                                                                          |
| `>.`       | def fib(n):<br/>&nbsp;&nbsp;&nbsp;&nbsp;a, b = 0, 1<br/>&nbsp;&nbsp;&nbsp;&nbsp;while a < n;<br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<ins>p</ins>rint a,<br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;a, b = b, a+b<br/>fib(42) |

Another option would be to run `2>` from Visual mode to begin with, however the dot command provides instant visual feedback and granular control (hitting the dot command again will indent even further, `u` will undo a single indent instead of multiples).

When the dot command is used to repeat a Visual mode command it acts on the same amount of text as was marked by the most recent visual selection. This can be beneficial when operating line-wise visual selections but can have unexpected results when operating character-wise selections.

[▲ Return to Sections](#sections)

## Prefer Operators to Visual Commands Where Possible
_Visual mode's weakness is that it doesn't always play well with the dot command. It is a good idea to use Normal mode operators when appropriate._

Using the following as an example:

**[visual_mode/list-of-links.html](../code/visual_mode/list-of-links.html)**
```html
<a href="#">one</a>
<a href="#">two</a>
<a href="#">three</a>
```

Suppose the challenge is to uppercase the text inside the anchor tags. The inner contents of a tag could be selected with `vit` which can be read as: _visually_ select _inside_ the _tag_.

#### Using a Visual Operator

In Visual mode a selection is made to operate on. In this case the `U` command could be used to convert selected characters to uppercase (`:h v_U`).

| Keystrokes | Buffer Contents                                                                                                               |
| ---------- | ----------------------------------------------------------------------------------------------------------------------------- |
| {start}    | <ins>&lt;</ins>a href="#"&gt;one&lt;/a&gt;<br />&lt;a href="#"&gt;two&lt;/a&gt;<br />&lt;a href="#"&gt;three&lt;/a&gt;        |
| `vit`      | &lt;a href="#"&gt;<b>on<ins>e</ins></b>&lt;/a&gt;<br />&lt;a href="#"&gt;two&lt;/a&gt;<br />&lt;a href="#"&gt;three&lt;/a&gt; |
| `U`        | &lt;a href="#"&gt;<ins>O</ins>NE&lt;/a&gt;<br />&lt;a href="#"&gt;two&lt;/a&gt;<br />&lt;a href="#"&gt;three&lt;/a&gt;        |


After having transformed the first line, the dot command could be used to attempt to transform the next two lines. Running `j.` advances the cursors to the next line and then repeats the last change. After doing this for each line the text reads:

```html
<a href="#">ONE</a>
<a href="#">TWO</a>
<a href="#">THRee</a>
```

What happens is that when a Visual mode command is repeated, it affects the same range of text (see `:h visual-repeat`). The command is only repeated for a three-lettered word in the above example.

#### Using a Normal Operator

The Visual mode command `U` has a Normal mode equivalent: `gU{motion}` (`:h gU`). Using this command in Normal mode the subsequent changes can be made using the dot command:

| Keystrokes | Buffer Contents                                                                                                               |
| ---------- | ----------------------------------------------------------------------------------------------------------------------------- |
| {start}    | <ins>&lt;</ins>a href="#"&gt;one&lt;/a&gt;<br />&lt;a href="#"&gt;two&lt;/a&gt;<br />&lt;a href="#"&gt;three&lt;/a&gt;        |
| `gUit`     | &lt;a href="#"&gt;<ins>O</ins>NE&lt;/a&gt;<br />&lt;a href="#"&gt;two&lt;/a&gt;<br />&lt;a href="#"&gt;three&lt;/a&gt;        |
| `j.`       | &lt;a href="#"&gt;ONE&lt;/a&gt;<br />&lt;a href="#"&gt;<ins>T</ins>WO&lt;/a&gt;<br />&lt;a href="#"&gt;three&lt;/a&gt;        |
| `j.`       | &lt;a href="#"&gt;ONE&lt;/a&gt;<br />&lt;a href="#"&gt;TWO&lt;/a&gt;<br />&lt;a href="#"&gt;<ins>T</ins>HREE&lt;/a&gt;        |

The underlying semantics of `vitU` and `gUit` are different. The four keystrokes of `vitU` are considered two separate commands: `vit` to make the selection and `U` to transform the selection. `gUit`, on the other hand, can be considered a single command comprised of an opeartor (`gU`) followed by a motion (`it`).

In order to set up the dot command so that it repeats a useful operation it is better to stay out of Visual mode. **Prefer operator commands over their Visual mode equivalents when working through a repetitive set of changes**. Visual mode is perfectly adequate for one-off changes and for modifying a range of text whose structure is difficult to trace.

[▲ Return to Sections](#sections)

## Edit Tablular Data with Visual-Block Mode

Vim provides the capability to edit columns of text through its Visual-Block mode. Using the following as example:

**[visual_mode/chapter-table.txt](../code/visual_mode/chapter-table.txt)**
<pre lang="text">
Chapter            Page
Normal mode          15
Insert mode          31
Visual mode          44
</pre>

Visual-Block mode can be used to perform the following operations on this text:
1. Reduce the spacing between the two columns.
2. Separate the two columns of text with a pipe character to make it look more like a table.

The steps to accomplish this are:

| Keystrokes   | Buffer Contents                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| ------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| {start}      | Chapter&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<ins>&nbsp;&nbsp;</ins>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Page<br />Normal mode&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;15<br />Insert mode&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;31<br />Visual mode&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;44 |
| `<C-v>3j`    | Chapter&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&#9617;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Page<br />Normal mode&nbsp;&nbsp;&nbsp;&#9617;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;15<br />Insert mode&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&#9617;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;31<br />Visual mode&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<ins>&nbsp;&nbsp;</ins>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;44                                  |
| `x...`       | Chapter&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<ins>&nbsp;&nbsp;</ins>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Page<br />Normal mode&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;15<br />Insert mode&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;31<br />Visual mode&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;44                                                                                                                         |
| `gv`         | Chapter&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&#9617;&nbsp;&nbsp;&nbsp;&nbsp;Page<br />Normal mode&nbsp;&nbsp;&nbsp;&nbsp;&#9617;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;15<br />Insert mode&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&#9617;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;31<br />Visual mode&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<ins>&nbsp;&nbsp;</ins>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;44                                                                                                                                                          |
| `r\|`         | Chapter&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<ins>&nbsp;\|&nbsp;</ins>&nbsp;&nbsp;&nbsp;Page<br />Normal mode&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&#124;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;15<br />Insert mode&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&#124;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;31<br />Visual mode&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&#124;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;44                                                                                                                                                    |
| `yyp`        | Chapter&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&#124;&nbsp;&nbsp;&nbsp;&nbsp;Page<br /><ins>C</ins>hapter&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&#124;&nbsp;&nbsp;&nbsp;&nbsp;Page<br />Normal mode&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&#124;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;15<br />Insert mode&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&#124;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;31<br />Visual mode&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&#124;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;44              |
| `Vr-`        | Chapter&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&#124;&nbsp;&nbsp;&nbsp;&nbsp;Page<br /><ins>-</ins>--------------------------------<br />Normal mode&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&#124;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;15<br />Insert mode&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&#124;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;31<br />Visual mode&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&#124;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;44                                                                                                          |

`<C-v>` is used to engage Visual-Block mode followed by the motion `3j` to move the selection down the column 3 lines. `x` then deletes the selected column and the dot command repeats that operation. This is repeated until the spacing is not so excessive.

Rather than using the dot command, expanding the column selection by moving the cursor two or three characters to the right could have also worked. Operating one column at a time has the advantage of showing each change, however.

The `gv` command is then used to reselect the last visual selection and `r\|` will replace each character of the selection with the pipe character. `yyp` is then used to duplicate the selected line and `Vr-` is used to replace every character in the line with a dash.

[▲ Return to Sections](#sections)

## Change Columns of Text
_Visual-Block mode can be used to insert text into several lines simultaneously._

Using the following as example:

**[visual_mode/sprite.css](../code/visual_mode/sprite.css)**
<pre lang="css">
li.one   a{ background-image: url('/images/sprite.png'); }
li.two   a{ background-image: url('/images/sprite.png'); }
li.three a{ background-image: url('/images/sprite.png'); }
</pre>

Supposing that the `sprite.png` file is moved from the `/images` directory to a `/components` directory. This operation can be accomplished using Visual-Block mode by selecting the columns of text to change and then entering Insert mode to change the text. While in Insert mode the text updates will only appear in the top-left most area of the selection, but will update all lines upon pressing `<Esc>` and returning to Normal mode:

| Keystrokes                          | Buffer Contents                                                                                                                                                                                       |
| ----------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| {start}<br /><br />_Normal mode_    | li.one&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;a{ background-image: url('/<ins>i</ins>mages/sprite.png'); }<br />li.two&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;a{ background-image: url('/images/sprite.png'); }<br />li.three&nbsp;&nbsp;a{ background-image: url('/images/sprite.png'); } |
| `<C-v>jje`<br /><br />_Visual mode_ | li.one&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;a{ background-image: url('/<b>images</b>/sprite.png'); }<br />li.two&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;a{ background-image: url('/<b>images</b>/sprite.png'); }<br />li.three&nbsp;&nbsp;a{ background-image: url('/<b>image<ins>s</ins></b>/sprite.png'); } |
| `c`<br /><br />_Insert mode_ | li.one&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;a{ background-image: url('/<ins>/</ins>sprite.png'); }<br />li.two&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;a{ background-image: url('//sprite.png'); }<br />li.three&nbsp;&nbsp;a{ background-image: url('//sprite.png'); } |
| components<br /><br />_Insert mode_ | li.one&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;a{ background-image: url('/components<ins>/</ins>sprite.png'); }<br />li.two&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;a{ background-image: url('//sprite.png'); }<br />li.three&nbsp;&nbsp;a{ background-image: url('//sprite.png'); } |
| `<Esc>`<br /><br />_Normal mode_ | li.one&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;a{ background-image: url('/components<ins>/</ins>sprite.png'); }<br />li.two&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;a{ background-image: url('/components/sprite.png'); }<br />li.three&nbsp;&nbsp;a{ background-image: url('/components/sprite.png'); } |

[▲ Return to Sections](#sections)

| [Previous: 03 - Insert Mode](../03/README.md) | [Table of Contents](../README.md#table-of-contents) |
