# Visual Mode
Vim's Visual mode allows a selection of text to be defined and then operated upon. There are three variants of visual mode involving working with characters, lines, or rectangular blocks of text.

The dot command can be used to repeat Visual mode commands - it is especially effective when working on line-wise regions but can fall short of expectations when used with character-wise selections.

Visual-Block mode allows operations on rectangular columns of text. There are many uses for this feature.

## Sections
* [Grok Visual Mode](#grok-visual-mode)
* [Meet Select Mode](#meet-select-mode)
* [Define a Visual Selection](#define-a-visual-selection)

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

| [Previous: 03 - Insert Mode](../03/README.md) | [Table of Contents](../README.md#table-of-contents) |
