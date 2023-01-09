# Visual Mode
Vim's Visual mode allows a selection of text to be defined and then operated upon. There are three variants of visual mode involving working with characters, lines, or rectangular blocks of text.

The dot command can be used to repeat Visual mode commands - it is especially effective when working on line-wise regions but can fall short of expectations when used with character-wise selections.

Visual-Block mode allows operations on rectangular columns of text. There are many uses for this feature.

## Sections
* [Grok Visual Mode](#grok-visual-mode)
* [Meet Select Mode](#meet-select-mode)

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

| [Previous: 03 - Insert Mode](../03/README.md) | [Table of Contents](../README.md#table-of-contents) |
