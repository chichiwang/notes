# Insert Mode
Although many operations are triggered from [Normal mode](../02/README.md) (delete, yank, put) some functionality is within easy reach from Insert mode. There is a convenient shortcut for pasting text from a register without leaving Insert mode, there are two easy ways for inserting unusual characters that are not represented on the keyboard.

Replace mode is a special case for Insert mode which overwrites existing characters in the document. There is also Insert Normal mode, a submode that allows the firing of a single Normal mode command before returning to Insert mode.

## Sections
* [Make Corrections Instantly from Normal Mode](#make-corrections-instantly-from-normal-mode)
* [Get Back to Normal Mode](#get-back-to-normal-mode)

## Make Corrections Instantly from Normal Mode
_Besides using the backspace key there are a couple of Insert mode commands to make corrections immediately without exiting Insert mode._

When making a typing error, a common method of correction is to use the backspace key to correct the error. As long as the error is at the end of the word just typed this is the quickest strategy for correction. When a mistake is made at the beginning of a word deleting the entire words and typing it out again can be quick as long as the typist is fast.

Another option in Vim is to switch to Normal mode, navigate to the start of the word, fix the error, then hit `A` to continue typing on the same line. This context switching can be cognatively awkward. In Insert mode the following chords are also available for corrections:

| Keystrokes | Effect                                |
| ---------- | ------------------------------------- |
| `<C-h>`    | Delete back one character (backspace) |
| `<C-w>`    | Delete back one word                  |
| `<C-u>`    | Delete back to the start of the line  |

These commands are not unique to Insert mode, nor to Vim itself. They can be used in Vim's command line as wel as in the bash shell.

[▲ Return to Sections](#sections)

## Get Back to Normal Mode
_Insert mode is specialized for a single task: entering text. Normal mode is where a Vim user will spend most of their time. As such it is important to be able to quickly switch between Normal and Insert mode. The following are a couple of tricks to reduce the friction of mode switching._

The classic method of getting back to Normal mode is to press `<Esc>`. `<C-]>` (see `:h i_CTRL-[`) can also be used to achieve the same effect.

| Keystrokes | Effect                       |
| -----------| ---------------------------- |
| `<Esc>`    | Switch to Normal mode        |
| `<C-[>`    | Switch to Normal mode        |
| `<C-o>`    | Switch to Insert Normal mode |

Vim novices frequently become fatigued by the constant need to switch modes but with practice it starts to feel natural. Vim provides a solution for one common scenario: the need to run a single Normal mode command while in Insert mode. This particular mode-switching friction can be resolved with _Insert Normal mode_.

#### Meet Insert Normal Mode
Insert Normal mode is a special version of Normal mode that allows the exection of one Normal mode action before returning to Insert mode. To switch to Insert Normal mode, from Insert mode press `<C-o>` (see `:h i_CTRL-O`).

One way to use this is to reposition the screen scroll while typing: `zz` will position the current line in the middle of the window. This can be triggered from Insert mode by pressing `<C-o>zz` without having to leave Insert mode allowing typing to continue uninterrupted.

[▲ Return to Sections](#sections)

| [Normal Mode](../02/README.md) | [Table of Contents](../README.md#table-of-contents) |
