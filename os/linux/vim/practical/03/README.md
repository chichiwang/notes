# Insert Mode
Although many operations are triggered from [Normal mode](../02/README.md) (delete, yank, put) some functionality is within easy reach from Insert mode. There is a convenient shortcut for pasting text from a register without leaving Insert mode, there are two easy ways for inserting unusual characters that are not represented on the keyboard.

Replace mode is a special case for Insert mode which overwrites existing characters in the document. There is also Insert Normal mode, a submode that allows the firing of a single Normal mode command before returning to Insert mode.

## Sections
* [Make Corrections Instantly from Normal Mode](#make-corrections-instantly-from-normal-mode)
* [Get Back to Normal Mode](#get-back-to-normal-mode)
* [Paste from a Register Without Leaving Insert Mode](#paste-from-a-register-without-leaving-insert-mode)
* [Tip: Remap the Caps Lock Key](#tip-remap-the-caps-lock-key)
* [Do Back-of-the-Envelope Calculations in Place](#do-back-of-the-envelope-calculations-in-place)
* [Insert Unusual Characters by Character Code](#insert-unusual-characters-by-character-code)

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

## Paste from a Register Without Leaving Insert Mode
_Vim's yank and put operations are usually executed from Normal mode but sometimes it is desirable to paste text into the document without leaving Insert mode._

Using this example file with an unfinished excerpt of text:

**[insert_mode/practical-vim.txt](../code/insert_mode/practical-vim.txt)**
<pre lang="text">
Practical Vim, by Drew Neil
Read Drew Neil's
</pre>

The task is to complete the second line by inserting the title of the book. Since the text is already present in the first line, the operation will involve yanking it into a register and then appending it to the end of the next line in Insert mode:

| Keystrokes   | Buffer Contents                                                                 |
| ------------ | ------------------------------------------------------------------------------- |
| `yt,`        | <ins>P</ins>ractical Vim, by Drew Neil<br/>Read Drew Neil's                     |
| `jA`␣        | Practical Vim, by Drew Neil<br/>Read Drew Neil's <ins>&nbsp;</ins>              |
| `<C-r>0`     | Practical Vim, by Drew Neil<br/>Read Drew Neil's Practical Vim<ins>&nbsp;</ins> |
| .&lt;Esc&gt; | Practical Vim, by Drew Neil<br/>Read Drew Neil's Practical Vim<ins>.</ins>      |

The command `yt,` yanks the words "Practical Vim" into the yank register. In Insert mode `<C-r>0` will paste the yanked text at the current cursor position. The general format of the command is `<C-r>{register}` where {register} is the address of the register to be inserted (see `:h i_CTRL-R`).

#### Use <C-r>{register} for Character-wise Registers
The `<C-r>{register}` command is useful for pasting a few words in Insert mode. If the register contains a lot of text there will be a slight delay before the screen updates. This is because Vim inserts the text from the register as if it were being typed one character at a time. If the `textwidth` or `autoindent` options are enabled, the result may contain unwanted line breaks or extra indentation.

The `<C-r><C-p>{register}` command is a bit smarter: it inserts text plainly and fixes unintended indentation (see `:h i_CTRL-R_CTRL-P`). However, this is a more complicated action.

[▲ Return to Sections](#sections)

## Tip: Remap the Caps Lock Key
For Vim users the Caps Lock key is a nuisance. If Caps Lock is engaged, `k` and `j` will no longer navigate in Normal mode, but instead trigger the `K` and `J` commands (see `:h K` and `:h J`).

For reasons such as this many Vim users remap the Caps Lock key to another key such as &lt;Ctrl&gt; or &lt;Esc&gt;. The caps lock key is in a useful, easy to reach position and when using Vim the &lt;Ctrl&gt; and &lt;Esc&gt; keys are far more useful.

Remapping the Caps Lock key is done at the system level and the steps to do so will differ between operating systems. This change will impact the Caps Lock key system wide (however, caps lock is not a useful feature in the vast majority of cases).

[▲ Return to Sections](#sections)

## Do Back-of-the-Envelope Calculations in Place
_The expression register allows calculations to be performed and then the result to be directly inserted into the document. This section presents one possible application of this powerful feature._

Most of Vim's registers contain text either as a string of characters or as entire lines of text. The delete and yank commands set the contents of a register while the put command retrieves the contents of a register by inserting it into the document.

The expression register is different: it can evaluate Vim script and return the result. This section will demonstrate how to use it like a calculator, passing it an arithmetic expression and using the return value as though it were a string of text saved in a regular register.

The expression register is addressed by the = symbol. From Insert mode it can be accessed by pressing `<C-r>=`. This will open a prompt at the bottom of the screen where an expression can be entered to be evaluated. Entering an expression with `<CR>` will insert the result of the expression at the current position in the document.

Using the following example file:

**[insert_mode/back-of-evelope.txt](../code/insert_mode/back-of-envelope.txt)**
<pre lang="text">
6 chairs, each costing $35, total $
</pre>

| Keystrokes         | Buffer Contents                                                                      |
| ------------------ | ------------------------------------------------------------------------------------ |
| `A`                | 6 chairs, each costing <span>$</span>35, total <span>$</span><ins>&nbsp;</ins>       |
| `<C-r>=`6*35`<CR>` | 6 chairs, each costing <span>$</span>35, total <span>$</span>210<ins>&nbsp;</ins>    |

The expression register is capable of much more than simple arithmetic (as will be highlighted in a later section).

[▲ Return to Sections](#sections)

## Insert Unusual Characters by Character Code
_Vim can insert any character by its numeric code. This can be handy for enetering symbols that are not found on the keyboard._

`<C-v>{code}` can be used to insert any character by its numeric code where {code} is the address of the desired character. Vim expects the numeric code to consist of 3 digits so, for example, the character 'A' would be inserted by `<C-v>065`.

To insert a character whose numeric digit exceeds three digits (the Unicode Basic Mutilingual Plane has an address space for up to 65,535 characters) a four-digit hexadecimal code can be provided instead. The command `<C-v>u{code}` is used to do this. The _u_ preceeding the {code} is necessary in this instance.  For example: to insert an inverted question mark symbol ("¿") whose character code is 00bf enter `<C-v>u00bf`.

For more information see `:h i_CTRL-V_digit`.

To find the numeric code for any character in a document: place the cursor over the character and trigger the `ga` command. This outputs a message at the bottom of the screen revealing the character's code in decimal, hexadecimal, and octal notations (see `:h ga`). For the codes of characters not in a document, look up the unicode tables online.

If the `<C-v>` command is followed by a nondigit key, the character represented by the key will be inserted literally. For example: if the `expandtab` option is enabled then pressing `<Tab>` will insert space characters instead of a tab character. However, pressing `<C-v><Tab>` will always insert a tab character literally, regardless of the `expandtab` option.

| Keystrokes            | Effect                                                   |
| --------------------- | -------------------------------------------------------- |
| `<C-v>{123}`          | Insert character by 3 digit decimal code                 |
| `<C-v>u{1234}`        | Insert character by 4 digit hexadecimal code             |
| `<C-v>{nondigit}`     | Insert nondigit literally                                |
| `<C-k>{char1}{char2}` | Insert character represented by `{char1}{char2}` digraph |

[▲ Return to Sections](#sections)

| [Previous: 02 - Normal Mode](../02/README.md) | [Table of Contents](../README.md#table-of-contents) |
