# Smash into Vim - WIP
**Author**: [Chi-chi Wang](https://github.com/chichiwang)

These notes follow along with the [PluralSight Course](https://app.pluralsight.com/library/courses/smash-into-vim/table-of-contents) of the same title.

Notes completed: ongoing

## Table of Contents
* [Vim](#vim)
  * [Common Conventions](#common-conventions)
* [Basic Operations](#basic-operations)
* [Editing](#editing)
* [Configuration](#configuration)
  * [Mapping Keys](#mapping-keys)
  * [Mapping Codes](#mapping-codes)
  * [Map Leader](#map-leader)
  * [Abbreviations](#abbreviations)
* [Files and Directories](#files-and-directories)
* [Buffers](#buffers)
* [Windows](#windows)
* [Tabs](#tabs)
* [Folding](#folding)

## Vim
The [vim](https://en.wikipedia.org/wiki/Vim_(text_editor)) text editor was specifically built to help programmers edit text more efficiently, because of its origins in the terminal. It can be daunting, but it is not as hard as it looks.

The benefits of learning vim include:
* It is available on every platform
* It is highly configurable
* It is 100% keyboard-driven
* It is extendable with plugins
* It allows for a high degree of precision in text editing
* It provides powerful editing capabilities
* It has great documentation
* It allows editing of files both locally and remotely

Some drawbacks of vim include:
* Many defaults are undesirable
* It has a clunky scripting language
  * Places an upper bound on functionalities that can be scripted
* Destructive tasks are too easy to execute
  * Such as overwriting the paste buffer
* It default regular expression syntax is inconsistent

Vim was created in 1991 by [Bram Moolenaar](https://en.wikipedia.org/wiki/Bram_Moolenaar) as an improvement over [Bill Joy's](https://en.wikipedia.org/wiki/Bill_Joy) [vi](https://en.wikipedia.org/wiki/Vi) (created in 1976). The name vim is a contraction of **Vi IMproved**.

While it looks complex, Vim is built on two simple ideas:
1. Modal Editing
2. Operators

Vim's primary philosophy is centered around editing text efficiently. The goal of vim is to make navigation and editing as easy as, or even easier than, typing the text the first time. Most software prioritizes text input over editing. In the spirit of efficiency, the operations most frequently performed are available on the home row of the keyboard.

What a particular key does in vim depends on the mode. The letter `j` for example has three different functions depending on the mode:
* Command Mode: Move the cursor down one line
* Insert Mode: Add the letter 'j' to the text at the cursor
* Visual Mode: Move selection down one full line

There are five main modes in vim:
* Normal (Command)
* Insert
* Visual
* Replace
* Command-Line

There are anumber of different command types, including:
* Operator
* Count
* Motion

In the example `d2w`, `d` is the operator command (delete), `2` is the count command, and `w` is the motion command (word). This chain of commands will delete 2 words from the cursor position.

To get help on any command or topic use `:h search-term`, such as `:h movement`. To exit help, type `:bd` for _buffer delete_.

### Common Conventions
Uppercased commands are often super-sized versions of their lower-case counterparts. Some examples include:
* `i` inserts text at the cursor
* `I` inserts text at the beginning of the line that the cursor is sitting on
* `w` moves the cursor foward by one word
* `W` treats continguous code as one word and jumps forward to the next meaningful code word

Adding `!` to the end of a command will force it to happen without confirmation. To quit vim, `:q` is used. But vim won't quit if there are unsaved changes in the file. `:q!` will force vim to exit without saving.

[▲ Return to Table of Contents](#table-of-contents)

## Basic Operations
### Navigation
* By character:
  * `h`: Move cursor left 1 character
  * `j`: Move cursor down 1 line at the same column
  * `gj`: Move cursor down 1 line at the same column
  * `+` : Move cursor to the first character of the next line
  * `k`: Move cursor up 1 line at the same column
  * `gk`: Move cursor up 1 line at the same column
  * `-`: Move cursor to the first character of the previous line
  * `l`: Move cursor right 1 character
  * `6l`: Move cursor right 6 characters
  * `2j`: Move cursor down 2 lines at the same column
* By line number:
  * `42gg`: Move cursor to line 42
  * `12G`: Move cursor to line 12
  * `:12`: Move cursor to line 12
* By file:
  * `gg`: Move cursor to the start of the first line in the file
  * `go`: Move cursor to the start of the first line in the file
  * `G`: Move cursor to the start of the last line in the file
* By screen:
  * `H`: Move cursor to the start of the line at the top of screen
  * `M`: Move cursor to the start of the line at the middle of screen
  * `L`: Move cursor to the start of the line at the bottom of screen
* By word:
  * `b`: Move cursor back 1 word
  * `B`: Move cursor back one code word
  * `w`: Move cursor forward 1 word
  * `W`: Move cursor forward 1 code word
  * `3w`: Move cursor forward 3 words
  * `e`: Move cursor to the end of word
  * `e`: Move cursor to the end of code word
* By character on current line:
  * `fN`: Move cursor to the next 'N' on the line
  * `FN`: Move cursor to the previous 'N' on the line
  * `;`: Repeat last cursor jump with `f`/`F`
  * `,`: Reverse of last cursor jump with `f`/`F`
  * `3fN`: Move cursor to the third 'N' on the line
  * `%`: Move cursor to matching parentheses, quotes, or language-specific blocks
* By position on current line:
  * `0`: Move cursor to the start of the line
  * `^`: Move cursor to the first word character on the line
  * `$`: Move cursor to the end of the line
  * `g0`: Move cursor to the start of the line
  * `g^`: Move cursor to the first word character on the line
  * `gm`: Move cursor to the middle of the screen on the line
  * `g$`: Move cursor to the end of the line
* By pattern matching:
  * `/public`: Search against the regex "public" and places the cursor at the start of the first match
    * `n` will place the cursor at the start of the next match
    * `N` will place the cursor at the start of the previous match

### Marks
* Setting and navigating marks:
  * `m[letter]`: Create a mark at the cursor using a letter (ex: `ma`, `mb`, etc.)
    * Uppercase marks are valid across files (ex: `mA`, `mB`, etc.)
    * Lowercase marks operate on a per-file basis (ex: `ma`, `mb`, etc.)
  * `` `[letter] ``: Move the cursor to the character marked with the letter provided (ex: `` `a ``, `` `b ``, etc.)
  * `'[letter]`: Move the cursor to the beginning of the line marked with the letter provided (ex: `'a`, `'b`, etc.)
  * `:marks`: Display a list of marks
* Operators with marks:
  * `d'[letter]`: Deletes everything from the cursor position to the start of the line marked with the letter provided (ex: `d't`)
* Built-in marks:
  * `` `0-`9 ``: The location of the cursor when vim was last exited
    * `` `0 `` being the most recent exit, `` `1 `` being the next most recent exit, and so forth
  * `''` : The position of the cursor before the latest jump (can also use double backticks)
  * `'.`: The location of the last edit (can also use `` `. ``)


### Scroll
* By cursor:
  * `zt`: Scroll screen so cursor is at the top of screen
  * `zz`: Scroll screen so cursor is in the middle of screen
  * `zb`: Scroll screen so cursor is at the bottom of screen
* By screen:
  * `Ctrl+b`: Scroll up one screen
  * `Ctrl+u`: Scroll up 1/2 screen
  * `Ctrl+d`: Scroll down 1/2 screen
  * `Ctrl+f`: Scroll down one screen
* By line:
  * `Ctrl+y`: Scroll up one line
  * `Ctrl+e`: Scroll down one line

### Text Selection
* Command Mode:
  * `{`: Move cursor to the top of the paragraph under the cursor
  * `}`: Move cursor to the top of the paragraph after the paragraph under the cursor
  * `(`: Move to the top of the sentence under the cursor
  * `)`: Move to the top of the sentence after the sentence under the cursor
  * `v`: Visual Mode (select characters)
  * `V`: Visual Mode (select lines)
  * `gv`: Return to the last Visual Mode selection state
* Visual Mode:
  * `o`: While in Visual Mode, moves the cursor from one end of the selection to the other
* Search:
  * `/searchterm`: Search for a term
    * `n`: Next match
    * `N`: Previous match

### File Operations
* `e`: Re-open the current file
* `e!`: Discard changes and re-open the current file
* `:e path/to/file`: Open a file, relative to vim's working direcotry
* `:e path/to/dir`: Open a directory, relative to vim's working directory
  * Navigate with `h`, `j`, `k`, `l` and press `Enter` to open the file under the cursor
* `:w`: Write the current file (save)
* `:w!`: Overwrite the current file without confirmation
* `:q`: Quit the current file
* `:q!`: Discard changes and quit the current file
* `:wq!`: Write and quit file without confirmation
* `:w !sudo tee %`: Write to the sudo command with the current file
  * Useful for writing to protected files
  * `:w !cmd` means "write the current buffer _piped_ through command"
  * `%` is the _filename_ associated with the buffer
* `Ctrl+g`: Display in status line: current file, current cursor location

### Editor Operations
* Directory:
  * `:cd /path/to/dir`: Change vim's working directory
  * `:pwd`: Print out vim's working direcotry
* Format:
  * `:retab`: Replace all tabs with spaces in the current buffer

### Buffer Operations
* `:buffers`: List current buffers
 * `:ls` and `:files` will do the same thing
* `:buffer`: Switch to a buffer given the name or number of the buffer
 * `:b`, `:bu`, or `:buf` will do the same thing
* `:bp`: Go to the previous buffer
* `:bn`: Go to the next buffer

### Opening Files
* `vim -N path/to/file`: Opens the file with no vi compatibility
  * Opening vim in compatible mode means all enhancemeents and improvements of vim over vi are turned off. If a personal configuration file `~/.vimrc` exists, vim automatically turns on `nocompatible` mode. `:help comapatible` will provide more details.

[▲ Return to Table of Contents](#table-of-contents)

## Editing
The grammar of a vim editing command goes: `[operator][extent][object]`:
* `[operator]`: The command operator
  * `c`: Change
  * `d`: Delete
  * `y`: Yank
  * `v`: Visual
* `[extent]`: The object extent
  * `a`: All delimiters
  * `i`: Inner object
  * `[number]`: Number of objects
* `[object]`: The command object
  * `w`: Word
  * `W`: Code word
  * `s`: Sentence
  * `p`: Paragraph
  * `t`: Tag
  * `"`, `'`, `[`, `{`, `(`

### Editing Commands
* Insert text:
  * `i`: Insert text before the cursor
  * `I`: Insert text at the first non-blank character of the line
  * `a`: Insert text after the cursor
  * `A`: Insert text at the end of the line
* Insert line:
  * `o`: Insert new line below the cursor
  * `O`: Insert new line above the cursor
* Copy/paste:
  * `yy`: Yank line (copy)
    * `Y` also works
  * `p`: Paste below cursor
  * `P`: Paste above cursor
* Replace text by cursor:
  * `cc`: Change the line under the cursor
    * `C` also works
  * `cw`: Change word from the cursor position to end of word
  * `c3w`: Change the next 3 words from the cursor position.
  * `ciw`: Change the entire word under the cursor
  * `caw`: Change the entire word and surrounding spaces under the cursor
  * `r`: Replace the character under the cursor with another character
  * `rp`: Replace the character under the cursor with "p"
  * `R`: Replace multiple characters starting at the cursor position
    * `Backspace` restores the original text
  * `3cw`: Change 3 words
  * `J`: Joins the line under the cursor with the next line
    * Removes the indent and inserts up to two spaces (not good for code)
  * `5J`: Joins the line under the cursor with the next four lines
  * `gJ`: Joins line line under the cursor with the next line
    * Does not insert or remove any spaces
* Remove text by cursor:
  * `x`: Delete the character under the cursor
  * `dd`: Delete the line under the cursor
  * `dw`: Delete word from the cursor position to the end of word
  * `diw`: Delete the entire word under the cursor
  * `daw`: Delete the entire word and surrounding spaces under the cursor
* Modify line:
  * `<<`: Outdent line of text under the cursor in Command Mode
  * `>>`: Indent line of text under the cursor in Command Mode
  * `Ctrl+d`: Outdent line of text under the cursor in Insert Mode
  * `Ctrl+t`: Indent line of text under the cursor in Insert Mode
* Undo/redo/repeat:
  * `u`: Undo
  * `Ctrl+r`: Redo
  * `.`: Repeat the last change
    * Does not repeat commands in Command-Line Mode prefixed with `:`
* Search/replace by pattern:
  * `%s/search/replace/gc` Search and replace "search" in the document with "replace"
    * `%`: Search the current buffer
    * `s`: Substitute
    * `g`: Global/greedy - will replace all occurrences on a line, not just one per line
    * `c`: Confirm each match

[▲ Return to Table of Contents](#table-of-contents)

## Configuration
Some of vim's default values are less than ideal, so it is worth knowing how to change them by editing a few files.

Important configuration files:
* `~/.vimrc`: Terminal and graphical settings for a single user
* `~/.gvimrc`: Graphic-only settings (font, window-size) for a single user
* `~/.vim/`: Plugins, language-specific options, color schemese for a single user

Conventions for setting, showing, and resetting configurations:
* `set list`: Turn a boolean value on
* `set nolist`: Turn a boolean value off
* `set softtabstop=2`: Set a numerical/string value
* `set softtabstop`: Show a numerical/string value
* `set list?`: Show current value
* `set list&`: Reset to default value
* `verbose set wildmode?`: Show a current value as well as which file set it
  * Last set message will appear unless the option has not been set or was set by manually

Almost every option name has a short form (`softtabstop` can be abbreviated to `sts`). Typing `:options` will bring up vim's options screen where all available options are listed, grouped by area. Typing `:set` on its own will list all options that differ from their default values.

Vim configuration files are simple sequences of commands just as would be run in a live session, but with the `:` omitted. Vim reads the configuration file when it starts up.

To configure vim on a per-document basis, use [modelines](https://vim.fandom.com/wiki/Modeline_magic). Modelines may have to be enabled in a vim configuration file. Modelines must appear at the top or bottom of a file (the number of lines by which modelines can appear is set using the configuration `modelines`).

Vim can also detect filetypes and apply custom syntax highlighting and configurations to different filetypes. For more details use `:help filetypes`.

### Configuration Options
* Editor:
  * `set laststatus=2`: Always show the status bar
  * `set ruler`: Show the cursor position in the status bar
  * `set number`: Show line numbers
  * `set foldcolumn=1`: Set the width of the fold column to 1
* Syntax:
  * `syntax enable`: Turn on syntax highlighting
  * `set syntax=filetype`: To allow better syntax highlighting, specify the filetype
    * `setfiletype ` + `Ctrl+d` to list out possible filetypes
* Search:
  * `set hlsearch`: Highlight search terms
  * `set incsearch`: Highlight matches as patterns are typed
  * `set nohlsearch`: Turn off search highlighting
  * `set ignorecase`: Search patterns automatically ignore letter casing
  * `set smartcase`: If search pattern contains uppercase letters, casing is respected
    * Use in combination with `set ignorecase`
* Format
  * `set tabstop=2`: Set the global tab width to 2
  * `set shiftwidth=2`: Set width of indentation to 2
  * `set expandtab`: Set editor to use spaces instead of tabs

### Mapping Keys
* `imap`: Bind a key in Insert Mode
* `iunmap`: Unbind a key in Insert Mode
* `nmap`: Bind a key in Normal Mode (Command Mode)
* `nunmap`: Unbind a key in Normal Mode (Command Mode)
* `vmap`: Bind a key in Visual Mode
* `vunmap`: Unbind a key in Visual Mode
* `map`: Bind a key in Normal, Visual, and operating-pending modes
* `unmap`: Unbind a key in Normal, Visual, and operating-pending modes
* `map!`: Bind a key in Command and Insert modes
* `unmap!`: Unbind a key in Command and Insert modes
* `nnoremap`: Safely swap characters in normal mode
  * Will not allow rebinding of the bound key recursively

In Command-Line Mode, enter `imap`, `nmap`, `vmap`, or `map` to see a list of current mappings.

### Mapping Codes
* `<Tab>`: Tab key
* `<C-n>`: Ctrl + n
* `<S-n>`: Shift + n
* `<leader>`: Leader key (`\` by default)
* `<cr>`: Carriage Return

### Map Leader
The map leader (default `\`) is a character whose sole purpose is to prevent custom mappings from conflicting with with vim's commands. To set the map leader: `let mapleader=","` to set it to `,` or `let mapleader=" "` to set it to the space bar.

### Abbreviations
To declare an abbreviation for long words that are typed often use `ab`. For example, to replace "ff" with "Firefox" in Insert Mode: `iab ff Firefox`. Vim applies abbreviates when `Space`, `Enter`, or `Escaped` is pressed, or when encountering punctuation.

* `ab`: Declare an abbreviation for Insert Mode, Replace Mode, and Command-Line Mode
* `iab`: Declare an abbreviation for Insert Mode

[▲ Return to Table of Contents](#table-of-contents)

## Files and Directories
Most tasks involve working with several files. Programming tasks may involve working with dozens, or hundreds, of files. Vim has very robust file finding/opening capabilities, as well as window arrangement capabilities. There are different ways to look for files: visual hierarchical views, textual lists, and everything in between. While vim's own file finding is very basic, there are many plugins available for file finding spread out across the visual/textual spectrum.

Vim always has a current working directory. The `:pwd` command will display the current working directory, and it can be changed with `:cd path/to/directory`. `:set autochdir` can be used to automatically use the current file's directory as the working directory.

`:e` can be used to open files. Tab auto-completion for filepaths using this command can be enabled by setting: `:set wildmode=list:longest`. Entering a directory into the `:e` command (ex: `:e ~`) will open a directory tree listing all contents of a directory that can be navigated.

Directory Listing commands:
* `i`: Thin, long, wide, or tree listings
* `s`: Sort by name, time, or file size
* `r`: Reverse sort order
* `gh`: Hide/unhide dotfiles
* `<Enter>`: Open the file/directory
* `x`: View file with associated application
* `d`: Make directory
* `D` Delete the file/directory
* `R`: Rename file/directory
* `-`: Go up a directory

When traversing a file, `gf` will jump to a file under the cursor. It is smart enough to recognize filepaths in a number of different filetypes.

File-Finding Plugins:
* [NERDTree](https://github.com/preservim/nerdtree): File system tree
* [Project](https://github.com/vimplugin/project.vim): User-configured tree
* [LustyExplorer](https://github.com/vim-scripts/LustyExplorer): Quick folder navigation
* [FuzzyFinder](https://github.com/vim-scripts/FuzzyFinder): Flexible search
* [PeepOpen](https://github.com/shemerey/vim-peepopen): Mac OS X GUI, Git metadata

[▲ Return to Table of Contents](#table-of-contents)

## Buffers
A _buffer_ in vim is the contents of a file in memory. There may be any number of buffers in memory. Vim's buffer navigation is passable, but a plugin can help make buffer management much simpler. Buffer numbers will not change while vim is open.

### Buffer Management
* `:ls`: List all buffers
* `:buffers`: List all buffers
* `:b3`: Open buffer number 3
* `:bn`: Open next buffer
* `:bp`: Open previous buffer
* `:bd`: Delete buffer (close file)
* `:bf`: Open the first buffer
* `:bl`: Open the last buffer
* `:ba`: Open a window for every buffer
* `Ctrl+^`: Edit the alternate file (generally the last file edited)

When listing out buffers, the first column is the buffer number. The second column shows the buffer's status with various flags:
* `+`: Buffer with unsaved changes
* `=`: Read-only buffer
* `%`: Buffer in the current window

Using the [Buffer Explorer](https://github.com/jlanzarotta/bufexplorer) plugin, `<leader>be` will open a buffer management window that can be navigated to manage buffers. Buffers in this window are sorted by most recently edited. Press `Enter` to open a buffer, `d` to close a buffer. Close the Buffer Explorer window with `q`.

[▲ Return to Table of Contents](#table-of-contents)

## Windows
A _window_ in vim is a rectangular view of a buffer. There may be any number of windows onto a buffer. Windows can be arranged horizontally and vertically in any number of configurations, on a tab-by-tab basis.

### Window Management
* Split:
  * `Ctrl+w s`: Split window horizontally
  * `Ctrl+w v`: Split window vertically
* Move focus:
  * `Ctrl+w w`: Cycle focus counter-clockwise
  * `Ctrl+w W`: Cycle focus clockwise
  * `Ctrl+w p`: Focus previous window
  * `Ctrl+w t`: Move focus to top-left window
  * `Ctrl+w b`: Move focus to bottom-right window
  * `Ctrl+w h`: Move focus left
  * `Ctrl+w j`: Move focus down
  * `Ctrl+w k`: Move focus up
  * `Ctrl+w l`: Move focus right
* Move buffer:
  * `Ctrl+w H`: Move buffer left one window
  * `Ctrl+w J`: Move buffer down one window
  * `Ctrl+w K`: Move buffer up one window
  * `Ctrl+w L`: Move buffer right one window
  * `Ctrl+w r`: Rotate windows counter-clockwise
  * `Ctrl+w R`: Rotate windows clockwise
  * `Ctrl+w x`: Exchange the currently focused window with the next window
* Window size:
  * `Ctrl+w +`: Increase window height by N (default 1)
  * `Ctrl+w -`: Decrease window height by N (default 1)
  * `Ctrl+w >`: Increase window width by N (default 1)
  * `Ctrl+w <`: Decrease window width by N (default 1)
* Close and tabs:
  * `Ctrl+w c`: Close currently focused window
  * `Ctrl+w o`: Close all but the currently focused window
  * `Ctrl+w T`: Move the currently focused window to a new tab

Use `:h window-resize` to view documentation on resizing windows.

[▲ Return to Table of Contents](#table-of-contents)

## Tabs
A _tab_ in vim holds one more more windows. Most commands work on a per-tab basis.

### Tab Management
* `:tabnew`: Create new tab
* `:tabedit path/to/file`: Open file in new tab
* `:tabclose`: Close the currently focused tab
* `:tabonly`: Close all but the currently focused tab
* `:tabnext`: Focus the next tab
* `:tabprevious`: Focus the previous tab
* `:tabfirst`: Focus the first tab
* `:tablast`: Focus the last tab
* `:tabmove -2` Move the tab 2 to the left

[▲ Return to Table of Contents](#table-of-contents)

## Folding
[Folding](https://vim.fandom.com/wiki/Folding) allows the collapsing of long ranges of text into a single line, only displaying the first line of the fold. Many programming languages have a syntax file that supports folding.

Vim offers several fold strategies defined by `foldmethod`. By default it is `manual`:
`:set foldmethod=`
* `indent`: Use spaces or tabs to find foldable blocks
* `syntax`: Fold on language features(methods, classes)
* `marker`: Fold on textual marks
* `diff`: Fold unchanged text
* `expr`: Custom, code-driven folding
* `manual`: Select ranges to fold

Vim does not have a lexer or parser, so there will be corner cases when folding by `indent` or `syntax` where the vim does not get the fold right. Folding manually can be tiresome, and polluting a text with markers should generally be avoided, so it is recommended that folds be performed by `syntax` or `indent`.

### Fold Operations
* `zk`: Move cursor up to the previous fold
* `zj`: Move cursor down to the next fold
* `zM`: Close all folds
* `zc`: Close fold under the cursor
* `za`: Toggle fold open/close under the cursor
* `zo`: Open fold under the cursor
* `zR`: Open all folds
* `zi`: Toggle folding on/off altogether
* `zx`: Close all folds except the fold under the cursor

[▲ Return to Table of Contents](#table-of-contents)
