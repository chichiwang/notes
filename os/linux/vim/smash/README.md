# Smash into Vim - WIP
**Author**: [Chi-chi Wang](https://github.com/chichiwang)

These notes follow along with the [PluralSight Course](https://app.pluralsight.com/library/courses/smash-into-vim/table-of-contents) of the same title.

Notes completed: ongoing

## Table of Contents
* [Vim](#vim)
  * [Common Conventions](#common-conventions)
* [Basic Operations](#basic-operations)
* [Configuration](#configuration)
* [Files and Directories](#files-and-directories)
* [Buffers](#buffers)
* [Windows](#windows)
* [Tabs](#tabs)

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
Navigation:
* `h`: Move cursor left 1 character
* `j`: Move cursor down 1 character
* `k`: Move cursor up 1 line
* `l`: Move cursor right 1 line
* `6l`: Move cursor right 6 characters
* `2j`: Move cursor down 2 lines
* `12G`: Go to line 12
* `gg`: Go to the top of the file
* `G`: Go to the end of the file
* `w`: Move cursor forward 1 word
* `3w`: Move cursor forward 3 words
* `fN`: Move cursor to the first 'N' on the line
* `3fN`: Move cursor to the third 'N' on the line
* `/public`: Search against the regex "public" and places the cursor at the start of the first match
  * `n` will place the cursor at the start of the next match
  * `N` will place the cursor at the start of the previous match

Editing:
* `i`: Insert text before the cursor
* `a`: Insert text after the cursor
* `yy`: Yank line (copy)
* `p`: Paste below cursor
* `P`: Paste above cursor
* `cw`: Change word from the cursor position to end of word
  * The removed word is placed into the paste register
* `ciw`: Change the entire word under the cursor
  * The removed word is placed into the paste register
* `caw`: Change the entire word and surrounding spaces under the cursor
  * The removed word is placed into the paste register
* `rp`: Replace the character under the cursor with "p"
* `3cw`: Change 3 words
  * The removed words are placed into the paste register
* `dw`: Delete word from the cursor position to the end of word
  * The removed word is placed into the paste register
* `diw`: Delete the entire word under the cursor
  * The removed word is placed into the paste register
* `daw`: Delete the entire word and surrounding spaces under the cursor
  * The removed word is placed into the paste register
* `u`: Undo
* `Ctrl+r`: Redo
* `%s/search/replace/gc` Search and replace "search" in the document with "replace"
  * `%`: Search the current buffer
  * `s`: Substitute
  * `g`: Global/greedy - will replace all occurrences on a line, not just one per line
  * `c`: Confirm each match
* `.`: Repeat the last change
  * Does not repeat commands in Command-Line Mode prefixed with `:`

Text Selection:
* `v`: Visual mode (select characters)
* `V`: Visual mode (select lines)
* `/searchterm`: Search for a term

File Operations:
* `:w`: Write file (save)
* `:w!`: Overwrite file without confirmation
* `:q`: Quit file
* `:wq!`: Write and quit file without confirmation
* `:e path/to/file`: Open a file, relative to vim's working direcotry
* `:e path/to/dir`: Open a directory, relative to vim's working directory
  * Navigate with `h`, `j`, `k`, `l` and press `Enter` to open the file under the cursor
* `:syntax enable`: Turn on syntax highlighting
* `:set syntax=filetype`: To allow better syntax highlighting, specify the filetype
* `:w !sudo tee %`: Write to the sudo command with the current file
  * Useful for writing to protected files
  * `:w !cmd` means "write the current buffer _piped_ through command"
  * `%` is the _filename_ associated with the buffer
* `:cd /path/to/dir`: Change vim's working directory
* `:pwd`: Print out vim's working direcotry
* `:buffers`: List current buffers
 * `:ls` and `:files` will do the same thing
* `:buffer`: Switch to a buffer given the name or number of the buffer
 * `:b`, `:bu`, or `:buf` will do the same thing
* `:bp`: Go to the previous buffer
* `:bn`: Go to the next buffer

Configuration:
* `set hlsearch`: Highlight search terms
* `set nohlsearch`: Turn off search highlighting
* `set incsearch`: Highlight matches as patterns are typed

Opening files:
* `vim -N path/to/file`: Opens the file with no vi compatibility
  * Opening vim in compatible mode means all enhancemeents and improvements of vim over vi are turned off. If a personal configuration file `~/.vimrc` exists, vim automatically turns on `nocompatible` mode. `:help comapatible` will provide more details.

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

Buffer Management:
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

Window management:
* `Ctrl+w s`: Split window horizontally
* `Ctrl+w v`: Split window vertically
* `Ctrl+w w`: Cycle focus counter-clockwise
* `Ctrl+w W`: Cycle focus clockwise
* `Ctrl+w p`: Focus previous window
* `Ctrl+w t`: Move focus to top-left window
* `Ctrl+w b`: Move focus to bottom-right window
* `Ctrl+w h`: Move focus left
* `Ctrl+w j`: Move focus down
* `Ctrl+w k`: Move focus up
* `Ctrl+w l`: Move focus right
* `Ctrl+w H`: Move buffer left one window
* `Ctrl+w J`: Move buffer down one window
* `Ctrl+w K`: Move buffer up one window
* `Ctrl+w L`: Move buffer right one window
* `Ctrl+w r`: Rotate windows counter-clockwise
* `Ctrl+w R`: Rotate windows clockwise
* `Ctrl+w x`: Exchange the currently focused window with the next window
* `Ctrl+w T`: Move the currently focused window to a new tab
* `Ctrl+w +`: Increase window height by N (default 1)
* `Ctrl+w -`: Decrease window height by N (default 1)
* `Ctrl+w >`: Increase window width by N (default 1)
* `Ctrl+w <`: Decrease window width by N (default 1)
* `Ctrl+w c`: Close currently focused window
* `Ctrl+w o`: Close all but the currently focused window

Use `:h window-resize` to view documentation on resizing windows.

[▲ Return to Table of Contents](#table-of-contents)

## Tabs
A _tab_ in vim holds one more more windows. Most commands work on a per-tab basis.

Tab management:
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
