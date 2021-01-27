# Linux Command Line Interface Fundamentals (WIP)
**Author**: [Chi-chi Wang](https://github.com/chichiwang)

An overview of the fundamentals of the Linux command line. These notes follow along with the [PluralSight Course](https://app.pluralsight.com/library/courses/linux-cli-fundamentals/table-of-contents) of the same title.

## Table of Contents
* [Working On The Command Line](#working-on-the-command-line)
  * [Physical Consoles](#physical-consoles)
  * [Accessing Virutal Consoles](#accessing-virtual-consoles)
  * [Pseudo Consoles](#pseudo-consoles)
  * [Shells](#shells)
  * [Bash History](#bash-history)
  * [Working With Text Files](#working-with-text-files)
    * [cat](#cat)
    * [tac](#tac)
* [Additional Resources](#additional-resources)

## Working On The Command Line

### Physical Consoles
In the early days of computing, companies used expensive mainframe computers. Multiple users could connect to a mainframe computer independently, each accessing it as a separate computer.

A device known as a terminal (a monitor with an integrated keyboard) were used to access the computer. The earliest terminals were known as *teletypes* (abbreviated to TTY). These terminals connected to a mainframe computer via a physical serial port.

Mainframe computers were eventually replaced by personal computers (PCs) that each ran their own operating system. The Linux OS have the necessary software configurations that allow physical terminals to connect to it, but they also have ways to access that software configuration virtually: this is known as a virtual console.

**Physical console** - A physical terminal device connected with Linux system on serial port via serial cable physically.

**Virtual console** - An application that simulates a physical terminal device in software and connects it with Linux system on serial port through software configuration virtually.

**Physical terminal device** - A physical device that consisted of nothing more than a monitor and keyboard attached to it. It uses all resources such as CPU, RAM and Hard disk from server system.

**Virtual terminal application** - An application that provides a text based environment to access the shell. Since physical terminals are no longer used, it became common practice to use the word terminal to refer the virtual terminal application.

### Accessing Virutal Consoles
From within Linux you can access physical consoles via the command: `CTRL+ALT+Fx` where `x` is the number of the console. Typically the tty (teletype) consoles on a Linux machine are 1-6. To access physical console 2 you would press `CTRL+ALT+F2`.

Once inside the terminal you can use the command `tty` to display the current terminal name. The command `who` will display the user who is logged on. In order to exit the terminal you can use the commands `exit`, `logout`, or the keys `CTRL+D`.

Often the graphical terminal (graphical environment) is `tty1`, so you can access it from a command line console by pressing `CTRL+ALT+F1`.

To change terminals from within a console you can use the command `chvt` followed by the tty number. For example, if you are on tty 2 and you want to switch to the tty 3, you would enter the command `chvt 3`.

Virtual (physical) consoles are represented by device files that are permanently resident on the machine.

### Pseudo Consoles
These consoles are the most common type of connection - often representing remote connections to a server. Pseudo consoles are created dynamically as a connection is made.

Pseudo terminals are named by the following convention: `/dev/psts/x` where `x` is a number.

More information about a console connection can be found by inspecting the `SSH_` variables. Using autocomplete on `$SSH_` will display the environment variables set up for a SSH connection.

Often these terminals represent remote connections (via [SSH](https://en.wikipedia.org/wiki/SSH_(Secure_Shell)) or [Telnet](https://en.wikipedia.org/wiki/Telnet)) as well as connections from the GUI (via XTerminal or Gnome Terminal).

**SSH vs Telnet**

`SSH` is encrypted, listens on port 22, and is 3rd party software (PuTTY).

`Telnet` is unencrypted, listens on port 23, and is first party packaged software.

### Shells
A *shell* is a program that takes commands from the keyboard and gives them to the operating system to perform. In the past it was the only interface available on a Unix-like system.

The default shell in a Linux environment is often Bash (Bourne Again Shell, written by Steve Bourne). Bash is not the only shell. You can use `chsh -l` to list all available shells. Alternatively you can list out the contents of `/etc/shells`.

Your default shell is configured in the user account database config file `/etc/passwd`.

The shell that you choose is often about memory usage and the memory footprint of a shell when it is running.

### Bash History
Bash history maintains command history for each user in `~/.bash_history`. Since this is stored in a file, it persists even through server reboots.

To call a previously used command, you can use the `!` operator. `!v` will run the last command in our bash history that started with the character `v`.

`!$` represents the last argument used in bash history. This is a useful shortcut when you are operating on a specific file or directory. For example: `mkdir test` followed by `cd !$` will change directory into the newly created `test/` directory.

`!?` will execute the last command to contain a particular string: `!?etc` will run the last command with `etc` in it.

`CTRL+r` will reverse search through your inputs.

`cat ~/.bash_history` will display all of the commands saved in the bash history file. The command `history` can be used to display all of the commands saved in memory, even since the last save to the bash history file. The system will generally save to the bash history upon log out of the session, but you can force a save to history with the command `history -a`.

You can also use a line number from the history file to rerun a command:
```bash
> history
1 cd ~
2 mkdir test
3 cd test
4 cd /etc
5 ls -a
6 cd /usr
7 ls -a
> !4
cd /etc
>
```
Running `!4` in this instance re-ran the command `cd /etc`.

**History variables**

History variables can be used to control history behavior. Setting the variable `HISTCONTROL` to `erasedups` to erase duplicate commands from the bash history.

The in-memory history can be cleared with the command `history -c`. `history -r` can be used to restore the history from the last point of save to the `.bash_history` file. `history -w` can be used to write the current in-memory history to the `.bash_history` file, overwriting it.

## Working With Text Files
There are many tools in the Linux command line used for reading the contents of files, including:
* `cat`: Write the contents of a file, sequentially, to standard output
* `tac`: Write the contents of a file, in reverse-line order, to standard output
* `head`: Display the top _n_ lines of a file
* `tail`: Display the bottom _n_ lines of a file
* `cut`: Display certain columns
* `less`: Page through a file
* `sort`: Sort the output of the above operations, organizing the data into columns.

### cat
[cat](https://en.wikipedia.org/wiki/Cat_(Unix)) is a standard Unix utility that reads files sequentially, writing them to [standard output](https://en.wikipedia.org/wiki/Standard_output). The name is derived from its function to con<b>cat</b>enate files.

`cat` can be used to show a file's contents, especially useful for smaller files.

```bash
$ cat hello.txt
Hello,
$ cat world.txt
world!
$ cat hello.txt world.txt
Hello,
world!
$
```

`cat` can also be passed options to display hidden characters in a file:

```bash
$ cat -vet hello.txt
Hello, $
$ cat -vet hello-world.txt
Hello, $
world!$
$
```

Above, the trailing `$` in the output denotes the end of a line of text. This is useful for debugging issues such as scripts that contain invisible characters. Files created in windows notepad will have different invisible characters at each line termination than Linux has which can lead to script execution problems:

```bash
$ cat test.sh
#!/bin/bash
echo "hello"
echo "goodbye"
$ cat -vet test.sh
#!/bin/bash^M$
echo "hello"^M$
echo "goodbye"^M$
```

The above shows that a shell script contains invalid hidden characters at the end of each line which would lead to exceptions at execution.

### tac
`tac` will concatenate files and write them to standard output, much like [cat](#cat) does, but it does so in reverse-line order:

```bash
$ cat hello-world.txt
Hello,
world!
$ tac hello-world.txt
world!
Hello,
$
```

The first displayed line is the last line of the file, the last displayed line is the first line of the file.

## Additional Resources
* [Linux Virtual Console And Terminal Explained](https://www.computernetworkingnotes.com/linux-tutorials/linux-virtual-console-and-terminal-explained.html)
* [What is "the Shell"?](http://linuxcommand.org/lc3_lts0010.php)
