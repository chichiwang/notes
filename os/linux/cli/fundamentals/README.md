# Linux Command Line Interface Fundamentals (WIP)
**Author**: [Chi-chi Wang](https://github.com/chichiwang)

An overview of the fundamentals of the Linux command line. These notes follow along with the [PluralSight Course](https://app.pluralsight.com/library/courses/linux-cli-fundamentals/table-of-contents) of the same title.

## Table of Contents
* [Working On The Command Line](#working-on-the-command-line)
  * [Physical Consoles](#physical-consoles)
  * [Accessing Virutal Consoles](#accessing-virtual-consoles)
  * [Pseudo Consoles](#pseudo-consoles)
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

Often the graphical terminal (graphical environment) is tty1, so you can access it from a command line console by pressing `CTRL+ALT+F1`.

To change terminals from within a console you can use the command `chvt` followed by the tty number. For example, if you are on tty 2 and you want to switch to the tty 3, you would enter the command `chvt 3`.

Virtual (physical) consoles are represented by device files that are permanently resident on the machine.

### Pseudo Consoles
These consoles are the most common type of connection - often representing remote connections to a server. Pseudo consoles are created dynamically as a connection is made.

Pseudo terminals are named by the following convention: `/dev/psts/x` where `x` is a number.

More information about a console connection can be found by inspecting the `SSH_` variables. Using autocomplete on `$SSH_` will display the environment variables set up for a SSH connection.

Often these terminals represent remote connections (via SSH or Telnet) as well as connections from the GUI (via XTerminal or Gnome Terminal).

**SSH vs Telnet**

`SSH` is encrypted, listens on port 22, and is 3rd party software (PuTTY).

`Telnet` is unencrypted, listens on port 23, and is first party packaged software.

## Additional Resources
* [Linux Virtual Console And Terminal Explained](https://www.computernetworkingnotes.com/linux-tutorials/linux-virtual-console-and-terminal-explained.html)
