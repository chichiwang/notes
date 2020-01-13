# Getting Started with the Linux Command Line - WIP
**Author**: [Chi-chi Wang](https://github.com/chichiwang)

A brief overview of the Linux command line. These notes follow along with the [PluralSight Course](https://www.pluralsight.com/courses/getting-started-linux-command-line) of the same title.

## Table of Contents
* [Overview](#overview)
  * [The Terminal and Shell](#the-terminal-and-shell)
  * [Using Linux Help Resources](#using-linux-help-resources)
* [Navigating the File System](#navigating-the-file-system)
  * [Working with Files and Directories](#working-with-files-and-directories)
  * [Standard Streams](#standard-streams)
  * [Archives](#archives)
  * [Kernel Modules and Peripherals](#kernal-modules-and-peripherals)
* [Network Connectivity](#network-connectivity)
  * [IPv4 vs IPv6](#ipv4-vs-ipv6)

## Overview
The past and future of system administration are heavily skewed towards the command line, over graphical interfaces. The reasons for this are: efficiency and infrastructure automation.

Efficiency: the speed at which you can accomplish rote, repetitive tasks favor the command line.

Infrastructure Automation: managing infrastructure is best done through automation and that is best executed through scripting, often through terminal sessions. Remote scripting loves command lines.

### The Terminal and Shell
Whenever you open a *terminal* window from a Linux desktop a new *shell* session is created for you using a hidden settings file in your home directory: `.bashrc`.

`.bashrc` only applies settings to *non-login shell* sessions. These are shell sessions where you opened up the terminal from the system and there was no requirement for you to log into the session.

A *login shell*, by contrast, is a shell session you remotely logged into. Those sessions are generally configured other files (including `.profile`) in your home directory.

There are additional profile files in the `/etc` directory, where system files are configured.

To see the default shell set for each user, check the `/etc/passwd` file. There is a single line for each system and user.

### Using Linux Help Resources
Most programs in Linux come with their own manual files. These can be accessed via the command `man`.

Running `info` will give you a navigatable menu of programs installed and their manuals. You can navigate this program using the arrow keys and pressing enter.

## Navigating the File System

### Working with Files and Directories
The command line allows for globbing when running commands:

```
> touch file1 file2 file3 file4 file5
> mkdir newdir
> cp file* newdir/
```
The above commands will create 5 new files, then copy them into a new directory `/newdir`. The `*` is a wildcard that will match all files that start with "file" regardless of what comes after them.


```
> touch file1 file2 file10 file11
> rm file?
> ls
file10 file11
```

Using the `?` will match only 1 more character on top of the name "file".

You can use backticks to interpolate command outputs into arguments for other commands: `` ls -al /lib/modules/`uname -r` `` will list out the conents of the kernel modules directory for your current running version of the kernel.

```
> rmdir newdir/
```

`rmdir` can be used to remove entire directories.

The `>` command is used to write the output of a command to a file. A single `>` will overwrite contents of any existing file of the same name, while `>>` will append the new content to the end of the file.

`head` will print the first 10 lines of a file, while `tail` will print the last 10 lines.

The easiest way to find files is using `locate`. You can pipe the output of the results to other commands in order to make them easier to parse through.

### Standard Streams
You have three standard streams available to you on the command line:

| Name            | Designation | Numeric Code |
| --------------- | ----------- | ------------ |
| Standard Input  | stdin       | 0            |
| Standard Output | stdout      | 1            |
| Standard Error  | stderr      | 2            |

You can use the numeric codes for these standard streams to designate where you would like to read the input to a file from:
* `echo "Hello" 1> file.txt` would write "Hello" from Standard Output to the file `file.txt`
* `wget invalid.address 2> errors.txt` would write the error returned by `wget` to the file `errors.txt`

### Archives
The most common tool for archiving and compression on linux is `tar`. `tar` used to stand for "tape archive". While tape archives have largely disappeared, `tar` is still in widespread use.

A common compression algorithm used in unix/linux environments is `gzip`. You will often see archive files with the extensions `.tar.gz`.

To unpack such a file, you would run `tar xzf archive.tar.gz`. The argument `x` tells `tar` we want to *extract* the archive. The `z` argument is used to decompress zipped archives. The `f` argument stands for *file* denoting that the filename will be passed in as the next argument.

To archive a directory you would run `tar czf newarchive.tar.gz dirname/`. The argument `c` tells `tar` we want to *compress* a file/directory.

You can also compress a directory without `gzip` and then `gzip` after the fact:
```
> tar cf newarchive.tar dirname/
> gzip newarchive.tar
> ls
newarchive.tar.gz
```

A `.zip` archive can be created using the `zip` command: `zip filename.zip dirname/`. The `unzip` command can be used to unzip `.zip` files.

### Kernel Modules and Peripherals
When troubleshooting problems with your peripherals there are two main steps:
1. Ensure that the system recognizes the device.
2. Ensure that the proper kernel modules is loaded that will allow linux to comminicate with the device and expose it to users.

`lsusb` will list all connected/recognized usb devices.
`lspci` will display all devices connected via pci slots.
`lshw` will list the whole hardware range in one output.
`lsmod` will display all of the modules currently loaded.

The software files that make up kernel files are usually kept in `/lib/modules`. The module you want to use will depend on the version of the linux kernel that you are running.

Running `uname -r` will list out the version of the kernel that you are running currently.

`modprobe` followed by a module name will load a kernel module if it is not already currently loaded.

## Network Connectivity
`ip route show` can be used to show all of your network connections.

`dhclient` can be used to show if there is a DHCP (dynamic host configuration protocol) server on the network that can assign your machine an ip address. This will not show anything if you already have an active connection.

`ip addr` can be used to show you your own ip address. The loopback address listed is a virtual loopback interface that allows connectivity to local resources (usually 127.0.0.1).

`netstat -i` will display all of your network interfaces as well as usage statistics. `netstat -l` will display all of the open and listening ports.

**Older commands**
`route` will display similar output as `ip route show`.

`ipconfig` will display pretty much the same information as `ip addr`.

### IPv4 vs IPv6
IPv4 address are made up of four 8-bit numbers. This allows for a possible 2^32 (~4 billion) combinations. The number of devices connected to the internet has already grown beyond 4 billion.

IPv6 addresses are 128-bit numbers made up of 8 groups of 4 hexadecimal numbers. This allows for exponentially more possible addresses than IPv4.
