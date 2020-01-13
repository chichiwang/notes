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
  * [Domain Name System Configuration](#domain-name-system-configuration)
  * [Remote Connections and SSH](#remote-connections-and-ssh)
* [Linux Scripting](#linux-scripting)
  * [Scripting Basics](#scripting-basics)

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

### Domain Name System Configuration
The domain name system (DNS) was created to map numeric addresses to human readable names.

There are databases accessible to every network that contain an up-to-date index of names. The services that maintain these databases are called *DNS servers*.

You can tell if your computer has access to a DNS server by trying to request a page from a DNS name (such as [google.com](http://www.google.com)).

The `host` command can be used to translate DNS to IP and IP back to DNS (ex: `host www.google.com`).

You can also use `ping` to check the health of your network connection. `ping` is a command that will send a short message to a server with a request that the message is echoed back. This lets us know the address is live.

`ping 8.8.8.8` (the IP of one of Google's servers) will work just as well as pinging a DNS address (`ping www.google.com`).

If pinging an IP address works, but pinging a DNS address does not, this means there is something wrong with your connection to the DNS server, or there is something wrong with the DNS server itself.

On some Linux distributions you manage your DNS settings from the `/etc/resolve.conf` file.

You can create/manage your own DNS indicies by editing the `/etc/hosts` file. This configuration is used alongside any DNS servers that you use.

### Remote Connections and SSH
The [OpenSSH package](https://www.openssh.com/) operates using the [Secure Shell Protocol](https://www.ssh.com/ssh/protocol).

This package was designed to solve two big problems:
1. Allowing system administrators to access remote and virtual servers so they can get their work done.
2. Securing the data that flows back and forth as a result of that access against snooping.

Why not access the servers physically? Servers tend to not have displays or keyboards attached. They are often mounte with many other servers on racks. Virtual machines, which make up an overwhelming majority of running servers, don't even have physical ports to connect keyboards or displays. Also, many server workloads are running offsite, far away from those who administer them.

SSH secures your connection through *session encryption*. All of the data packets sent between the two machines will be scrambled.

OpenSSH is so popular it is even available natively on Windows 10. SSH has been a staple of Linux for decades.

To get an OpenSSH session going you will need to install packages on both machines. The server will run the `openssh-server` package while the client will run the `openssh-client` (or `openssh-clients` on CentOS) package.

The configuartions for both server and client are found in the `/etc/ssh` directory. `sshd_config` is the configuration used to manage your system's ssh host behavior. `ssh_config` will handle how your system will login as a client on remote hosts.

By default SSH servers listen on port 22.

Another useful tool is `scp`. `scp` stands for *secure copy*. It operates on top of your SSH infrastructure to securely copy files between machines.

## Linux Scripting
On Linux script live somewhere between programming code and command line execution. Bash scripts are command-line friendly: you can use just about any of the terminal commands you are familiar with, in exactly the same ways. They also have some of the conveniences associated with programming: user inputs, dynamic variables, and loops.

### Scripting Basics

Giving script files a `.sh` extension is a useful convention for keeping track of script files, but is not actually necessary.

The first line of every script must be `#!/bin/bash` to tell Linux that it is an executable script and that it uses the `/bin/bash` shell interpreter.

`declare -i var1` declares `var1` as an integer. Without the `-i` flag, `declare` assumes the variables will contain text strings.

`echo` will output a string to the STDOUT stream.

`read` will read user input and assign it to a variable (ie: `read var1`).

You can end the program with `exit 0` to tell the script to terminate and return the exit code 0 (indicating success). This is not always necessary.
