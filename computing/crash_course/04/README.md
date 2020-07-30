# Representing Numbers and Letters with Binary
[Video Link](https://youtu.be/1GSjbWt0c9M)

## Numbers
In a decimal system, there are 10 possible values a single digit can be: 0 through 9. To get values larger than 9 we add more digits to the number.

Similarly in binary there are only 2 possible values a single digit can be: 0 and 1.

The placement of each digit in a number system denotes its count of a particular value. Take the number `263` in a decimal system. It can be thought of as:

| 100's | 10's | 1's |
| ----- | ---- | --- |
| 2     | 6    | 3   |

The above number means that the value contains 3 `1's`, 6 `10's`, and 2 `100's` or:

2 x 100 = 200<br/>
6 x 10  = 60<br/>
3 x 1   = 3<br/>

200 + 60 + 3 = 263

Because there are only 10 possible digits to choose from for each column, once you exceed the highest value for a digit (9) you carry the remaining values over to the next column over, right-to-left.

This system of representing numbers is called [positional notation](../glossary/README.md#positional-notation). The number system we are most familiar with is a base-10 system, otherwise known as a decimal numeber system.

Binary notation works the same way, it is just base-2 because there are only 2 possible digits in binary (0, 1). Each position must be 2x higher than the column to its right.

The binary number `101` can be broken down thusly:

| 4's | 2's | 1's |
| --- | --- | --- |
| 1   | 0   | 1   |

This number means you have 1 `4`, 0 `2's`, and 1 `1`. That would be the vaue `5` in base-10 notation:

1 x 4 = 4<br/>
0 x 2 = 0<br/>
1 x 1 = 1<br/>

4 + 0 + 1 = 5

To represent larger values, binary requires more digits than decimal. The binary number `10110111` equates to the decimal number `183`.

A single binary digit is called a [bit](../glossary/README.md#bit). The binary number `10110111` is an 8-bit number. The lowest value of an 8-bit number is `00000000` or 0 in decimal. The highest value of an 8-bit number is `11111111` or 255 in decimal. That is 256 possible values, or 2<sup>8</sup>.

8-bits is such a common size in computing that it is called a [byte](../glossary/README.md#byte). The words **kilobyte**, **megabyte**, **gigabyte**, and so forth denote different scales of data. One kilobyte (KB) is 1,000 bytes (8,000 bits). Mega is a million bytes (MB), and giga is a billion bytes (GB). A terabyte is a trillion bytes, or 8 trillion bits (TB).

When talking about computers, a 32-bit computer means that the computer operates in chunks of 32 bits. The largest (unsigned) number you can represent with 32 bits is 4,294,967,295. Similarly a 64-bit computer operates in chunks of 64 bits.

In order to represent positive and negative numbers most computer use the first bit for the sign: 1 for negative, 0 for positive. They then use the remaining bits for the number itself. This gives 32-bit numbers a range of roughly -2,147,483,648 to 2,147,483,647.

Computers also need to label locations in their memory in order to store and retrieve values. These labels are known as _addresses_. As computer storage scaled up into gigabytes and terabytes of storage it became necessary to have 32 and 64-bit memory addresses.

Computers must also deal with decimal values, or [floating point](../glossary/README.md#floating-point) numbers. Several methods have been developed to represent floating point numbers, the most common of which is the [IEEE 754 standard](https://en.wikipedia.org/wiki/IEEE_754).

## Letters
Computers use numbers to represent letters.

The most straightforward approach may be to assign numbers to the letters of the alphabet (A = 1, B=2, C=3, etc). Sir Francis Bacon used 5-bit sequences to [encode](https://en.wikipedia.org/wiki/Bacon%27s_cipher) all 26 letters of the English Alphabet to send secret messages in the 1600's. 5 bits can store 32 possible values which is enough to encode every letter of the English alphabet, but is not enough to encode all punctuation, digits, as well as upper _and_ lower-case letters.

[ASCII](https://en.wikipedia.org/wiki/ASCII), the American Standard Code for Information Interchange, was invented in 1963. ASCII is a 7-bit code, enough to store 128 different values. This is enough range to encode all upper and lower case letters, numbers, punctuation marks, and symbols.

ASCII also contains a selection of special command codes such as a _newline_ character to tell the computer where to wrap a line to the next row. In older computer systems, text would literally continue off the screen if a newline character wasn't included.

Because ASCII was an early standard, it became widely used and allowed different computers manufactured by different companies to exchange data. The ability to universally exchange information is known as [interoperability](../glossary/README.md#interoperability).

Unfortunately the limitation of ASCII is that it was only really designed for English. Because there are 8 bits in a byte and not 7, it quickly became popular to use codes 128-255 (previously unused) for national characters. In the US these extra codes are used to encode additional symbols (mathematical notation, graphical elements, common accentuated characters, etc). Russian computers used these extra codes to encode cyrillic characters, Greek computers used them to encode Greek characters, and so on.

Things totally broke with the rise of computing in Asia: languages like Chinese and Japanese have thousands of characters. There was no way to encode all of the characters in 8 bits. Each country wound up inventing multi-byte encoding schemes, all of which were mutually incompatible.

Eventually the Unicode format was developed in 1992 to do away with all of the international schemes, replacing them with one universal encoding scheme. The most common version of unicode uses 16 bits with space for over a million codes, enough for every single character from every language ever used, as well as symbols, special characters, emojis, etc.

In the same way that ASCII defines a scheme for encoding letters as binary numbers, other file formats like `mp3` and `gif` use binary numbers to encode sounds or colors of a pixel in our photos, movies, and music. Under the hood it all comes down to long sequences of bits.

| [Previous: Boolean Logic and Logic Gates](../03/README.md) | [Table of Contents](../README.md#table-of-contents) | Next |
| :--------------------------------------------------------: | :-------------------------------------------------: | :--: |
