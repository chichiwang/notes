# Instructions and Programs
[Video Link](https://youtu.be/zltgXvg6r3k)

Some common, useful operation codes used in computer programs include:
* [JUMP](https://en.wikipedia.org/wiki/Branch_(computer_science)): Update the [instruction address register](https://en.wikipedia.org/wiki/Program_counter) to a new address.
* [HALT](https://en.wikipedia.org/wiki/HLT_(x86_instruction)): Halts program execution, awaits the next external interrupt.

A program written in such a way that it never reaches the end of its instruction set or halts is said to contain an [infinite loop](https://en.wikipedia.org/wiki/Infinite_loop). An infinite loop is a series of instructions written in a way that the instructions will continue to execute endlessly.

In order to escape an *infinite loop* a [conditional statement](https://en.wikipedia.org/wiki/Conditional_(computer_programming)) must be used. Conditional statments, conditional expressions, and conditional constructs are features of a programming language which will execute one next instruction or another depending on whether a programmer-specified boolean condition evaluates to *true* or *false*. This allows a loop to terminate upon some conditional turning *true* or *false* in the course of program execution.

Software is able to provide functionality that the hardware does not. If a computer's ALU does not have logic to computer divisions, for example, software written could be used to accout for that gap.

In any [instruction set architecture](https://en.wikipedia.org/wiki/Instruction_set_architecture) an [instruction](https://en.wikipedia.org/wiki/Instruction_set_architecture#Instructions) is [encoded](https://en.wikipedia.org/wiki/Instruction_set_architecture#Instruction_encoding) in memory with a fixed size limit. These instructions are comprised of an [opcode](../glossary/README.md#operation-code) of a fixed size, and zero or more operands specifiers which may specify registers, memory locations, or literal data. The amount of memory allocated to each instruction is referred to as the [instruction length](https://en.wikipedia.org/wiki/Instruction_set_architecture#Instruction_length). The *instruction length* may be of a fixed length in some architectures. Other architectures are able to use variable length instructions.

*Operand specifiers* can be registers, memory locations, or *immediate values* depending on the architecture and the opcode. An immediate [value](https://en.wikipedia.org/wiki/Value_(computer_science)) is a value stored as part of the instruction which employs it. It is stored as a literal value for the instructions which expect such values.

| [Previous: The Central Processing Unit](../07/README.md) | [Table of Contents](../README.md#table-of-contents) | Next |
