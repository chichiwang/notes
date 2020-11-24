# The First Programming Languages
[Video Link](https://youtu.be/RU1u-js7db8)

Computer hardware can only handle raw [binary](../glossary/README.md#binary) instructions. This is the only language a computer's processor is able to speak. This language is known as [machine code](../glossary/README.md#machine-code). In the early days of computing programmers had to write entire programs in _machine code_. They would first write a program on paper in [pseudocode](../glossary/README.md#pseudocode), a high level version of a program in English. When the program was fully figured out on paper, a programmer would then expand and translate the program to binary machine code by hand using things like [opcode tables](https://en.wikipedia.org/wiki/Opcode_table). After the translation of the program was complete, the program could be fed into the computer and run.

In the late 1940s, into the 50s, programmers developed higher level languages that were more human-readable. [Opcodes](../glossary/README.md#operation-code) were given simple names called [mnemonics](https://en.wikipedia.org/wiki/Assembly_language#Opcode_mnemonics_and_extended_mnemonics), which were followed by [operands](../glossary/README.md#operand) to form instructions. Rather than write instructions in binary code, programmers could write commands such as `LOAD_A 14`. A CPU, however, doesn't understand `LOAD_A 14` or any other text-based instructions - it only understands binary. Programmers created reusable helper programs in binary that read in text-based instructions and _assembled_ them into the corresponding binary instructions automatically. These helper programs are called [assemblers](../glossary/README.md#assembler): the read in programs written in _assembly language_ and converts them to native _machine code_.

Over time _assemblers_ gained new features that made programming even easier. One such feature was automatically figuring out _JUMP_ addresses. Assembly languages added the ability to label your lines and then `JUMP` instructions could target these labels. The _assembler_ will then do the work of figuring out the target JUMP addresses.

While assembly languages makes writing instructions more human legible, they are still a thin veneer over the underlying machine code. Generally each assembly language instruction converts directly to a corresponding machine instruction in a one-to-one mapping, making it inherently tied to the underlying hardware. The assembler still forces programmers to think about which [registers](../glossary/README.md#register) and memory locations they will use.

This problem did not escape [Dr. Grace Hopper](https://en.wikipedia.org/wiki/Grace_Hopper), a U.S. naval officer and one of the first programmers on the [Harvard Mark I computer](https://en.wikipedia.org/wiki/Harvard_Mark_I). The Mark I's instruction set was so primitive, there weren't even JUMP instructions. After the war, Hopper continued to work at the forefront of computing. To unleash the potential of computers, Hopper designed a high level programming language called [Aritmetic Language Version 0](https://en.wikipedia.org/wiki/A-0_System), also known as _A-0_.

_Assembly languages_ have a direct one-to-one mapping to machine instructions, but a single line of a high-level programming language might result in dozens of instructions being executed by the CPU. To perform this complex translation, Hopper built the first [compiler](../glossary/README.md#compiler) in 1952: this is a specialized progrma that translates source code written in a progrmaming language into a low-level language like assembly or the binary machine code that the CPU can directly process.

Many were skeptical of the value of compilers. Dr. Grace Hopper once stated:
> I had a running compiler and nobody would touch it. ... they carefully told me, computers could only do arithmetic; they could not do programs.

Ultimately the idea was a good one and soon many efforts were underway to develop new programming languages. Today [there are hundreds](https://en.wikipedia.org/wiki/List_of_programming_languages), but sadly there are no surviving examples of A-0 code.

In these higher-level programming languages, programmers no longer have to deal with registers and memory locations. The compiler abstracts away low-level, unnecessary complexity, tracking registers and memory locations under the hood. The programmer can simply create abstractions for needed memory locations known as _variables_ and giving these locations names.

While an important historical milestone, the A-0 and its later variants were not widely used. A few years later, in 1957, IBM released the language [Fortran](https://en.wikipedia.org/wiki/Fortran) (derived from _Formula Translation_) which came to dominate early computer programming. The Fotran project director, John Backus, said:
> Much of my work has come from being lazy. I didn't like writing programs, and so ... I started work on a programming system to make it easier to write programs.

On average, programs written in Fortran were 20 times shorter than equivalent hand-written assembly code. The Fortran compiler would take the terse Fortram code and expand and translate it into native machine code. The community was skeptical that performance would be as good as hand-written code, but the fact that programmers could write more code more quickly made it an easy choice economically, trading a small increase in computation time for a significant decrease in programming time.

Initially Fortran code could only be compiled and run on IBM computers. Indeed, most programming languages and compilers in the 1940s and 50s could only run on a single type of computer. If you upgraded your computer, you'd often need to rewrite all of your code as well. In response to this problem, computer experts from industry, academia, and government formed a consortium in 1959: [The Committee on Data Systems Languages](https://en.wikipedia.org/wiki/CODASYL), advised by Grace Hopper, to guide the development of a common programming language that could be used on different machines. The result was the high-level, easy to use, [Common Business-Oriented Language](https://en.wikipedia.org/wiki/COBOL) (or COBOL).

To deal with the underlying hardware each computing architecture needed its own COBOL compiler, but each of these compilers could accept the same COBOL source code no matter which machine it was run on. This philosophy is known as [write once, run anywhere](https://en.wikipedia.org/wiki/Write_once,_run_anywhere), which is true of most programming languages today (a benefit of moving away from assembly and machine code, which is still CPU-specific).

The biggest benefit to high-level programming languages that can be compiled onto many architectures is that they reduced the barrier of entry to computing. Before these high-level programming languages existed it was a realm exclusive to computer experts and enthusiasts (and it was often their full time profession). Now, scientists, engineers, doctors, economists, teachers, and others can incorporate computation into their work. Thanks to these languages, computing went from a cumbersome and esoteric discipline to a general purpose and accessible tool. At the same time, abstractions in programming languages allowed the computer experts to create increasingly sophisticated programs (which would have taken millions, tens-of-millions of lines of assembly code).

The advent of COBOL in 1959 jump started a golden era in programming language design, evolving in lock-step with advances in computer hardware. In the 1960s we had languages like [ALGOL](https://en.wikipedia.org/wiki/ALGOL), [Lisp](https://en.wikipedia.org/wiki/Lisp_(programming_language)), and [BASIC](https://en.wikipedia.org/wiki/BASIC). In the 1970s [Pascal](https://en.wikipedia.org/wiki/Pascal_(programming_language)), [C](https://en.wikipedia.org/wiki/C_(programming_language)), and [Smalltalk](https://en.wikipedia.org/wiki/Smalltalk) were released. In the 1980s [C++](https://en.wikipedia.org/wiki/C%2B%2B), [Objective-C](https://en.wikipedia.org/wiki/Objective-C), and [Perl](https://en.wikipedia.org/wiki/Perl) entered the stage. In the 1990s we saw the introduction of [Python](https://en.wikipedia.org/wiki/Python_(programming_language)), [Ruby](https://en.wikipedia.org/wiki/Ruby_(programming_language)), and [Java](https://en.wikipedia.org/wiki/Java_(software_platform)). The new millenium has seen the rise of [Swift](https://en.wikipedia.org/wiki/Swift_(programming_language)), [C#](https://en.wikipedia.org/wiki/C_Sharp_(programming_language)), and [Go](https://en.wikipedia.org/wiki/Go_(programming_language)).

| [Previous: Early Programming](../10/README.md) | [Table of Contents](../README.md#table-of-contents) | [Next: Statements and Functions](../12/README.md) |