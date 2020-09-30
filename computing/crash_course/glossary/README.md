# Glossary
## [Algorithm](https://en.wikipedia.org/wiki/Algorithm)
In mathematics and computer science, an _algorithm_ is a finite sequence of well-defined, computer-implementable instructions, typically to solve a class of problems or to perform a computation. Algorithms are always unambiguous and are used as specifications for performing calculations, data processing, automated reasoning, or other tasks.

## [Assembler](https://en.wikipedia.org/wiki/Assembly_language#Assembler)
An _assembler_ program creates object code by translating combinations of _mnemonics_ and syntax for operations and addressing modes into their numerical equivalents.

## [Big O Notation](https://en.wikipedia.org/wiki/Big_O_notation)
A mathematical notation that describes the limiting behavior of a function when the argument tends towards a certain value or infinity. Big O is a member of a family of notations invented by [Paul Bachmann](https://en.wikipedia.org/wiki/Paul_Gustav_Heinrich_Bachmann), [Edmund Landau](https://en.wikipedia.org/wiki/Edmund_Landau), and others, collectively called _Bachmann-Landau notation_ or _asymptotic notation_. In computer science, big O notation is used to classify algorithms according to how their run time or space requirements grow as the input size grows.

## [Binary](https://en.wikipedia.org/wiki/Binary)
Means "of two states". From the latin "consisting of two". [Binary numbers](https://en.wikipedia.org/wiki/Binary_number) are numbers represented by two digits. [Binary code](https://en.wikipedia.org/wiki/Binary_code) consists of instructions consisting of two symbols.

## [Bit](https://en.wikipedia.org/wiki/Bit)
A basic unit of information in computing and digital communications. The name "bit" is a portmanteau of "binary digit." The bit represents a logical state with one of two possible values.

## [Branch Predictor](https://en.wikipedia.org/wiki/Branch_predictor)
A digital circuit that tries to guess which way a branch (e.g., an if-then-else structure) will go before this is known definitively. The purpose of the _branch predictor_ is to improve the flow in the [instruction pipeline](#instruction-pipelining).

## [Bus](https://en.wikipedia.org/wiki/Bus_(computing))
A _bus_ is a communication system that transfers data between components inside a computer, or between computers. The expression covers all related hardware components (wire, optical fiber, etc.) and software, including communication protocols.

## [Byte](https://en.wikipedia.org/wiki/Byte)
A unit of digital information that most commonly consists of 8 [bits](#bit). This is the smallest addressable unit of memory in many computer architectures (due to hardware), and is historically the number of bits used to encode a single character of text in a computer.

## [Carry-Look-Ahead Adder](https://en.wikipedia.org/wiki/Adder_(electronics)#Carry-lookahead_adder)
To reduce computation time, engineers devised faster ways to add two binary numbers. The carry-look-ahead adder (CLA) works by creating two signals (_P_ and _G_) for each bit position, based on whether a carry is propagated through from a less significant bit position (at least one input is a 1), generated in that bit position (both inputs are 1), or killed in that bit position (both inputs are 0).

## [Central Processing Unit](https://en.wikipedia.org/wiki/Central_processing_unit)
The _central processing unit_ (_CPU_), also known as a _central processor_, _main processor_, or just _processor_, is the electronic circuitry within a computer that executes instructions that make up a computer program. The CPU performs basic arithmetic, logic, controlling, and input/output (I/O) operations specified by the instructions in the program.

## [Clock Signal](https://en.wikipedia.org/wiki/Clock_signal)
A _clock signal_ (or _logic beat_) oscillates between a high and a low state and is used like a metronome to coordinate actions of digital circuits. A single pulse of this signal is called a _clock cycle_.

## [Clock Generator](https://en.wikipedia.org/wiki/Clock_generator)
A _clock generator_ is an electronic oscillator (circuit) that produces a [clock signal](#clock-signal) for use in synchronizing a circuit's operation. The signal can range from a simple symmetrical square wave to more complex arrangements. The basic parts that all clock generators share are a resonant circuit and an amplifier.

## [Combinational Logic](https://en.wikipedia.org/wiki/Combinational_logic)
A type of [logic circuit](#logic-gate) whose output is a pure function of the present input only.

## [Compiler](https://en.wikipedia.org/wiki/Compiler)
A _compiler_ is a computer proram that translates computer code written in one programming language (the _source language_) into another language (the _target language_). The name "compiler" is primarily used for programs that translate source code from a high-level program language to a lower level language to create an executable program.

## [Computational Complexity](https://en.wikipedia.org/wiki/Computational_complexity)
The _computational complexity_ or simply _complexity_ of an [algorithm](#algorithm) is the amount of resources required to run it. Particular focus is given to time and memory requirements.

## [Conditional](https://en.wikipedia.org/wiki/Conditional_(computer_programming))
In computer science, _conditional statements_, _conditional expressions_, and _conditional constructs_ are features of a programming language, which performs different computations or actions depending on whether a programmer-specified boolean _condition_ evaluates to `true` or `false`. Apart from the case of [branch prediction](#branch-predictor), this is always achieved by selectively altering the [control flow](#control-flow) based on some condition.

## [Conditional Loop](https://en.wikipedia.org/wiki/Conditional_loop)
A _conditional loop_, or _repetitive control structure_, is a way for computer programs to repeat one or more various steps depending on [conditions](#conditional) set either by the programmer initially or real-time by the actual program.

## [Control Flow](https://en.wikipedia.org/wiki/Control_flow)
In computer science _control flow_ (or _flow of control_) is the order in which individual [statements](#statement), instructions, or function calls of an imperative program are executed or evaluated. The emphasis on explicit _control flow_ distinguishes an _imperative programming language_ from a _declarative programming language_.

## [Control Unit](https://en.wikipedia.org/wiki/Control_unit)
The _control unit_ (_CU_) is a component of a computer's central processing unit (CPU) that directs the operation of the processor. It tells the computer's memory, arithmetic and logic unit and input and output devices how to respond to the instructions that have been sent to the processor.

## [CPU Cache](https://en.wikipedia.org/wiki/CPU_cache)
A hardware cache used by the [CPU](#central-processing-unit) of a computer to reduce the average cost (time or energy) to access data from the main memory. This cache is a smaller, faster memory, located closer to a processor core, which stores copies of the data from frequently used memory locations. Most CPUs have different independent caches, including instruction and data cahces, where the data cache is usually organized as a hierarchy of more cache levels (L1, L2, L3, L4, etc.).

## [Diode](https://en.wikipedia.org/wiki/Diode)
An electronic component that permits the one-way flow of current. A diode is a kind of [vacuum tube](#vacuum-tube).

## [Dirty Bit](https://en.wikipedia.org/wiki/Dirty_bit)
A [bit](#bit) that is associated with a block of computer memory and indicates whether or not the corresponding block of memory has been modified. The dirty bit is set when the processor writes to (modifies) this memory. The bit indicates that its associated block of memory has been modified and has not been saved to storage yet. When a block of memory is to be replaced, its corresponding dirty bit is checked to see if the block needs to be written back to secondary memory before being replaced or if it can simply be removed. Dirty bits are used by the [CPU cache](#cpu-cache) and in the page replacement algorithms of an operating system.

## [Dynamic Random-Access Memory](https://en.wikipedia.org/wiki/Dynamic_random-access_memory)
_DRAM_ is a type of [random access](#random-access-memory) semiconductor memory that stores each bit of data in a memory cell consisting of a tiny capacitor and transitor.

## [Flip-Flop](https://en.wikipedia.org/wiki/Flip-flop_(electronics))
A _flip-flop_, or _latch_, is a circuit that has two stable states that can be used to store state information. The circuit can be made to change state by signals applied to one more more control inputs and will have one or two outputs.

## [Floating Point](https://en.wikipedia.org/wiki/Floating-point_arithmetic)
Arithmetic using formulaic representation of real numbers as an approximaton to support a trade-off between range and precision. The term _floating point_ refers to the fact that a number's radix point (decimal point) can "float" anywhere relative to the significant digits of the number.

## [FLOPS](https://en.wikipedia.org/wiki/FLOPS)
_Floating point operations per second_ (_FLOPS_, _flops_, or _flop/s_) is a measure of computer performance, useful in fields of scientific computations that require [floating point](#floating-point) calculations. For such cases it is a more accurate measure than measuring instructions per second.

## [Full Adder](https://en.wikipedia.org/wiki/Adder_(electronics)#Full_adder)
A _full adder_ is a circuit that adds binary numbers and accounts for values carried in as well as out. A one-bit full adder adds three one-bit numbers (A, B, and C<sub>in</sub>). A and B are the [operands](#operand), and C<sub>in</sub> is a bit carried from the previous stage.

## [Graph](https://en.wikipedia.org/wiki/Graph_(discrete_mathematics))
In mathematics, and more specifically in _graph theory_, a _graph_ is a structure amounting to a set of objects in which some pairs of the objects are in some sense "related". The objects correspond to mathematical abstrcations called _vertices_ (also called _nodes_ or _points_) and each of the related pairs of vertices is called an _edge_ (also called _link_ or _line_). Typically, a graph is represented in diagrammatic form as a set of dots or circles for the vertices, joined by lines or curves for the edges. Graphes are one of the objects of study in discrete mathematics.

![A graph with six vertices and seven edges](./graph.svg)
<br />
A graph with six vertices and seven edges.

## [Graph Traversal](https://en.wikipedia.org/wiki/Graph_traversal)
_Graph traversal_, also known as _graph search_, refers to the process of visiting (checking and/or updating) each vertex in a graph. Such traversals are classified by the order in which the vertices are visited. _Tree traversal_ is a special case of graph traversal.

## [Half Adder](https://en.wikipedia.org/wiki/Adder_(electronics)#Half_adder)
The _half adder_ is a circuit that adds two single binary digits A and B. It has two outputs, sum (S) and carry (C). The carry signal represents an overflow into the next digit of a multi-digit addition.

## [Instruction Pipelining](https://en.wikipedia.org/wiki/Instruction_pipelining)
A technique for implementing instruction-level parallelism within a single processor. Pipelining attempts to keep every part of the processor busy with some instruction by dividing incoming instructions into a series of sequential steps performed by different processor units with different parts of instructions processed in parallel.

## [Integer Overflow](https://en.wikipedia.org/wiki/Integer_overflow)
An _integer overflow_ occurs when an arithmetic operation attempts to create a numeric value that is outside of the range that can be represented with a given number of digits - either higher than the maximum or lower than the minimum representable value. An overflow condition may give results leading to unintended behavior. If the possibility of an overflow has not been anticipated, it can compromise a program's reliability and security.

## [Interoperability](https://en.wikipedia.org/wiki/Interoperability)
A characteristic of a product or system, whose interaces are completely understood, to work with other products or systems, at present or in the future, in either implementation or access, without any restrictions. The term was initially defined for information technology or systems engineering services to allow for information exchange.

## [Latency](https://en.wikipedia.org/wiki/Latency_(engineering))
The time delay between the cause and effect of some physical change in a system being observed.

## [Library](https://en.wikipedia.org/wiki/Library_(computing))
A _library_ is a collection of non-volatile resources used by computer programs, often for software development. These may include configuration data, documentation, help data, message templates, pre-written code and [subroutines](#subroutine), classes, values or type specifications. A _library_ is a collection of implementations of behavior, written in terms of a language, that has a well-defined interface by which the behavior is invoked.

## [Logic Gate](https://en.wikipedia.org/wiki/Logic_gate)
A physical electronic/electromechanical device implementing a boolean function. It performs a logical operation on one or more binary inputs to produce a single binary output.

## [Machine Code](https://en.wikipedia.org/wiki/Machine_code)
In computer programming, _machine code_, consisting of machine language instructions, is a low-level programming language used to directly control a computer's [central processing unit](#central-processing-unit). Each instruction causes the CPU to perform a specific task such as a _load_, a _store_, a _jump_, or an [arithmetic logic unit (ALU)](../05/README.md) operation on one or more units of of data in the CPU's [registers](#register) or memory.

## [Microarchitecture](https://en.wikipedia.org/wiki/Microarchitecture)
In computer engineering, _microarchitecture_, also called _computer organization_, is the way a given [instruction set architecture (ISA)](https://en.wikipedia.org/wiki/Instruction_set_architecture) is implemented in a particular [processor](#central-processing-unit).

## [Memory Address](https://en.wikipedia.org/wiki/Memory_address)
A reference to a specific memory location used at various levels of software and hardware. Memory addresses are fixed-length sequences of digits conventionally displayed and manipulated as unsigned integers.

## [Multi-Core Processor](https://en.wikipedia.org/wiki/Multi-core_processor)
A _multi-core processor_ is a [computer processor](#central-processing-unit) integrated circuit with two or more separate processing units, called _cores_, each of which reads and executes program instructions, as if the computer had several processors.

## [Multiplexer](https://en.wikipedia.org/wiki/Multiplexer)
A _multiplexer_ (or _mux_/_data selector_) is a device that selects between several analog or digital input signals and forwards it to a single output line.

## [Non-Volatile Memory](https://en.wikipedia.org/wiki/Non-volatile_memory)
A type of computer memory that can retrieve stored data even after having been power cycled.

## [Operand](https://en.wikipedia.org/wiki/Operand)
In mathematics, an _operand_ is the object of a mathematical operation, i.e., it is the object or quantity that is being operated on.

## [Operation Code](https://en.wikipedia.org/wiki/Opcode)
An _operation code_ (also known as _opcode_, _instruction machine code_, _instruction code_, _instruction syllable_, _instruction parcel_, or _opstring_) is the portion of a machine language instruction that specifies the operation to be performed. Besides the opcode itself, most instructions specify the data they will process in the form of [operands](#operand).

## [Out-Of-Order Execution](https://en.wikipedia.org/wiki/Out-of-order_execution)
_Out-of-order execution_ (or _dynamic execution_) is a paradigm used in most high-performance [CPUs](#central-processing-unit) to make use of [instruction cycles](#clock-signal) that would otherwise be wasted. In this paradigm, a processor executes instructions in an order governed by the availability of input data and execution units, rather than by their original order in a program. In doing so, the processor can avoid being idle while waiting for the preceding instruction to complete and can, in the meantime, process the next instructions that are able to run immediately and independently.

## [Positional Notation](https://en.wikipedia.org/wiki/Positional_notation)
A way of representing the values of a positional system (a numeral system in which the contribution of a digit to the value of a number is the product of the value of the digit by a factor determined by the position of the digit).

## [Pseudocode](https://en.wikipedia.org/wiki/Pseudocode)
_Pseudocode_ is a plain language description of the steps in an algorithm or another system. Pseudocode often uses structural conventions of a normal programming language, but is intended for human reading rather than machine reading. It typically omits details that are essential for machine understanding of the algorithm, such as variable declarations and language-specific code.

## [Random-Access Memory](https://en.wikipedia.org/wiki/Random-access_memory)
_Random-access memory_ (_RAM_) is a form of computer memory that can be read and changed in any order, typically used to store working data and machine code. Most RAM is [volatile](#volatile-memory).

## [Register](https://en.wikipedia.org/wiki/Processor_register)
A _processor register_ (or _CPU register_) is a quickly accessible location available to a computer's processors. Registers usually consist of a small amount of fast storage and may be read-only or write-only. Some registers may have specific hardware functions.

## [Relay](https://en.wikipedia.org/wiki/Relay)
Electronically controlled, mechanical switches. In a relay a control wire connected to a coil of wire which is used to create an electromagnetic field when current flows through. This field attracts a metal arm inside the relay, completing a circuit.
![Diagram of a relay](./relay.jpg)

## [Ripple-Carry Adder](https://en.wikipedia.org/wiki/Adder_(electronics)#Ripple-carry_adder)
A _ripple-carry adder_ is a circuit that utilizes multiple [full adders](#full-adder) to add _N_-bit numbers. Each full adder inputs a C<sub>in</sub>, which is the C<sub>out</sub> of the previous adder. This kind of adder is called a ripper-carry adder (RCA) because each carry bit "ripples" to the next full adder. The first (and only the first) full adder may be replaced with a [half adder](#half-adder) under the assumption that C<sub>in</sub> = 0.

## [Sequential Logic](https://en.wikipedia.org/wiki/Sequential_logic)
A type of [logic circuit](#logic-gate) whose output depends not only on the present value of its input signals but on the sequence of past inputs (the input history).

## [Speculative Execution](https://en.wikipedia.org/wiki/Speculative_execution)
An optimization technique where a computer system performs some task that may not be needed. Work is done before it is known whether it is actually needed, so as to prevent a delay that would have to be incurred by doing the work after it is known that it is needed. If it turns out the work was not needed after all, most changes made by the work are reverted and the results are ignored.

## [Statement](https://en.wikipedia.org/wiki/Statement_(computer_science))
In _computer programming_ a _statement_ is a syntactic unit of an imperative programming language that expresses some action to be carried out. A program written in cuch a language is formed by a sequence of one or more _statements_. A statement may have internal components (e.g., expressions).

## [Static Random-Access Memory](https://en.wikipedia.org/wiki/Static_random-access_memory)
_Static Random-Access Memory_ (_SRAM_) is a type of [RAM](#random-access-memory) that uses latching circuitry ([flip-flop](#flip-flop)) to store each bit. SRAM is [volatile memory](#volatile-memory): data is lost whenever power is removed.

## [Subroutine](https://en.wikipedia.org/wiki/Subroutine)
A _subroutine_ is a sequence of program instructions that performs a specific task, packaged as a unit. This unit can then be used in programs wherever that particular task should be performed. _Subroutines_ can be defined within programs, or separately in _libraries_ that can be used by many programs. In different programming languages, a _subroutine_ may be called a _routine_, _subprogram_, _function_, _method_, or _procedure_. Technically, these terms all have different definitions. The generic, umbrella term _callable unit_ is sometimes used.

## [Supercomputer](https://en.wikipedia.org/wiki/Supercomputer)
A _supercomputer_ is a computer with a high level of performance as compared to a general-purpose computer. Supercomputers play an important role in the field of computational science, and are used for a wide range of computationally intensive tasks in various fields, including quantum mechanics, weather forecasting, climate research, oil and gas exploration, molecular modeling, and physical simulations.

## [Superscalar](https://simple.wikipedia.org/wiki/Superscalar)
A _superscalar [CPU](#central-processing-unt)_ design makes a form of parallel computing calle instruction-level parallelism inside a single CPU, which allows more work to be done at the same clock rate. This means the CPU executes more than one instruction during a clock cycle by running multiple instructions at the same time (called _instruction dispatching_) on duplicate functional units.

## [Syntax](https://en.wikipedia.org/wiki/Syntax_(programming_languages))
The _syntax_ of a computer language is the set of rules that defines th combinations of symbols that are considered to be correctly structured [statements](#statement) and expressions in that language. This applies to both _programming languages_ where the document represents source code, and to _markup languages_, where the document represents data.

## [Transistor](https://en.wikipedia.org/wiki/Transistor)
Similar to a [relay](#relay) or [vacuum tube](#vacuum-tube) a _transistor_ is a switch that can be opened or closed via the application of a current to a control wire. Typically a transistor consists of two electrodes separated by a gate electrode (a semiconductor). By changing the electrical charge of the gate, the conductivity of the semiconductor could be manipulated.

## [Vacuum Tube](https://en.wikipedia.org/wiki/Vacuum_tube)
A device that controls electric current flow in a high vacuum between electrodes to which an electric potential difference has been applied.

## [Volatile Memory](https://en.wikipedia.org/wiki/Volatile_memory)
Computer memory that requires power to maintain the stored information. It retains its contents while powered on but when the power is interrupted the stored data is quickly lost. In addition to being faster than forms of mass storage, volatility can protect sensitive information (as it becomes unavailable on power-down).
