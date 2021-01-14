# Glossary
## [3D Projection](https://en.wikipedia.org/wiki/3D_projection)
A _3D projection_ (or _graphical projection_) is a design technique used to display a three-dimensional (3D) object on a two-dimensional (2D) surface. These projections rely on visual perspective and aspect analysis to project a complex object for viewing capability on a simpler plane.

3D projections use the primary qualities of an object's basic shape to create a map of points, that are then connected to one another to create a visual element. The result is a graphic that contains conceptual properties to interpret that the figure or image is not actually flat (2D), but rather, is a solid object (3D) being viewed on a 2D display.

3D objects are largely displayed on two-dimensional mediums (i.e. paper and computer monitors). As such, graphical projections are a commonly used design element; notably, in engineering drawing, drafting, and computer graphics. Projections can be calculated through employment of mathematical analysis and formulae, or by using various geometric and optical techniques.

## [Access Control](https://en.wikipedia.org/wiki/Access_control)
In the fields of physical security and information security, _access control_ (_AC_) is the selective restriction of access to a place or other resource while access management describes the process. The act of accessing may mean consuming, entering, or using. Permission to access a resource is called authorization.

Locks and login credentials are two analogous mechanisms of access control.

## [Acknowledgement](https://en.wikipedia.org/wiki/Acknowledgement_(data_networks))
In data networking, telecommunications, and computer buses, an _acknowledgement_ (_ACK_) is a signal that is passed between communicating processes, computers, or devices to signify acknowledgement, or receipt of message, as part of a communications protocol. The negative-acknowledgement (_NAK_ or _NACK_) signal is sent to reject a previously received message or to indicate some kind of error. Acknowledgements and negative acknowledgements inform a sender of the receiver's state so that it can adjust its own state accordingly.

Oftentimes, data messages in telecommunications contain [checksums](#checksum) to verify the integrity of the payload and [header](#header). Checksums work in such a way that if a single bit of the data is corrupted, the checksum would have a different value, so they can provide an inexpensive way to check for (probable) signal integrity. If a message is received with an invalid checksum (that is, the data received would have a different checksum than the message had), the receiver can know that some information was corrupted. Most often, when checksums are employed, a corrupted message received will either not be served an ACK signal, or will be served a NAK signal.

## [Affective Computing](https://en.wikipedia.org/wiki/Affective_computing)
_Affective computing_ is the study and development of systems and devices that can recognize, interpret, process, and simulate human affects. It is an interdisciplinary field spanning computer science, psychology, and cognitive science. While some core ideas in the field may be traced as far back as to early philosophical inquiries into emotion, the more modern branch of computer science originated with Rosalind Picard's 1995 paper on affective computing and her book _Affective Computing_ published by MIT Press. One of the motivations for the research is the ability to give machines emotional intelligence, including to simulate empathy. The machine should interpret the emotional state of humans and adapt its behavior to them, giving an appropriate response to those emotions.

## [Algorithm](https://en.wikipedia.org/wiki/Algorithm)
In mathematics and computer science, an _algorithm_ is a finite sequence of well-defined, computer-implementable instructions, typically to solve a class of problems or to perform a computation. Algorithms are always unambiguous and are used as specifications for performing calculations, data processing, automated reasoning, or other tasks.

## [Android](https://en.wikipedia.org/wiki/Android_(robot))
An _android_ is a [robot](#robot) or other artificial being designed to resemble a human, and often made from a flesh-like material. Historically, androids were completely within the domain of science fiction and frequently seen in film and television, but recent advances in robot technology now allow the design of functional and realistic humanoid robots.

## [API](https://en.wikipedia.org/wiki/API)
An _application programming interface_ (_API_) is a computing interface which defines interactions between multiple software intermediaries. It defines the kinds of calls or requests that can be made, how to make them, the data formats that should be used, the conventions to follow, etc. It can also provide extension mechanisms so that users can extend existing functionality in various ways and to varying degrees. An API can be entirely custom, specific to a component, or it can be designed based on an industry-standard to ensure interoperability. Through information hiding, APIs enable modular programming, which allows users to use the interface independently of the implementation.

## [ARPANET](https://en.wikipedia.org/wiki/ARPANET)
The _ARPANET_ (an acronym for _Advanced Research Projects Agency Network_) was the first wide-area [packet-switching](#packet-switching) [network](#computer-network) with distributed control and one of the first networks to implement the [TCP/IP protocol suite](#internet-protocol-suite). Both technologies became the technical foundation of the [Internet](#internet). The ARPANET was established by the Advanced Research Projects Agency (ARPA) of the United States Department of Defense.

## [Array](https://en.wikipedia.org/wiki/Array_data_structure)
An _array data structure_, or simply an _array_, is a [data structure](#data-structure) consisting of a collection of elements (values of variables), each identified by at least one _array index_ or _key_. An array is stored in such that the position of each element can be computed from its index tuple by a mathematical formula. The simplest type of data structure is a linear array, also called one-dimensional array.

## [Artificial General Intelligence](https://en.wikipedia.org/wiki/Artificial_general_intelligence)
_Artificial general intelligence_ (_AGI_) is the hypothetical intelligence of a machine that has the capacity to understand or learn any intellectual task that a human being can. It is a primary goal of some [artificial intelligence](#artificial-intelligence) research and a common topic in science fiction and futures studies. AGI can also be referred to as _strong AI_, _full AI_, or _general intelligent action_. Some academic sources reserve the term "strong AI" for machines that can experience consciousness. Today's AI is speculated to be decades away from AGI.

In contrast to strong AI, [weak AI](#weak-ai) (also called _narrow AI_) is not intended to perform human cognitive abilities, rather, weak AI is limited to the use of software to study or accomplish specific problem solving or reasoning tasks.

## [Artificial Intelligence](https://en.wikipedia.org/wiki/Artificial_intelligence)
_Artificial intelligence_ (_AI_), sometimes called _machine intelligence_, is intelligence demonstrated by machines, unlike the _natural intelligence_ displayed by humans and animals. Leading AI textbooks define the field of study of "intelligent agents": any device that perceives its environment and takes actions that maximize its chance of successfully achieving its goals. Colloquially, the term "artificial intelligence" is often used to describe machines (or computers) that mimic "cognitive" functions that humans associate with the human mind, such as "learning" and "problem solving".

## [Artificial Neural Network](https://en.wikipedia.org/wiki/Artificial_neural_network)
_Artificial neural networks_ (_ANNs_), usually simply called _neural networks_ (_NNs_), are computing systems vaguely inspired by the biological neural networks that constitute animal brains.

An ANN is based on a collection of connected units or nodes called [artificial neurons](#artificial-neuron), which loosely model the neurons in a biological brain. Each connection, like the synapses in a biological brain, can transmit a signal to other neurons. An artificial neuron that receives a signal then processes it and can signal neurons connected to it. The "signal" at a connection is a real number, and the output of each neuron is computed by some non-linear function of the sum of its inputs. The connections are called edges. Neurons and edges typically have a weight that adjusts as learning proceeds. The weight increases or decreases the strength of the signal at a connection. Neurons may have a threshold such that a signal is sent only if the aggregate signal crosses that threshold. Typically, neurons are aggregated into layers. Different layers may perform different transformations on their inputs. Signals travel from the first layer (the input layer), to the last layer (the output layer), possibly after traversing the layers multiple times.

## [Artificial Neuron](https://en.wikipedia.org/wiki/Artificial_neuron)
An _artificial neuron_ is a mathematical function conceived as a model of biological neurons, a neural network. Artificial neurons are elementary units in an [artificial neural network](#artificial-neural-network). The artificial neuron receives one or more inputs (representing excitatory postsynaptic potentials and inhibitory postsynaptic potentials at neural dendrites) and sums them to produce an output (or activation, representing a neuron's action potential which is transmitted along its axon). Usually each input is separately weighted, and the sum is passed through a non-linear function known as an activation function or transfer function. The transfer functions usually have a sigmoid shape, but they may also take the form of other non-linear functions, piecewise linear functions, or step functions. They are also often monotonically increasing, continuous, differentiable and bounded. The thresholding function has inspired building logic gates referred to as threshold logic; applicable to building logic circuits resembling brain processing. For example, new devices such as memristors have been extensively used to develop such logic in recent times.

The artificial neuron transfer function should not be confused with a linear system's transfer function.

## [ASCII](https://en.wikipedia.org/wiki/ASCII)
_ASCII_ ,abbreviated from _American Standard Code for Information Interchange_, is a character encoding standard for electronic communication. ASCII codes represent text in computers, telecommunications equipment, and other devices. Most modern character-encoding schemes are based on ASCII, although they support many additional characters.

The _Internet Assigned Numbers Authority_ (_IANA_) prefers the name _US-ASCII_ for this character encoding.

ASCII is one of the [IEEE milestones](https://en.wikipedia.org/wiki/Timeline_of_electrical_and_electronic_engineering#List_of_IEEE_milestones).

## [Assembler](https://en.wikipedia.org/wiki/Assembly_language#Assembler)
An _assembler_ program creates object code by translating combinations of _mnemonics_ and syntax for operations and addressing modes into their numerical equivalents.

## [Authentication](https://en.wikipedia.org/wiki/Authentication)
_Authentication_ is the act of proving an assertion, such as the identity of a computer system user. In contrast with identification, the act of indicating a person or thing's identity, authentication is the process of verifying that identity. It might involve validating personal identity documents, verifying the authenticity of a website with a digital certificate, determining the age of an artifact by carbon dating, or ensuring that a product or document is not counterfeit.

## [Automaton](https://en.wikipedia.org/wiki/Automaton)
An _automaton_ (plural: _automata_ or _automatons_) is a relatively self-operating machine, or a machine or control mechanism designed to automatically follow a predetermined sequence of operations, or respond to predetermined instructions. Some automata, such as bellstrikers in mechanical clocks, are designed to give the illusion to the casual observer that they are operating under their own power. Since long ago, the term is commonly associated with automated puppets that resemble moving humans or animals, built to impress and/or to entertain people.

## [Bandwidth](https://en.wikipedia.org/wiki/Bandwidth_(computing))
In computing, _bandwidth_ is the maximum rate of data transfer across a given path. Bandwidth may be characterized as _network bandwidth_, _data bandwidth_, or _digital bandwidth_.

This definition of _bandwidth_ is in contrast to the field of signal processing, wireless communications, modem data transmission, digital communications, and electronics, in which bandwidth is used to refer to analog signal bandwidth measured in hertz, meaning the frequency range between lowest and highest attainable frequency while meeting a well-defined impairment level in signal power. The actual bit rate that can be achieved depends not only on the signal bandwidth but also on the noise on the channel.

## [Bandwidth Throttling](https://en.wikipedia.org/wiki/Bandwidth_throttling)
_Bandwidth throttling_ is the intentional slowing or speeding of an [internet](#internet) service by an [Internet service provider](#internet-service-provider) (ISP). It is a reactive measure employed in communication networks to regulate [network](#computer-network) traffic and minimize [bandwidth](#bandwidth) congestion. Bandwidth throttling can occur at different locations on the network. On a [local area network](#local-area-network) (LAN), a system administrator ("sysadmin") may employ bandwidth throttling to help limit network congestion and server crashes. On a broader level, the Internet service provider may use bandwidth throttling to help reduce a user's usage of bandwidth that is supplied to the local network. Bandwidth throttling is also used as a measurement of data rate on Internet speed test websites.

## [Back-Face Culling](https://en.wikipedia.org/wiki/Back-face_culling)
In computer graphics, _back-face culling_ determines whether a polygon of a graphical object is visible. It is a step in the graphical pipeline that tests whether the points in the polygon appear in clockwise or counter-clockwise order when projected onto the screen. If the user has specified that front-facing polygons have a clockwise winding, but the polygon projected on the screen has a counter-clockwise winding then it has been rotated to face away from the camera and will not be drawn.

The process makes rendering objects quicker and more efficient by reducing the number of polygons for the program to draw. For example, in a city street scene, there is generally no need to draw the polygons on the sides of the buildings facing away from the camera; they are completely occluded by the sides facing the camera.

## [Backlink](https://en.wikipedia.org/wiki/Backlink)
A _backlink_ for a given web resource is a [link](#hyperlink) from some other website (the referrer) to that web resource (the referent). A web resource may be (for example) a website, [web page](#web-page), or web directory.

A backlink is a reference comparable to a citation. The quantity, quality, and relevance of backlinks for a web page are among the factors that search engines like Google evaluate in order to estimate how important the page is. PageRank calculates the score for each web page based on how all the web pages are connected among themselves, and is one of the variables that Google Search uses to determine how high a web page should go in search results. This weighting of backlinks is analogous to citation analysis of books, scholarly papers, and academic journals. A Topical PageRank has been researched and implemented as well, which gives more weight to backlinks coming from the page of a same topic as a target page.

## [Batch Processing](https://en.wikipedia.org/wiki/Batch_processing)
Computerized _batch processing_ is the running of "jobs that can run without end user interaction, or can be scheduled to run as resources permit."

## [Big O Notation](https://en.wikipedia.org/wiki/Big_O_notation)
A mathematical notation that describes the limiting behavior of a function when the argument tends towards a certain value or infinity. Big O is a member of a family of notations invented by [Paul Bachmann](https://en.wikipedia.org/wiki/Paul_Gustav_Heinrich_Bachmann), [Edmund Landau](https://en.wikipedia.org/wiki/Edmund_Landau), and others, collectively called _Bachmann-Landau notation_ or _asymptotic notation_. In computer science, big O notation is used to classify [algorithms](#algorithm) according to how their run time or space requirements grow as the input size grows.

## [Binary](https://en.wikipedia.org/wiki/Binary)
Means "of two states". From the latin "consisting of two". [Binary numbers](https://en.wikipedia.org/wiki/Binary_number) are numbers represented by two digits. [Binary code](https://en.wikipedia.org/wiki/Binary_code) consists of instructions consisting of two symbols.

## [Biometrics](https://en.wikipedia.org/wiki/Biometrics)
_Biometrics_ are body measurements and calculations related to human characteristics. _Biometrics authentication_ (or _realistic authentication_) is used in computer science as a form of identification and access control. It is also used to identify individuals in groups that are under surveillance.

Biometric identifiers are the distinctive, measurable characteristics used to label and describe individuals. Biometric identifiers are often categorized as physiological versus behavioral characteristics. Physiological characteristics are related to the shape of the body. Examples include, but are not limited to fingerprint, palm veins, face recognition, DNA, palm print, hand geometry, iris recognition, retina and odour/scent. Behavioral characteristics are related to the pattern of behavior of a person, including but not limited to typing rhythm, gait, keystroke, signature, behavioral profiling, and voice. Some researchers have coined the term _behaviometrics_ to describe the latter class of biometrics.

## [Bit](https://en.wikipedia.org/wiki/Bit)
A basic unit of information in computing and digital communications. The name "bit" is a portmanteau of "binary digit." The bit represents a logical state with one of two possible values.

## [Bitmap](https://en.wikipedia.org/wiki/Bitmap)
In computing, a _bitmap_ is a mapping from some domain (for example, a range of integers) to [bits](#bit). It is also called a _bit array_ or _bitmap index_.

As a noun, the term "bitmap" is very often used to refer to a particular bitmapping application: the _pix-map_, which refers to a map of [pixels](#pixel), where each one may store more than two colors, thus using more than one bit per pixel. In such a case, the domain in question is the array of pixels which constitute a digital graphic output device (a screen or monitor). In some contexts, the term bitmap implies one bit per pixel, while pixmap is used for images with multiple bits per pixel.

A bitmap is a type of memory organization or image [file format](#file-format) used to store digital images. The term bitmap comes from the computer programming terminology, meaning just a map of bits, a spatially mapped array of bits. Now, along with pixmap, it commonly refers to the similar concept of a spatially mapped array of pixels. [Raster](#raster-scan) images in general may be referred to as bitmaps or pixmaps, whether synthetic or photographic, in files or memory.


## [Black Hat](https://en.wikipedia.org/wiki/Black_hat_(computer_security))
A _black hat hacker_ (or _black-hat hacker_) is a [hacker](#security-hacker) who violates computer security for personal gain or malice.

## [Botnet](https://en.wikipedia.org/wiki/Botnet)
A _botnet_ is a number of [Internet](#internet)-connected devices, each of which is running one or more bots. Botnets can be used to perform [Distributed Denial-of-Service](#denial-of-service-attack) (DDoS) attacks, steal data, send spam, and allow the attacker to access the device and its connection. The owner can control the botnet using command and control (C&C) software. The word "botnet" is a portmanteau of the words "robot" and "network". The term is usually used with a negative or malicious connotation.

## [Bounds Checking](https://en.wikipedia.org/wiki/Bounds_checking)
In computer programming, _bounds checking_ is any method of detecting whether a variable is within some bounds before it is used. It is usually used to ensure that a number fits into a given type (range checking), or that a variable being used as an array index is within the bounds of the array (index checking). A failed bounds check usually results in the generation of some sort of exception signal.

Because performing bounds checking during every usage is time-consuming, it is not always done. Bounds-checking elimination is a compiler optimization technique that eliminates unneeded bounds checking.

## [Branch Predictor](https://en.wikipedia.org/wiki/Branch_predictor)
A digital circuit that tries to guess which way a branch (e.g., an if-then-else structure) will go before this is known definitively. The purpose of the _branch predictor_ is to improve the flow in the [instruction pipeline](#instruction-pipelining).

## [Brute-Force Attack](https://en.wikipedia.org/wiki/Brute-force_attack)
In cryptography, a _brute-force attack_ consists of an attacker submitting many passwords or passphrases with the hope of eventually guessing a combination correctly. The attacker systematically checks all possible passwords and passphrases until the correct one is found. Alternatively, the attacker can attempt to guess the key which is typically created from the password using a key derivation function. This is known as an exhaustive key search.

A brute-force attack is a cryptanalytic attack that can, in theory, be used to attempt to decrypt any encrypted data (except for data encrypted in an information-theoretically secure manner). Such an attack might be used when it is not possible to take advantage of other weaknesses in an encryption system (if any exist) that would make the task easier.

## [Buffer Overflow](https://en.wikipedia.org/wiki/Buffer_overflow)
In information security and programming, a _buffer overflow_, or _buffer overrun_, is an anomaly where a program, while writing data to a buffer, overruns the buffer's boundary and overwrites adjacent memory locations.

Buffers are areas of memory set aside to hold data, often while moving it from one section of a program to another, or between programs. Buffer overflows can often be triggered by malformed inputs; if one assumes all inputs will be smaller than a certain size and the buffer is created to be that size, then an anomalous transaction that produces more data could cause it to write past the end of the buffer. If this overwrites adjacent data or executable code, this may result in erratic program behavior, including memory access errors, incorrect results, and crashes.

[Exploiting](#exploit) the behavior of a buffer overflow is a well-known security exploit. On many systems, the memory layout of a program, or the system as a whole, is well defined. By sending in data designed to cause a buffer overflow, it is possible to write into areas known to hold executable code and replace it with malicious code, or to selectively overwrite data pertaining to the program's state, therefore causing behavior that was not intended by the original programmer. Buffers are widespread in [operating system](#operating-system) (OS) code, so it is possible to make attacks that perform privilege escalation and gain unlimited access to the computer's resources. The famed Morris worm in 1988 used this as one of its attack techniques.

## [Bus](https://en.wikipedia.org/wiki/Bus_(computing))
A _bus_ is a communication system that transfers data between components inside a computer, or between computers. The expression covers all related hardware components (wire, optical fiber, etc.) and software, including communication protocols.

## [Byte](https://en.wikipedia.org/wiki/Byte)
A unit of digital information that most commonly consists of 8 [bits](#bit). This is the smallest addressable unit of memory in many computer architectures (due to hardware), and is historically the number of bits used to encode a single character of text in a computer.

## [Capacitor](https://en.wikipedia.org/wiki/Capacitor)
A _capacitor_ is a device that stores electrical energy in an electric field. It is a passive electronic component with two terminals.

## [Carrier-Sense Multiple Access](https://en.wikipedia.org/wiki/Carrier-sense_multiple_access)
_Carrier-sense multiple access_ (_CSMA_) is a media access control (MAC) protocol in which a node verifies the absence of other traffic before transmitting on a shared transmission medium, such as an electrical bus or a band of the electromagnetic spectrum.

A transmitter attempts to determine whether another transmission is in progress before initiating a transmission using a carrier-sense mechanism. That is, it tries to detect the presence of a carrier signal from another node before attempting to transmit. If a carrier is sensed, the node waits for the transmission in progress to end before initiating its own transmission. Using CSMA, multiple nodes may, in turn, send and receive on the same medium. Transmissions by one node are generally received by all other nodes connected to the medium.

## [Carry-Look-Ahead Adder](https://en.wikipedia.org/wiki/Adder_(electronics)#Carry-lookahead_adder)
To reduce computation time, engineers devised faster ways to add two binary numbers. The carry-look-ahead adder (CLA) works by creating two signals (_P_ and _G_) for each bit position, based on whether a carry is propagated through from a less significant bit position (at least one input is a 1), generated in that bit position (both inputs are 1), or killed in that bit position (both inputs are 0).

## [Cathode-Ray Tube](https://en.wikipedia.org/wiki/Cathode-ray_tube)
The _cathode-ray tube_ (_CRT_) is a [vacuum tube](#vacuum-tube) that contains one or more electron guns and a phosphorescent screen, and is used to display images. It modulates, accelerates, and deflects electron beam(s) onto the screen to create the images. The images may represent electrical waveforms (oscilloscope), pictures (television, computer monitor), radar targets, or other phenomena. CRTs have also been used as memory devices, in which case the visible light emitted from the fluorescent material (if any) is not intended to have significant meaning to a visual observer (though the visible pattern on the tube face may cryptically represent the stored data).

## [Central Processing Unit](https://en.wikipedia.org/wiki/Central_processing_unit)
The _central processing unit_ (_CPU_), also known as a _central processor_, _main processor_, or just _processor_, is the electronic circuitry within a computer that executes instructions that make up a computer program. The CPU performs basic arithmetic, logic, controlling, and [input/output](#inputoutput) (I/O) operations specified by the instructions in the program.

## [Certificate Signing Request](https://en.wikipedia.org/wiki/Certificate_signing_request)
In public key infrastructure (PKI) systems, a _certificate signing request_ (also _CSR_ or _certification request_) is a message sent from an applicant to a registration authority of the public key infrastructure in order to apply for a digital identity certificate. It usually contains the [public key](#public-key-cryptography) for which the certificate should be issued, identifying information (such as a domain name) and integrity protection (e.g., a digital signature). The most common format for CSRs is the PKCS #10 specification; another is the Signed Public Key and Challenge SPKAC format generated by some web browsers.

## [Chatbot](https://en.wikipedia.org/wiki/Chatbot)
A _chatbot_ is a software application used to conduct an on-line chat conversation via text or text-to-speech, in lieu of providing direct contact with a live human agent. Designed to convincingly simulate the way a human would behave as a conversational partner, chatbot systems typically require continuous tuning and testing, and many in production remain unable to adequately converse or pass the industry standard Turing test. The term "ChatterBot" was originally coined by Michael Mauldin (creator of the first Verbot) in 1994 to describe these conversational programs.

## [Checksum](https://en.wikipedia.org/wiki/Checksum)
A _checksum_ is a small-sized datum derived from a block of digital data for the purpose of detecting errors that may have been introduced during its transmission or storage. By themselves, checksums are often used to verify data integrity but are not relied upon to verify data authenticity.

The procedure which generates this checksum is called a _checksum function_ or _checksum algorithm_. Depending on its design goals, a good checksum [algorithm](#algorithm) will usually output a significantly different value, even for small changes made to the input. This is especially true of cryptographic hash functions, which may be used to detect many data corruption errors and verify overall data integrity; if the computed checksum for the current data input matches the stored value of a previously computed checksum, there is a very high probability the data has not been accidentally altered or corrupted.

## [Cipher](https://en.wikipedia.org/wiki/Cipher)
In [cryptography](#cryptography), a _cipher_ (or _cypher_) is an [algorithm](#algorithm) for performing [encryption](#encryption) or decryption - a series of well-defined steps that can be followed as a procedure. An alternative, less common term is _encipherment_. To encipher or encode is to convert information into cipher or code. In common parlance, "cipher" is synonymous with "code", as they are both a set of steps that encrypt a message; however, the concepts are distinct in cryptography, especially classical cryptography.

## [Circuit Switching](https://en.wikipedia.org/wiki/Circuit_switching)
_Circuit switching_ is a method of implementing a telecommunications network in which two network nodes establish a dedicated communications channel (circuit) through the network before the nodes may communicate. The circuit guarantees the full [bandwidth](#bandwidth) of the channel and remains connected for the duration of the communication session. The circuit functions as if the nodes were physically connected as with an electrical circuit. Circuit switching contrasts with [message switching](#message-switching) and [packet switching](#packet-switching).

## [Class Hierarchy](https://en.wikipedia.org/wiki/Class_hierarchy)
A _class hierarchy_ or _inheritance tree_ in computer science is a classification of object types, denoting objects as the instantiations of classes (class is like a blueprint, the object is what is built from that blueprint) inter-relating the various classes by relationships such as "inherits", "extends", "is an abstraction of", "an interface definition". In object-oriented programing, a class is a template that the defines the state and behavior common to objects of a certain kind. A class can be defined in terms of other classes.

## [Clock Signal](https://en.wikipedia.org/wiki/Clock_signal)
A _clock signal_ (or _logic beat_) oscillates between a high and a low state and is used like a metronome to coordinate actions of digital circuits. A single pulse of this signal is called a _clock cycle_.

## [Clock Generator](https://en.wikipedia.org/wiki/Clock_generator)
A _clock generator_ is an electronic oscillator (circuit) that produces a [clock signal](#clock-signal) for use in synchronizing a circuit's operation. The signal can range from a simple symmetrical square wave to more complex arrangements. The basic parts that all clock generators share are a resonant circuit and an amplifier.

## [Code Injection](https://en.wikipedia.org/wiki/Code_injection)
_Code injection_ is the [exploitation](#exploit) of a computer bug that is caused by processing invalid data. Injection is used by an attacker to introduce (or "inject") code into a vulnerable computer program and change the course of execution. The result of successful code injection can be disastrous, for example by allowing computer worms to propagate.

## [Code Reuse](https://en.wikipedia.org/wiki/Code_reuse)
_Code reuse_, also called _software reuse_ is the use of existing software, or software knowledge, to build new software, following the [reusability principles](https://en.wikipedia.org/wiki/Reusability).

## [Collision Domain](https://en.wikipedia.org/wiki/Collision_domain)
A _collision domain_ is a network segment connected by a shared medium or through repeaters where simultaneous data transmissions collide with one another. The collision domain applies particularly in wireless networks, but also affected early versions of [Ethernet](#ethernet). A network collision occurs when more than one device attempts to send a [packet](#network-packet) on a network segment at the same time. Members of a collision domain may be involved in collisions with one another. Devices outside the collision domain do not have collisions with those inside.

Only one device in the collision domain may transmit at any one time, and the other devices in the domain listen to the network and refrain from transmitting while others are already transmitting in order to avoid collisions. Because only one device may be transmitting at any one time, total network [bandwidth](#bandwidth) is shared among all devices on the collision domain. Collisions also decrease network efficiency on a collision domain as collisions require devices to abort transmission and retransmit at a later time.

Since data [bits](#bit) are propagated at a finite speed, simultaneously is to be defined in terms of the size of the collision domain and the minimum packet size allowed. A smaller packet size or a larger dimension would make it possible for a sender to finish sending the packet without the first bits of the message being able to reach the most remote node. So, that node could start sending as well, without a clue to the transmission already taking place and destroying the first packet. Unless the size of the collision domain allows the initial sender to receive the second transmission attempt  the collision  within the time it takes to send the packet he would neither be able to detect the collision nor to repeat the transmission - this is called a _late collision_.

## [Combinational Logic](https://en.wikipedia.org/wiki/Combinational_logic)
A type of [logic circuit](#logic-gate) whose output is a pure function of the present input only.

## [Command Line Interface](https://en.wikipedia.org/wiki/Command-line_interface)
A _command-line interface_ (_CLI_) processes commands to a computer program in the form of lines of text. The program which handles the interface is called a _command-line interpreter_ or _command-line processor_. [Operating systems](#operating-system) implement a command-line interface in a shell for interactive access to operating system functions or services. Such access was primarily provided to users by computer terminals starting in the mid-1960s, and continued to be used throughout the 1970s and 1980s on VAX/VMS, [Unix systems](#unix) and [personal computer](#personal-computer) systems including DOS, CP/M and Apple DOS.

Today, many users rely upon graphical user interfaces and menu-driven interactions. However, some programming and maintenance tasks may not have a graphical user interface and may still use a command line.

## [Comment](https://en.wikipedia.org/wiki/Comment_(computer_programming))
In computer programming, a _comment_ is a programmer-readable explanation or annotation in the source code of a computer program. They are added with the purpose of making the source code easier for humans to understand, and are generally ignored by [compilers](#compiler) and interpreters. The syntax of comments in various programming languages varies considerably.

Comments are sometimes also processed in various ways to generate documentation external to the source code itself by documentaton generators, or used for integration with source code management systems and other kinds of external programming tools.

The flexibility provided by comments allows for a wide degree of variability, but formal conventions for their use are commonly part of programming style guides.

## [Compiler](https://en.wikipedia.org/wiki/Compiler)
A _compiler_ is a computer proram that translates computer code written in one programming language (the _source language_) into another language (the _target language_). The name "compiler" is primarily used for programs that translate source code from a high-level program language to a lower level language to create an executable program.

## [Composite Data Type](https://en.wikipedia.org/wiki/Composite_data_type)
A _composite data type_ or _compound data type_ is any data type which can be constructed in a program using the programming language's primitive data types and other composite types. It is sometimes called a _structure_ or _aggregate data type_, although the latter term may also refer to arrays, lists, etc. The act of constructing a composite type is known as _composition_. Composite data types are often contrasted with scalar variables.

## [Computational Complexity](https://en.wikipedia.org/wiki/Computational_complexity)
The _computational complexity_ or simply _complexity_ of an [algorithm](#algorithm) is the amount of resources required to run it. Particular focus is given to time and memory requirements.

## [Computer-Aided Design](https://en.wikipedia.org/wiki/Computer-aided_design)
_Computer-aided design_ (_CAD_) is the use of computers (or workstations) to aid in the creation, modification, analysis, or optimization of a design. CAD software is used to increase the productivity of the designer, improve the quality of design, improve communications through documentation, and to create a database for manufacturing. CAD output is often in the form of electronic files for print, machining, or other manufacturing operations. The term _CADD_ (for _Computer Aided Design and Drafting_) is also used.

Its use in designing electronic systems is known as electronic design automation (_EDA_). In mechanical design it is known as mechanical design automation (_MDA_) or _computer-aided drafting_ (_CAD_), which includes the process of creating a technical drawing with the use of computer software.

## [Computer Keyboard](https://en.wikipedia.org/wiki/Computer_keyboard)
A _computer keyboard_ is a typewriter-style device which uses an arrangement of buttons or keys to act as mechanical levers or electronic switches. Replacing early [punched cards](#punched-card) and [paper tape](#punched-tape) technology, interaction via teleprinter-style keyboards have been the main input method for computers since the 1970s, supplemented by the computer mouse since the 1980s.

Keyboard keys (buttons) typically have a set of characters engraved or printed on them, and each press of a key typically corresponds to a single written symbol. However, producing some symbols may require pressing and holding several keys simultaneously or in sequence. While most keyboard keys produce letters, numbers or symbols (characters), other keys or simultaneous key presses can prompt the computer to execute system commands, such as such as the Control-Alt-Delete combination used with Microsoft Windows. In a modern computer, the interpretation of key presses is generally left to the software: the information sent to the computer, the scan code, tells it only which key (or keys) on which row and column, was pressed or released.

## [Computer-Mediated Communication](https://en.wikipedia.org/wiki/Computer-mediated_communication)
_Computer-mediated communication_ (_CMC_) is defined as any human communication that occurs through the use of two or more electronic devices. While the term has traditionally referred to those communications that occur via computer-mediated formats (e.g., instant messaging, email, chat rooms, online forums, social network services), it has also been applied to other forms of text-based interaction such as text messaging. Research on CMC focuses largely on the social effects of different computer-supported communication technologies. Many recent studies involve Internet-based social networking supported by social software.

## [Computer Mouse](https://en.wikipedia.org/wiki/Computer_mouse)
A computer _mouse_ (plural _mice_, rarely _mouses_) is a hand-held pointing device that detects two-dimensional motion relative to a surface. This motion is typically translated into the motion of a pointer on a display, which allows a smooth control of the graphical user interface of a computer.

The first public demonstration of a mouse controlling a computer system was in 1968. Mice originally used a ball rolling on a surface to detect motion, but modern mice often have optical sensors that have no moving parts. Originally wired to a computer, many modern mice are cordless, relying on short-range radio communication with the connected system.

In addition to moving a cursor, computer mice have one or more buttons to allow operations such as selection of a menu item on a display. Mice often also feature other elements, such as touch surfaces and scroll wheels, which enable additional control and dimensional input.

## [Computer Network](https://en.wikipedia.org/wiki/Computer_network)
A _computer network_ is a group of computers that use a set of common communication protocols over digital interconnections for the purpose of sharing resources located on or provided by the network nodes. The interconnections between nodes are formed from a broad spectrum of telecommunication network technologies, based on physically wired, optical, and wireless radio-frequency methods that may be arranged in a variety of network topologies.

The nodes of a computer network may be classified by many means as [personal computers](#personal-computer), servers, networking hardware, or general-purpose hosts. They are identified by hostnames and network addresses. Hostnames serve as memorable labels for the nodes, rarely changed after initial assignment. Network addresses serve for locating and identifying the nodes by communication protocols such as the [Internet Protocol](#internet-protocol).

## [Computer Security](https://en.wikipedia.org/wiki/Computer_security)
_Computer security_, _cybersecurity_ or _information technology security_ (_IT security_) is the protection of computer systems and [networks](#computer-network) from the theft of or damage to their hardware, software, or electronic data, as well as from the disruption or misdirection of the services they provide.

The field is becoming more significant due to the increased reliance on computer systems, the [Internet](#internet) and wireless network standards such as Bluetooth and [Wi-Fi](#wi-fi), and due to the growth of "smart" devices, including smartphones, televisions, and the various devices that constitute the ["Internet of things"](#internet-of-things). Owing to its complexity, both in terms of politics and technology, cybersecurity is also one of the major challenges in the contemporary world.

## [Computer Terminal](https://en.wikipedia.org/wiki/Computer_terminal)
A _computer terminal_ is an electronic or electromechanical hardware device that can be used for entering data into, and transcribing data from, a computer or a computing system. The [teletype](#teleprinter) was an example of an early day hardcopy terminal, and predated the use of a computer screen by decades.

Early terminals were inexpensive devices but very slow compared to [punched cards](#punched-card) or paper tape for input, but as the technology improved and video displays were introduced, terminals pushed these older forms of interaction from the industry. A related development was [timesharing](#time-sharing) systems, which evolved in parallel and made up for any inefficiencies of the user's typing ability with the ability to support multiple users on the same machine, each at their own terminal/terminals.

The function of a terminal is typically confined to transcription and input of data; a device with significant local programmable data processing capability may be called a "smart terminal" or fat client. A terminal that depends on the host computer for its processing power is called a "dumb terminal" or a thin client. A personal computer can run terminal emulator software that replicates the function of a terminal, sometimes allowing concurrent use of local programs and access to a distant terminal host system.

## [Computer Vision](https://en.wikipedia.org/wiki/Computer_vision)
_Computer vision_ is an interdisciplinary scientific field that deals with how computers can gain high-level understanding from digital images or videos. From the perspective of engineering, it seeks to understand and automate tasks that the human visual system can do.

Computer vision tasks include methods for acquiring, processing, analyzing and understanding digital images, and extraction of high-dimensional data from the real world in order to produce numerical or symbolic information, e.g. in the forms of decisions. Understanding in this context means the transformation of visual images (the input of the retina) into descriptions of the world that make sense to thought processes and can elicit appropriate action. This image understanding can be seen as the disentangling of symbolic information from image data using models constructed with the aid of geometry, physics, statistics, and learning theory.

## [Computer Worm](https://en.wikipedia.org/wiki/Computer_worm)
A _computer worm_ is a standalone [malware](#malware) computer program that replicates itself in order to spread to other computers. It often uses a [computer network](#computer-network) to spread itself, relying on security failures on the target computer to access it. It will use this machine as a host to scan and infect other computers. When these new worm-invaded computers are controlled, the worm will continue to scan and infect other computers using these computers as hosts, and this behaviour will continue. Computer worms use recursive methods to copy themselves without host programs and distribute themselves based on the law of exponential growth, thus controlling and infecting more and more computers in a short time. Worms almost always cause at least some harm to the network, even if only by consuming [bandwidth](#bandwidth), whereas viruses almost always corrupt or modify files on a targeted computer.

Many worms are designed only to spread, and do not attempt to change the systems they pass through. However, as the Morris worm and Mydoom showed, even these "payload-free" worms can cause major disruption by increasing network traffic and other unintended effects.

## [Conditional](https://en.wikipedia.org/wiki/Conditional_(computer_programming))
In computer science, _conditional statements_, _conditional expressions_, and _conditional constructs_ are features of a programming language, which performs different computations or actions depending on whether a programmer-specified boolean _condition_ evaluates to `true` or `false`. Apart from the case of [branch prediction](#branch-predictor), this is always achieved by selectively altering the [control flow](#control-flow) based on some condition.

## [Conditional Loop](https://en.wikipedia.org/wiki/Conditional_loop)
A _conditional loop_, or _repetitive control structure_, is a way for computer programs to repeat one or more various steps depending on [conditions](#conditional) set either by the programmer initially or real-time by the actual program.

## [Control Flow](https://en.wikipedia.org/wiki/Control_flow)
In computer science _control flow_ (or _flow of control_) is the order in which individual [statements](#statement), instructions, or function calls of an imperative program are executed or evaluated. The emphasis on explicit _control flow_ distinguishes an _imperative programming language_ from a _declarative programming language_.

## [Control Loop](https://en.wikipedia.org/wiki/Control_loop)
A _control loop_ is the fundamental building block of industrial control systems. It consists of all the physical components and control functions necessary to automatically adjust the value of a measured process variable (PV) to equal the value of a desired set-point (SP). It includes the process sensor, the controller function, and the final control element (FCE) which are all required for automatic control.

## [Control Unit](https://en.wikipedia.org/wiki/Control_unit)
The _control unit_ (_CU_) is a component of a computer's central processing unit (CPU) that directs the operation of the processor. It tells the computer's memory, arithmetic and logic unit and input and output devices how to respond to the instructions that have been sent to the processor.

## [Convolution](https://en.wikipedia.org/wiki/Kernel_(image_processing)#Convolution)
_Convolution_ is the process of adding each element of the image to its local neighbors, weighted by the [kernel](#kernel-image-processing). This is related to a form of mathematical convolution. The matrix operation being performedconvolutionis not traditional matrix multiplication, despite being similarly denoted by `*`.

## [Convolutional Neural Network](https://en.wikipedia.org/wiki/Convolutional_neural_network)
In [deep learning](#deep-learning), a _convolutional neural network_ (_CNN_, or _ConvNet_) is a class of deep [neural networks](#artificial-neural-network), most commonly applied to analyzing visual imagery. They are also known as _shift invariant_ or _space invariant artificial neural networks_ (_SIANN_), based on their shared-weights architecture and translation invariance characteristics. They have applications in image and video recognition, recommender systems, image classification, medical image analysis, natural language processing, brain-computer interfaces, and financial time series.

## [CPU Cache](https://en.wikipedia.org/wiki/CPU_cache)
A hardware cache used by the [CPU](#central-processing-unit) of a computer to reduce the average cost (time or energy) to access data from the main memory. This cache is a smaller, faster memory, located closer to a processor core, which stores copies of the data from frequently used memory locations. Most CPUs have different independent caches, including instruction and data cahces, where the data cache is usually organized as a hierarchy of more cache levels (L1, L2, L3, L4, etc.).

## [Cryptanalysis](https://en.wikipedia.org/wiki/Cryptanalysis)
_Cryptanalysis_ is the study of analyzing information systems in order to study the hidden aspects of the systems. Cryptanalysis is used to breach cryptographic security systems and gain access to the contents of [encrypted messages](#encryption), even if the cryptographic key is unknown.

In addition to mathematical analysis of [cryptographic](#cryptography) [algorithms](#algorithm), cryptanalysis includes the study of side-channel attacks that do not target weaknesses in the cryptographic algorithms themselves, but instead exploit weaknesses in their implementation.

## [Cryptography](https://en.wikipedia.org/wiki/Cryptography)
_Cryptography_, or _cryptology_, is the practice and study of techniques for secure communication in the presence of third parties called adversaries. More generally, cryptography is about constructing and analyzing protocols that prevent third parties or the public from reading private messages; various aspects in information security such as data confidentiality, data integrity, authentication, and non-repudiation are central to modern cryptography. Modern cryptography exists at the intersection of the disciplines of mathematics, computer science, electrical engineering, communication science, and physics. Applications of cryptography include electronic commerce, chip-based payment cards, digital currencies, computer passwords, and military communications.

## [CSS](https://en.wikipedia.org/wiki/CSS)
_Cascading Style Sheets_ (_CSS_) is a style sheet language used for describing the presentation of a document written in a markup language such as [HTML](#html). CSS is a cornerstone technology of the [World Wide Web](#world-wide-web), alongside HTML and JavaScript.

CSS is designed to enable the separation of presentation and content, including layout, colors, and fonts. This separation can improve content accessibility, provide more flexibility and control in the specification of presentation characteristics, enable multiple [web pages](#web-page) to share formatting by specifying the relevant CSS in a separate `.css` file which reduces complexity and repetition in the structural content as well as enabling the `.css` file to be cached to improve the page load speed between the pages that share the file and its formatting.

## [Cut, Copy, and Paste](https://en.wikipedia.org/wiki/Cut,_copy,_and_paste)
In humancomputer interaction and user interface design, _cut, copy, and paste_ are related commands that offer an interprocess communication technique for transferring data through a computer's user interface. The _cut_ command removes the selected data from its original position, while the _copy_ command creates a duplicate; in both cases the selected data is kept in temporary storage (the clipboard). The data from the clipboard is later inserted wherever a _paste_ command is issued. The data remains available to any application supporting the feature, thus allowing easy data transfer between applications.

The command names are an interface metaphor based on the physical procedure used in manuscript editing to create a page layout.

This interaction technique has close associations with related techniques in [graphical user interfaces](#graphical-user-interface) (GUIs) that use pointing devices such as a [computer mouse](#computer-mouse) (by drag and drop, for example). Typically, clipboard support is provided by an [operating system](#operating-system) as part of its GUI and widget toolkit.

## [Cybercrime](https://en.wikipedia.org/wiki/Cybercrime)
_Cybercrime_, or _computer-oriented crime_, is a crime that involves a computer and a [network](#computer-network). The computer may have been used in the commission of a crime, or it may be the target. Cybercrime may threaten a person, company or a nation's security and financial health.

## [Cyberwarfare](https://en.wikipedia.org/wiki/Cyberwarfare)
_Cyberwarfare_ is the use of digital attacks to attack a nation, causing comparable harm to actual warfare and or disrupting the vital computer systems. There is significant debate among experts regarding the definition of cyberwarfare, and even if such a thing exists. One view is that the term "cyberwarfare" is a misnomer, since no offensive cyber actions to date could be described as "war". An alternative view is that "cyberwarfare" is a suitable label for cyber attacks which cause physical damage to people and objects in the real world.

While there is debate over how to define and use "cyberwarfare" as a term, many countries including the United States, United Kingdom, Russia, India, Pakistan, China, Israel, Iran, and North Korea have active cyber capabilities for offensive and defensive operations. As states explore the use of cyber operations and combine capabilities the likelihood of physical confrontation and violence playing out as a result of, or part of, a cyber operation is increased. However, meeting the scale and protracted nature of war is unlikely, thus ambiguity remains.

## [Data Compression](https://en.wikipedia.org/wiki/Data_compression)
In signal processing, _data compression_, _source coding_, or _bit-rate reduction_ is the process of encoding information using fewer [bits](#bit) than the original representation. Any particular compression is either [lossy](#lossy-compression) or [lossless](#lossless-compression). Lossless compression reduces bits by identifying and eliminating statistical redundancy. No information is lost in lossless compression. Lossy compression reduces bits by removing unnecessary or less important information. Typically, a device that performs data compression is referred to as an encoder, and one that performs the reversal of the process (decompression) as a decoder.

The process of reducing the size of a [data file](#file) is often referred to as data compression. In the context of data transmission, it is called source coding; encoding done at the source of the data before it is stored or transmitted. Source coding should not be confused with channel coding, for error detection and correction or line coding, the means for mapping data onto a signal.

Compression is useful because it reduces resources required to store and transmit data. Computational resources are consumed in the compression and decompression processes. Data compression is subject to a spacetime complexity trade-off. For instance, a compression scheme for video may require expensive hardware for the video to be decompressed fast enough to be viewed as it is being decompressed, and the option to decompress the video in full before watching it may be inconvenient or require additional storage. The design of data compression schemes involves trade-offs among various factors, including the degree of compression, the amount of distortion introduced (when using lossy data compression), and the computational resources required to compress and decompress the data.

## [Data Encryption Standard](https://en.wikipedia.org/wiki/Data_Encryption_Standard)
The _Data Encryption Standard_ (_DES_) is a [symmetric-key algorithm](#symmetric-key-algorithm) for the [encryption](#encryption) of digital data. Although its short key length of 56 bits makes it too insecure for applications, it has been highly influential in the advancement of [cryptography](#cryptography).

Developed in the early 1970s at IBM and based on an earlier design by Horst Feistel, the algorithm was submitted to the National Bureau of Standards (NBS) following the agency's invitation to propose a candidate for the protection of sensitive, unclassified electronic government data. In 1976, after consultation with the National Security Agency (NSA), the NBS selected a slightly modified version (strengthened against differential cryptanalysis, but weakened against brute-force attacks), which was published as an official Federal Information Processing Standard (FIPS) for the United States in 1977.

## [Data Link Layer](https://en.wikipedia.org/wiki/Data_link_layer)
The _data link layer_, or _layer 2_, is the second layer of the seven-layer OSI model of [computer networking](#computer-network). This layer is the protocol layer that transfers data between nodes on a network segment across the [physical layer](#physical-layer). The data link layer provides the functional and procedural means to transfer data between network entities and might provide the means to detect and possibly correct errors that may occur in the physical layer.

## [Data Structure](https://en.wikipedia.org/wiki/Data_structure)
A _data structure_ is a data organization, management, and storage format that enables efficient access and modificaton. More precisely, a data structure is a collection of data values, the relationships among them, and the functions or operations that can be applied to the data.

## [Debugging](https://en.wikipedia.org/wiki/Debugging)
In computer programming and software development, _debugging_ is the process of finding and resolving bugs (defects or problems that prevent correct operation) within computer programs, software, or systems.

Debugging tactics can involve interactive debugging, [control flow](#control-flow) analysis, unit testing, integration testing, log file analysis, monitoring at the application or system level, memory dumps, and profiling. Many programming languages and software development tools also offer programs to aid in debugging, known as _debuggers_.

## [Decision Tree](https://en.wikipedia.org/wiki/Decision_tree)
A _decision tree_ is a decision support tool that uses a [tree-like](#tree) model of decisions and their possible consequences, including chance event outcomes, resource costs, and utility. It is one way to display an [algorithm](#algorithm) that only contains conditional control statements.

Decision trees are commonly used in operations research, specifically in decision analysis, to help identify a strategy most likely to reach a goal, but are also a popular tool in [machine learning](#machine-learning).

## [Deep Learning](https://en.wikipedia.org/wiki/Deep_learning)
_Deep learning_ (also known as _deep structured learning_) is part of a broader family of [machine learning](#machine-learning) methods based on [artificial neural networks](#artificial-neural-network) with representation learning. Learning can be supervised, semi-supervised or unsupervised.

Deep-learning architectures such as deep neural networks, deep belief networks, recurrent neural networks and convolutional neural networks have been applied to fields including computer vision, machine vision, speech recognition, natural language processing, audio recognition, social network filtering, machine translation, bioinformatics, drug design, medical image analysis, material inspection and board game programs, where they have produced results comparable to and in some cases surpassing human expert performance.

## [Defense in Depth](https://en.wikipedia.org/wiki/Defense_in_depth_(computing))
_Defense in depth_ is a concept used in Information security in which multiple layers of security controls (defense) are placed throughout an information technology (IT) system. Its intent is to provide redundancy in the event a security control fails or a vulnerability is exploited that can cover aspects of personnel, procedural, technical and physical security for the duration of the system's life cycle.

## [Defragmentation](https://en.wikipedia.org/wiki/Defragmentation)
In the maintenance of [file systems](#file-system), _defragmentation_ is a process that reduces the degree of [fragmentation](#file-system-fragmentation). It does this by physically organizing the contents of the mass storage device used to store [files](#file) into the smallest number of contiguous regions (fragments, extents). It also attempts to create larger regions of free space using compaction to impede the return of fragmentation. Some defragmentation utilities try to keep smaller files within a single directory together, as they are often accessed in sequence.

Defragmentation is advantageous and relevant to [file systems](#file-system) on electromechanical disk drives ([hard disk drives](#hard-disk-drive), [floppy disk](#floppy-disk) drives and optical disk media). The movement of the hard drive's read/write heads over different areas of the disk when accessing fragmented files is slower, compared to accessing the entire contents of a non-fragmented file sequentially without moving the read/write heads to seek other fragments.

## [Delay Line Memory](https://en.wikipedia.org/wiki/Delay_line_memory)
Delay line memory is a form of computer memory, now obsolete, that was used on some of the earliest digital computers. Like many modern forms of electronic computer memory, delay line memory was a refreshable memory, but as opposed to modern random-access memory, delay line memory was [sequential-access](#sequential-access-memory).

Analog delay line technology had been used since the 1920s to delay the propagation of analog signals. When a delay line is used as a memory device, an amplifier and a pulse shaper are connected between the output of the delay line and the input. These devices recirculate the signals from the output back into the input, creating a loop that maintains the signal as long as power is applied. The shaper ensures the pulses remain well-formed, removing any degradation due to losses in the medium.

The memory capacity is determined by dividing the time taken to transmit one bit into the time it takes for data to circulate through the delay line. Early delay-line memory systems had capacities of a few thousand bits, with recirculation times measured in microseconds. To read or write a particular bit stored in such a memory, it is necessary to wait for that bit to circulate through the delay line into the electronics. The delay to read or write any particular bit is no longer than the recirculation time.

## [Dependent and Independent Variables](https://en.wikipedia.org/wiki/Dependent_and_independent_variables)
_Dependent and independent variables_ are variables in mathematical modeling, statistical modeling and experimental sciences. _Dependent variables_ receive this name because, in an experiment, their values are studied under the supposition or hypothesis that they depend, by some law or rule (e.g., by a mathematical function), on the values of other variables. _Independent variables_, in turn, are not seen as depending on any other variable in the scope of the experiment in question; thus, even if the existing dependency is invertible (e.g., by finding the inverse function when it exists), the nomenclature is kept if the inverse dependency is not the object of study in the experiment. In this sense, some common independent variables are time, space, density, mass, fluid flow rate, and previous values of some observed value of interest (e.g. human population size) to predict future values (the dependent variable). Variables are given a special name that only applies to experimental investigations. The independent variable is the variable the experimenter changes or controls and is assumed to have a direct effect on the dependent variable. Two examples of common independent variables are gender and educational level.

## [Denial-Of-Service Attack](https://en.wikipedia.org/wiki/Denial-of-service_attack)
In computing, a _denial-of-service attack_ (_DoS attack_) is a cyber-attack in which the perpetrator seeks to make a machine or [network](#computer-network) resource unavailable to its intended users by temporarily or indefinitely disrupting services of a host connected to the Internet. Denial of service is typically accomplished by flooding the targeted machine or resource with superfluous requests in an attempt to overload systems and prevent some or all legitimate requests from being fulfilled.

In a _distributed denial-of-service attack_ (_DDoS attack_), the incoming traffic flooding the victim originates from many different sources. This effectively makes it impossible to stop the attack simply by blocking a single source.

A DoS or DDoS attack is analogous to a group of people crowding the entry door of a shop, making it hard for legitimate customers to enter, thus disrupting trade.

## [Desktop Metaphor](https://en.wikipedia.org/wiki/Desktop_metaphor)
In computing, the _desktop metaphor_ is an interface metaphor which is a set of unifying concepts used by [graphical user interfaces](#graphical-user-interface) to help users interact more easily with the computer. The desktop metaphor treats the computer monitor as if it is the top of the user's desk, upon which objects such as documents and folders of documents can be placed. A document can be opened into a [window](#window), which represents a paper copy of the document placed on the desktop. Small applications called desk accessories are also available, such as a desk calculator or notepad, etc.

The desktop metaphor itself has been extended and stretched with various implementations of desktop environments, since access to features and usability of the computer are usually more important than maintaining the 'purity' of the metaphor. Hence we find trash cans on the desktop, as well as disks and [network](#computer-network) volumes (which can be thought of as filing cabinets - not something normally found on a desktop). Other features such as menu bars or taskbars have no direct counterpart on a real-world desktop, though this may vary by environment and the function provided; for instance, a familiar wall calendar can sometimes be displayed or otherwise accessed via a taskbar or menu bar belonging to the desktop.

## [Determinism](https://en.wikipedia.org/wiki/Determinism)
_Determinism_ is the philosophical view that all events are determined completely by previously existing causes. Deterministic theories throughout the history of philosophy have sprung from diverse and sometimes overlapping motives and considerations. The opposite of determinism is some kind of indeterminism (otherwise called nondeterminism) or randomness. Determinism is often contrasted with free will.

Determinism often is taken to mean causal determinism, which in physics is known as cause-and-effect. It is the concept that events within a given paradigm are bound by causality in such a way that any state (of an object or event) is completely determined by prior states. This meaning can be distinguished from other varieties of determinism mentioned below.

## [Device Driver](https://en.wikipedia.org/wiki/Device_driver)
In computing, a _device driver_ is a computer program that operates or controls a particular type of device that is attached to a computer. A driver provides a software interface to hardware devices, enabling operating systems and other computer programs to access hardware functions without needing to know precise details about the hardware being used.

A driver communicates with the device through the computer [bus](#bus) or communications subsystem to which the hardware connects. When a calling program invokes a routine in the driver, the driver issues commands to the device. Once the device sends data back to the driver, the driver may invoke routines in the original calling program.

Drivers are hardware dependent and operating-system-specific. They usually provide interrupt handling required for any necessary asynchronous time-dependent hardware behavior.

## [Dialogue System](https://en.wikipedia.org/wiki/Dialogue_system)
A _dialogue system_, or _conversational agent_ (_CA_), is a computer system intended to converse with a human. Dialogue systems employed one or more of text, speech, graphics, haptics, gestures, and other modes for communication on both the input and output channel.

The elements of a dialogue system are not defined, however they are different from [chatbot](#chatbot). The typical GUI wizard engages in a sort of dialog, but it includes very few of the common dialogue system components, and dialog state is trivial.

## [Diffie-Hellman Key Exchange](https://en.wikipedia.org/wiki/Diffie%E2%80%93Hellman_key_exchange)
_Diffie-Hellman key exchange_ is a method of securely exchanging [cryptographic](#cryptography) keys over a public channel and was one of the first [public-key protocols](#public-key-cryptography) as conceived by Ralph Merkle and named after Whitfield Diffie and Martin Hellman. DH is one of the earliest practical examples of public [key exchange](#key-exchange) implemented within the field of cryptography. Published in 1976 by Diffie and Hellman, this is the earliest publicly known work that proposed the idea of a private key and a corresponding public key.

Traditionally, secure encrypted communication between two parties required that they first exchange keys by some secure physical means, such as paper key lists transported by a trusted courier. The DiffieHellman key exchange method allows two parties that have no prior knowledge of each other to jointly establish a shared secret key over an insecure channel. This key can then be used to encrypt subsequent communications using a [symmetric key cipher](#symmetric-key-algorithm).

## [Diode](https://en.wikipedia.org/wiki/Diode)
An electronic component that permits the one-way flow of current. A diode is a kind of [vacuum tube](#vacuum-tube).

## [Dirty Bit](https://en.wikipedia.org/wiki/Dirty_bit)
A [bit](#bit) that is associated with a block of computer memory and indicates whether or not the corresponding block of memory has been modified. The dirty bit is set when the processor writes to (modifies) this memory. The bit indicates that its associated block of memory has been modified and has not been saved to storage yet. When a block of memory is to be replaced, its corresponding dirty bit is checked to see if the block needs to be written back to secondary memory before being replaced or if it can simply be removed. Dirty bits are used by the [CPU cache](#cpu-cache) and in the page replacement [algorithms](#algorithm) of an operating system.

## [Discrete Component](https://www.pcmag.com/encyclopedia/term/discrete-component)
An elementary electronic device constructed as a single unit. Before the advent of [integrated circuits](#integrated-circuit) (chips), all [transistors](#transistor), [resistors](#resistor), [capacitors](#capacitor), and [diodes](#diode) were discrete. Discrete components are widely used in amplifiers and other electronic products that use large amounts of current. On a circuit board, they are intermingled with the chips, and there is hardly any electronic product that does not have at least one or two discrete resistors or capacitors.

## [Domain Name System](https://en.wikipedia.org/wiki/Domain_Name_System)
The _Domain Name System_ (_DNS_) is a hierarchical and decentralized naming system for computers, services, or other resources connected to the [Internet](#internet) or a [private network](#computer-network). It associates various information with domain names assigned to each of the participating entities. Most prominently, it translates more readily memorized domain names to the numerical [IP addresses](#ip-address) needed for locating and identifying computer services and devices with the underlying network protocols. By providing a worldwide, distributed directory service, the Domain Name System has been an essential component of the functionality of the Internet since 1985.

The Domain Name System delegates the responsibility of assigning domain names and mapping those names to Internet resources by designating authoritative name servers for each domain. Network administrators may delegate authority over sub-domains of their allocated name space to other name servers. This mechanism provides distributed and fault-tolerant service and was designed to avoid a single large central database.

The Domain Name System also specifies the technical functionality of the database service that is at its core. It defines the _DNS protocol_, a detailed specification of the data structures and data communication exchanges used in the DNS, as part of the [Internet Protocol Suite](#internet-protocol-suite).

## [Dot Matrix](https://en.wikipedia.org/wiki/Dot_matrix)
A _dot matrix_ is a 2-dimensional patterned array, used to represent characters, symbols and images. Every type of modern technology uses dot matrices for display of information, including mobile phones, televisions, and printers. They are also used in textiles with sewing, knitting and weaving.

An alternate form of information display using lines and curves is known as a vector display, was used with early computing devices such as air traffic control radar displays and pen-based plotters but is no longer used. Electronic [vector displays](#vector-display) were typically monochrome only, and either don't fill in the interiors of closed vector shapes, or shape-filling is slow, time-consuming, and often non-uniform, as on pen-based plotters.

## [Drum Memory](https://en.wikipedia.org/wiki/Drum_memory)
_Drum memory_ was a magnetic data storage device invented by [Gustav Tauschek](https://en.wikipedia.org/wiki/Gustav_Tauschek) in 1932 in Austria. Drums were widely used in the 1950s and into the 1960s as computer memory.

For many early computers, drum memory formed the main working memory of the computer. It was so common that these computers were often referred to as drum machines. Some drum memories were also used as secondary storage.

Drums were displaced as primary computer memory by magnetic core memory, which offered a better balance of size, speed, cost, reliability and potential for further improvements. Drums in turn were replaced by [hard disk drives](#hard-disk-drive) for secondary storage, which were both less expensive and offered denser storage. The manufacture of drums ceased in the 1970s.

## [Dynamic Random-Access Memory](https://en.wikipedia.org/wiki/Dynamic_random-access_memory)
_DRAM_ is a type of [random access](#random-access-memory) semiconductor memory that stores each bit of data in a memory cell consisting of a tiny [capacitor](#capacitor) and transitor.

## [Emotion Recognition](https://en.wikipedia.org/wiki/Emotion_recognition)
_Emotion recognition_ is the process of identifying human emotion. People vary widely in their accuracy at recognizing the emotions of others. Use of technology to help people with emotion recognition is a relatively nascent research area. Generally, the technology works best if it uses multiple modalities in context. To date, the most work has been conducted on automating the recognition of facial expressions from video, spoken expressions from audio, written expressions from text, and physiology as measured by wearables.

## [Encryption](https://en.wikipedia.org/wiki/Encryption)
In [cryptography](#cryptography), _encryption_ is the process of encoding information. This process converts the original representation of the information, known as _plaintext_, into an alternative form known as _ciphertext_. Ideally, only authorized parties can decipher a ciphertext back to plaintext and access the original information. Encryption does not itself prevent interference but denies the intelligible content to a would-be interceptor. For technical reasons, an encryption scheme usually uses a pseudo-random encryption key generated by an [algorithm](#algorithm). It is possible to decrypt the message without possessing the key, but, for a well-designed encryption scheme, considerable computational resources and skills are required. An authorized recipient can easily decrypt the message with the key provided by the originator to recipients but not to unauthorized users. Historically, various forms of encryption have been used to aid in cryptography. Early encryption techniques were often utilized in military messaging. Since then, new techniques have emerged and become commonplace in all areas of modern computing. Modern encryption schemes utilize the concepts of public-key and [symmetric-key](#symmetric-key-algorithm). Modern encryption techniques ensure security because modern computers are inefficient at cracking the encryption.

## [Ethernet](https://en.wikipedia.org/wiki/Ethernet)
_Ethernet_ is a family of wired computer networking technologies commonly used in [local area networks](#local-area-network) (LAN), metropolitan area networks (MAN) and [wide area networks](#wide-area-network) (WAN). It was commercially introduced in 1980 and first standardized in 1983 as IEEE 802.3. Ethernet has since been refined to support higher bit rates, a greater number of nodes, and longer link distances, but retains much backward compatibility. Over time, Ethernet has largely replaced competing wired LAN technologies such as Token Ring, FDDI and ARCNET.

## [Event-Driven Programming](https://en.wikipedia.org/wiki/Event-driven_programming)
In computer programming, _event-driven programming_ is a [programming paradigm](../../../paradigms/overview/README.md) in which the flow of the program is determined by events such as user actions ([mouse](#computer-mouse) clicks, key presses), sensor outputs, or messages from other programs or threads. Event-driven programming is the dominant paradigm used in [graphical user interfaces](#graphical-user-interface) and other applications (e.g., JavaScript web applications) that are centered on performing certain actions in response to user input. This is also true of programming for device drivers (e.g., P in USB device driver stacks).

In an event-driven application, there is generally a main loop that listens for events and then triggers a callback function when one of those events is detected. In embedded systems, the same may be achieved using hardware interrupts instead of a constantly running main loop. Event-driven programs can be written in any programming language, although the task is easier in languages that provide high-level abstractions, such as await and closures.

## [Exploit](https://en.wikipedia.org/wiki/Exploit_(computer_security))
An _exploit_ (from the English verb to _exploit_, meaning "to use something to one’s own advantage") is a piece of software, a chunk of data, or a sequence of commands that takes advantage of a bug or vulnerability to cause unintended or unanticipated behavior to occur on computer software, hardware, or something electronic (usually computerized). Such behavior frequently includes things like gaining control of a computer system, allowing privilege escalation, or a [denial-of-service](#denial-of-service-attack) (DoS or related DDoS) attack.

## [Exponential Backoff](https://en.wikipedia.org/wiki/Exponential_backoff)
_Exponential backoff_ is an [algorithm](#algorithm) that uses [feedback](#feedback) to multiplicatively decrease the rate of some process, in order to gradually find an acceptable rate.

## [Feature Detection](https://en.wikipedia.org/wiki/Feature_detection_(computer_vision))
In [computer vision](#computer-vision) and image processing _feature detection_ includes methods for computing abstractions of image information and making local decisions at every image point whether there is an image feature of a given type at that point or not. The resulting features will be subsets of the image domain, often in the form of isolated points, continuous curves or connected regions.

## [Feedback](https://en.wikipedia.org/wiki/Feedback)
_Feedback_ occurs when outputs of a system are routed back as inputs as part of a chain of cause-and-effect that forms a circuit or loop. The system can then be said to feed back into itself. The notion of cause-and-effect has to be handled carefully when applied to feedback systems:

> Simple causal reasoning about a feedback system is difficult because the first system influences the second and second system influences the first, leading to a circular argument. This makes reasoning based upon cause and effect tricky, and it is necessary to analyze the system as a whole.

Karl Johan Åström and Richard M.Murray, Feedback Systems: An Introduction for Scientists and Engineers

## [File](https://en.wikipedia.org/wiki/Computer_file)
A _computer file_ is a computer resource for recording data discretely in a computer storage device. Just as words can be written to paper, so can information be written to a computer file. Files can be edited and transferred through the internet on that particular computer system.

There are different types of computer files, designed for different purposes. A file may be designed to store a picture, a written message, a video, a computer program, or a wide variety of other kinds of data. Some types of files can store several types of information at once.

By using computer programs, a person can open, read, change, save, and close a computer file. Computer files may be reopened, modified, and copied an arbitrary number of times.

Typically, files are organised in a [file system](#file-system), which keeps track of where the files are located on [disk](#hard-disk-drive) and enables user access.

## [File Extension](https://en.wikipedia.org/wiki/Filename_extension)
A _filename extension_ or _file type_ is an identifier specified as a suffix to the name of a [computer file](#file). The extension indicates a characteristic of the file contents or its intended use. A filename extension is typically delimited from the filename with a full stop (period), but in some systems it is separated with spaces.

Some [file systems](#file-system) implement filename extensions as a feature of the file system itself and may limit the length and format of the extension, while others treat filename extensions as part of the filename without special distinction.

## [File Format](https://en.wikipedia.org/wiki/File_format)
A _file format_ is a standard way that information is encoded for storage in a [computer file](#file). It specifies how [bits](#bit) are used to encode information in a digital storage medium. File formats may be either proprietary or free and may be either unpublished or open.

Some file formats are designed for very particular types of data: _PNG_ files, for example, store bitmapped images using lossless data compression. Other file formats, however, are designed for storage of several different types of data: the _Ogg_ format can act as a container for different types of multimedia including any combination of audio and video, with or without text (such as subtitles), and metadata. A text file can contain any stream of characters, including possible control characters, and is encoded in one of various character encoding schemes. Some file formats, such as _[HTML](#html)_, scalable vector graphics, and the source code of computer software are text files with defined syntaxes that allow them to be used for specific purposes.

## [File System](https://en.wikipedia.org/wiki/File_system)
In computing, a _file system_ or _filesystem_ (often abbreviated to _fs_) controls how data is stored and retrieved. Without a file system, data placed in a storage medium would be one large body of data with no way to tell where one piece of data stops and the next begins. By separating the data into pieces and giving each piece a name, the data is easily isolated and identified. Taking its name from the way paper-based data management system is named, each group of data is called a ["file."](#file) The structure and logic rules used to manage the groups of data and their names is called a "file system."

There are many different kinds of file systems. Each one has different structure and logic, properties of speed, flexibility, security, size and more. Some file systems have been designed to be used for specific applications.

## [File System Fragmentation](https://en.wikipedia.org/wiki/File_system_fragmentation)
In computing, _file system fragmentation_, sometimes called _file system aging_, is the tendency of a [file system](#file-system) to lay out the contents of [files](#file) non-continuously to allow in-place modification of their contents. It is a special case of data fragmentation. File system fragmentation increases [disk](#hard-disk-drive) head movement or seek time (where it applies), which are known to hinder throughput. In addition, file systems cannot sustain unlimited fragmentation. The correction to existing fragmentation is to reorganize files and free space back into contiguous areas, a process called [defragmentation](#defragmentation).

In modern computers, with [SSD "disks"](#solid-state-electronics) that do not rotate and are not really discs in the conventional sense, file system fragmentation is not as much of a performance problem (that should be "fixed"), as there is no movement of heads or discs. In fact, overly defragmenting such drives can slowly shorten their lifespan.

## [Fillrate](https://en.wikipedia.org/wiki/Fillrate)
The term pixel _fillrate_ refers to the number of pixels a [video card](#video-card) can render to screen and write to video memory in a second or in case of texture fillrate the number of texture map elements (texels) [GPU](#graphics-processing-unit) can map to pixels in a second. Pixel fillrates are given in megapixels per second or in gigapixels per second (in the case of newer cards), and they are obtained by multiplying the number of Raster Output Units (ROPs) by the clock frequency of the graphics processor unit (GPU) of a video card and texture fillrate is obtained by multiplying the number of Texture Mapping Units (TMUs) by the clock frequency of the graphics processing unit (GPU). Texture fillrates are given in mega or gigatexels per second. However, there is no full agreement on how to calculate and report fillrates. Other possible method is: to multiply the number of pixel pipelines by the clock frequency. The results of these multiplications correspond to a theoretical number. The actual fillrate depends on many other factors. In the past, the fillrate has been used as an indicator of performance by video card manufacturers such as ATI and NVIDIA, however, the importance of the fillrate as a measurement of performance has declined as the bottleneck in graphics applications has shifted. For example, today, the number and speed of unified shader processing units has gained attention.

## [Flat Shading](https://en.wikipedia.org/wiki/Shading#Flat_shading)
Here, the lighting is evaluated only once for each polygon (usually for the first vertex in the polygon, but sometimes for the centroid for [triangle meshes](#polygon-mesh)), based on the polygon's surface normal and on the assumption that all polygons are flat. The computed color is used for the whole polygon, making the corners look sharp. This is usually used when more advanced shading techniques are too computationally expensive. Specular highlights are rendered poorly with _flat shading_: If there happens to be a large specular component at the representative vertex, that brightness is drawn uniformly over the entire face. If a specular highlight doesn't fall on the representative point, it is missed entirely. Consequently, the specular reflection component is usually not included in flat shading computation.

## [Flip-Flop](https://en.wikipedia.org/wiki/Flip-flop_(electronics))
A _flip-flop_, or _latch_, is a circuit that has two stable states that can be used to store state information. The circuit can be made to change state by signals applied to one more more control inputs and will have one or two outputs.

## [Floating Point](https://en.wikipedia.org/wiki/Floating-point_arithmetic)
Arithmetic using formulaic representation of real numbers as an approximaton to support a trade-off between range and precision. The term _floating point_ refers to the fact that a number's radix point (decimal point) can "float" anywhere relative to the significant digits of the number.

## [Floppy Disk](https://en.wikipedia.org/wiki/Floppy_disk)
A _floppy disk_ or _floppy diskette_ (sometimes casually referred to as a _floppy_ or _diskette_) is a type of disk storage composed of a thin and flexible disk of a magnetic storage medium in a square or nearly square plastic enclosure lined with a fabric that removes dust particles from the spinning disk. Floppy disks are read from and written to by a floppy disk drive (FDD).

## [FLOPS](https://en.wikipedia.org/wiki/FLOPS)
_Floating point operations per second_ (_FLOPS_, _flops_, or _flop/s_) is a measure of computer performance, useful in fields of scientific computations that require [floating point](#floating-point) calculations. For such cases it is a more accurate measure than measuring instructions per second.

## [Formant](https://en.wikipedia.org/wiki/Formant)
In speech science and phonetics, a _formant_ is the broad spectral maximum that results from an acoustic resonance of the human vocal tract. In acoustics, a formant is usually defined as a broad peak, or local maximum, in the spectrum. For harmonic sounds, with this definition, the formant frequency is sometimes taken as that of the harmonic partial that is most augmented by a resonance. The difference between these two definitions resides in whether "formants" characterise the production mechanisms of a sound or the produced sound itself. In practice, the frequency of a spectral peak differs from the associated resonance frequency, except when, by luck, harmonics are aligned with the resonance frequency.

## [Framebuffer](https://en.wikipedia.org/wiki/Framebuffer)
A _framebuffer_ (_frame buffer_, or sometimes _framestore_) is a portion of [random-access memory](#random-access-memory) (_RAM_) containing a [bitmap](#bitmap) that drives a video display. It is a memory buffer containing data representing all the [pixels](#pixel) in a complete video frame. Modern [video cards](#video-card) contain framebuffer circuitry in their cores. This circuitry converts an in-memory bitmap into a video signal that can be displayed on a computer monitor.

In computing, a _screen buffer_ is a part of computer memory used by a computer application for the representation of the content to be shown on the computer display. The screen buffer may also be called the _video buffer_, the _regeneration buffer_, or _regen buffer_ for short. Screen buffers should be distinguished from video memory. To this end, the term off-screen buffer is also used.

## [Full Adder](https://en.wikipedia.org/wiki/Adder_(electronics)#Full_adder)
A _full adder_ is a circuit that adds binary numbers and accounts for values carried in as well as out. A one-bit full adder adds three one-bit numbers (A, B, and C<sub>in</sub>). A and B are the [operands](#operand), and C<sub>in</sub> is a bit carried from the previous stage.

## [Graph](https://en.wikipedia.org/wiki/Graph_(discrete_mathematics))
In mathematics, and more specifically in _graph theory_, a _graph_ is a structure amounting to a set of objects in which some pairs of the objects are in some sense "related". The objects correspond to mathematical abstrcations called _vertices_ (also called _nodes_ or _points_) and each of the related pairs of vertices is called an _edge_ (also called _link_ or _line_). Typically, a graph is represented in diagrammatic form as a set of dots or circles for the vertices, joined by lines or curves for the edges. Graphes are one of the objects of study in discrete mathematics.

![A graph with six vertices and seven edges](./graph.svg)
<br />
A graph with six vertices and seven edges.

## [Graph Traversal](https://en.wikipedia.org/wiki/Graph_traversal)
_Graph traversal_, also known as _graph search_, refers to the process of visiting (checking and/or updating) each vertex in a graph. Such traversals are classified by the order in which the vertices are visited. _Tree traversal_ is a special case of graph traversal.

## [Graphical User Interface](https://en.wikipedia.org/wiki/Graphical_user_interface)
The _graphical user interface_ is a form of user interface that allows users to interact with electronic devices through graphical icons and audio indicator such as primary notation, instead of text-based user interfaces, typed command labels or text navigation. _GUIs_ were introduced in reaction to the perceived steep learning curve of [command-line interfaces](#command-line-interface) (CLIs), which require commands to be typed on a computer keyboard.

The actions in a GUI are usually performed through direct manipulation of the graphical elements. Beyond computers, GUIs are used in many handheld mobile devices such as MP3 players, portable media players, gaming devices, smartphones and smaller household, office and industrial controls. The term GUI tends not to be applied to other lower-display resolution types of interfaces, such as video games (where head-up display (HUD) is preferred), or not including flat screens, like volumetric displays because the term is restricted to the scope of two-dimensional display screens able to describe generic information, in the tradition of the computer science research at the Xerox Palo Alto Research Center.

## [Graphics Library](https://en.wikipedia.org/wiki/Graphics_library)
A _graphics library_ is a program library designed to aid in rendering computer graphics to a monitor. This typically involves providing optimized versions of functions that handle common rendering tasks. This can be done purely in software and running on the [CPU](#central-processing-unit), common in embedded systems, or being hardware accelerated by a [GPU](#graphics-processing-unit), more common in PCs. By employing these functions, a program can assemble an image to be output to a monitor. This relieves the programmer of the task of creating and optimizing these functions, and allows them to focus on building the graphics program. Graphics libraries are mainly used in video games and simulations.

## [Graphics Processing Unit](https://en.wikipedia.org/wiki/Graphics_processing_unit)
A _graphics processing unit_ (_GPU_) is a specialized, electronic circuit designed to rapidly manipulate and alter memory to accelerate the creation of images in a frame buffer intended for output to a display device. GPUs are used in embedded systems, mobile phones, personal computers, workstations, and game consoles. Modern GPUs are very efficient at manipulating computer graphics and image processing. Their highly parallel structure makes them more efficient than general-purpose [central processing units](#central-processing-unit) (CPUs) for [algorithms](#algorithm) that process large blocks of data in parallel. In a [personal computer](#personal-computer), a GPU can be present on a [video card](#video-card) or embedded on the [motherboard](#motherboard). In certain CPUs, they are embedded on the CPU die.

## [Hacktivism](https://en.wikipedia.org/wiki/Hacktivism)
In Internet activism, _hacktivism_, or _hactivism_ (a portmanteau of [hack](#security-hacker) and activism), is the use of computer-based techniques such as hacking as a form of civil disobedience to promote a political agenda or social change. With roots in hacker culture and hacker ethics, its ends are often related to free speech, human rights, or freedom of information movements.

## [Half Adder](https://en.wikipedia.org/wiki/Adder_(electronics)#Half_adder)
The _half adder_ is a circuit that adds two single binary digits A and B. It has two outputs, sum (S) and carry (C). The carry signal represents an overflow into the next digit of a multi-digit addition.

## [Hard Disk Drive](https://en.wikipedia.org/wiki/Hard_disk_drive)
A _hard disk drive_ (_HDD_), _hard disk_, _hard drive_, or _fixed disk_ is an electro-mechanical data storage device that stores and retrieves digital data using magnetic storage and one or more rigid rapidly rotating platters coated with magnetic material. The platters are paired with magnetic heads, usually arranged on a moving actuator arm, which read and write data to the platter surfaces. Data is accessed in a random-access manner, meaning that individual blocks of data can be stored and retrieved in any order. HDDs are a type of non-volatile storage, retaining stored data even when powered off.

## [Header](https://en.wikipedia.org/wiki/Header_(computing))
In information technology, _header_ refers to supplemental data placed at the beginning of a block of data being stored or transmitted. In data transmission, the data following the header is sometimes called the payload or body.

It is vital that header composition follows a clear and unambiguous specification or format, to allow for parsing.

## [HTML](https://en.wikipedia.org/wiki/HTML)
_Hypertext Markup Language_ (_HTML_) is the standard markup language for documents designed to be displayed in a [web browser](#web-browser). It can be assisted by technologies such as [Cascading Style Sheets](#css) (CSS) and scripting languages such as JavaScript.

Web browsers receive HTML documents from a [web server](#web-server) or from local storage and render the documents into multimedia [web pages](#web-page). HTML describes the structure of a web page semantically and originally included cues for the appearance of the document.

## [Huffman Coding](https://en.wikipedia.org/wiki/Huffman_coding)
In computer science and information theory, a _Huffman code_ is a particular type of optimal [prefix code](#prefix-code) that is commonly used for [lossless](#lossless-compression) [data compression](#data-compression). The process of finding or using such a code proceeds by means of Huffman coding, an [algorithm](#algorithm) developed by [David A. Huffman](https://en.wikipedia.org/wiki/David_A._Huffman) while he was a Sc.D. student at MIT, and published in the 1952 paper "A Method for the Construction of Minimum-Redundancy Codes".

The output from Huffman's algorithm can be viewed as a variable-length code table for encoding a source symbol (such as a character in a file). The algorithm derives this table from the estimated probability or frequency of occurrence (weight) for each possible value of the source symbol. As in other entropy encoding methods, more common symbols are generally represented using fewer bits than less common symbols. Huffman's method can be efficiently implemented, finding a code in time linear to the number of input weights if these weights are sorted. However, although optimal among methods encoding symbols separately, Huffman coding is not always optimal among all compression methods - it is replaced with arithmetic coding or asymmetric numeral systems if better compression ratio is required.

## [Human-In-The-Loop](https://en.wikipedia.org/wiki/Human-in-the-loop)
_Human-in-the-loop_ or _HITL_ is defined as a model that requires human interaction. HITL is associated with modeling and simulation (M&S) in the live, virtual, and constructive taxonomy. HITL models may conform to human factors requirements as in the case of a mockup. In this type of simulation a human is always part of the simulation and consequently influences the outcome in such a way that is difficult if not impossible to reproduce exactly. HITL also readily allows for the identification of problems and requirements that may not be easily identified by other means of simulation.

HITL is often referred to as interactive simulation, which is a special kind of physical simulation in which physical simulations include human operators, such as in a flight or a driving simulator.

## [Human-Computer Interaction](https://en.wikipedia.org/wiki/Human%E2%80%93computer_interaction)
_Human-computer interaction_ (_HCI_) studies the design and use of computer technology, focused on the interfaces between people (users) and computers. Researchers in the field of HCI observe the ways in which humans interact with computers and design technologies that let humans interact with computers in novel ways.

## [Human-Robot Interaction](https://en.wikipedia.org/wiki/Human%E2%80%93robot_interaction)
_Human-robot interaction_ is the study of interactions between humans and [robots](#robot). It is often referred as _HRI_ by researchers. Human-robot interaction is a multidisciplinary field with contributions from [human-computer interaction](#human-computer-interaction), [artificial intelligence](#artificial-intelligence), robotics, [natural language](#natural-language) understanding, design, humanities and social sciences.

## [Hyperlink](https://en.wikipedia.org/wiki/Hyperlink)
_Hyperlink_ computing, a _hyperlink_, or simply a _link_, is a reference to data that the user can follow by clicking or tapping. A hyperlink points to a whole document or to a specific element within a document. [Hypertext](#hypertext) is text with hyperlinks. The text that is linked from is called anchor text. A software system that is used for viewing and creating hypertext is a hypertext system, and to create a hyperlink is to hyperlink (or simply to link). A user following hyperlinks is said to navigate or browse the hypertext.

## [Hypertext](https://en.wikipedia.org/wiki/Hypertext)
_Hypertext_ is text displayed on a computer display or other electronic devices with references ([hyperlinks](#hyperlink)) to other text that the reader can immediately access. Hypertext documents are interconnected by hyperlinks, which are typically activated by a mouse click, keypress set, or by touching the screen. Apart from text, the term "hypertext" is also sometimes used to describe tables, images, and other presentational content formats with integrated hyperlinks. Hypertext is one of the key underlying concepts of the [World Wide Web](#world-wide-web), where [Web pages](#web-page) are often written in the [Hypertext Markup Language](#html) (HTML). As implemented on the Web, hypertext enables the easy-to-use publication of information over the [Internet](#internet).

## [Hypertext Transfer Protocol](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol)
The _Hypertext Transfer Protocol_ (_HTTP_) is an application layer protocol for distributed, collaborative, hypermedia information systems. HTTP is the foundation of data communication for the [World Wide Web](#world-wide-web), where [hypertext](#hypertext) documents include [hyperlinks](#hyperlink) to other resources that the user can easily access, for example by a mouse click or by tapping the screen in a [web browser](#web-browser).

## [Icon](https://en.wikipedia.org/wiki/Icon_(computing))
In computing, an _icon_ is a pictogram or ideogram displayed on a computer screen in order to help the user navigate a computer system. The icon itself is a quickly comprehensible symbol of a software tool, function, or a data [file](#file), accessible on the system and is more like a traffic sign than a detailed illustration of the actual entity it represents. It can serve as an electronic [hyperlink](#hyperlink) or file shortcut to access the program or data. The user can activate an icon using a [mouse](#computer-mouse), pointer, finger, or recently voice commands. Their placement on the screen, also in relation to other icons, may provide further information to the user about their usage. In activating an icon, the user can move directly into and out of the identified function without knowing anything further about the location or requirements of the file or code.

## [Industrial Robot](https://en.wikipedia.org/wiki/Industrial_robot)
An _industrial robot_ is a [robot](#robot) system used for manufacturing. Industrial robots are automated, programmable and capable of movement on three or more axes.

Typical applications of robots include welding, painting, assembly, disassembly, pick and place for [printed circuit boards](#printed-circuit-board), packaging and labeling, palletizing, product inspection, and testing; all accomplished with high endurance, speed, and precision. They can assist in material handling.

## [Input/Output](https://en.wikipedia.org/wiki/Input/output)
In computing, _input/output_ or _I/O_ (or, informally, _io_ or _IO_) is the communication between an information processing system, such as a computer, and the outside world, possibly a human or another information processing system. Inputs are the signals or data received by the system and outputs are the signals or data sent from it. The term can also be used as part of an action; to "perform I/O" is to perform an input or output operation.

I/O devices are the pieces of hardware used by a human (or other system) to communicate with a computer. For instance, a keyboard or computer mouse is an input device for a computer, while monitors and printers are output devices. Devices for communication between computers, such as modems and network cards, typically perform both input and output operations.

The designation of a device as either input or output depends on perspective. Mice and keyboards take physical movements that the human user outputs and convert them into input signals that a computer can understand; the output from these devices is the computer's input. Similarly, printers and monitors take signals that computers output as input, and they convert these signals into a representation that human users can understand. From the human user's perspective, the process of reading or seeing these representations is receiving output; this type of interaction between computers and humans is studied in the field of [humancomputer interaction](#human-computer-interaction). A further complication is that a device traditionally considered an input device, e.g., card reader, keyboard, may accept control commands to, e.g., select stacker, display keyboard lights, while a device traditionally considered as an output device may provide status data, e.g., low toner, out of paper, paper jam.

In computer architecture, the combination of the [CPU](#central-processing-unit) and main memory, to which the CPU can read or write directly using individual instructions, is considered the brain of a computer. Any transfer of information to or from the CPU/memory combo, for example by reading data from a [disk drive](#hard-disk-drive), is considered I/O. The CPU and its supporting circuitry may provide memory-mapped I/O that is used in low-level computer programming, such as in the implementation of device drivers, or may provide access to I/O channels. An I/O [algorithm](#algorithm) is one designed to exploit locality and perform efficiently when exchanging data with a secondary storage device, such as a disk drive.

## [Instruction Pipelining](https://en.wikipedia.org/wiki/Instruction_pipelining)
A technique for implementing instruction-level parallelism within a single processor. Pipelining attempts to keep every part of the processor busy with some instruction by dividing incoming instructions into a series of sequential steps performed by different processor units with different parts of instructions processed in parallel.

## [Integer Overflow](https://en.wikipedia.org/wiki/Integer_overflow)
An _integer overflow_ occurs when an arithmetic operation attempts to create a numeric value that is outside of the range that can be represented with a given number of digits - either higher than the maximum or lower than the minimum representable value. An overflow condition may give results leading to unintended behavior. If the possibility of an overflow has not been anticipated, it can compromise a program's reliability and security.

## [Integrated Circuit](https://en.wikipedia.org/wiki/Integrated_circuit)
An _integrated circuit_ or _monolithic integrated circuit_ (also referred to as an _IC_, a _chip_, or a _microchip_) is a set of electronic circuits on one small flat piece (or "chip") of semiconductor material that is normally silicon. The integration of large numbers of tiny MOS transistors into a small chip results in circuits that are orders of magnitude smaller, faster, and less expensive than those constructed of discrete electronic components. The IC's mass production capability, reliability, and building-block approach to integrated circuit design has ensured the rapid adoption of standardized ICs in place of designs using discrete transistors. ICs are now used in virtually all electronic equipment and have revolutionized the world of electronics. Computers, mobile phones, and other digital home appliances are now intextricable parts of the structure of modern societies, made possible by the small size and low cost of ICs.

## [Integrated Development Environment](https://en.wikipedia.org/wiki/Integrated_development_environment)
An _integrated development environment_ (_IDE_) is a software application that provides comprehensive facilities to computer programmers for software development. An IDE normally consists of at least a source code editor, build automation tools and a debugger. Some IDEs, Such as [NetBeans](https://en.wikipedia.org/wiki/NetBeans) and [Eclipse](https://en.wikipedia.org/wiki/Eclipse_(software)), contain the necessary [compiler](#compiler), interpreter, or both; others such as [SharpDevelop](https://en.wikipedia.org/wiki/SharpDevelop) and [Lazarus](https://en.wikipedia.org/wiki/Lazarus_(IDE)) do not.

## [Internet](https://en.wikipedia.org/wiki/Internet)
The _Internet_ (or _internet_) is the global system of interconnected [computer networks](#computer-network) that uses the [Internet protocol suite](#internet-protocol-suite) (TCP/IP) to communicate between networks and devices. It is a network of networks that consists of private, public, academic, business, and government networks of local to global scope, linked by a broad array of electronic, wireless, and optical networking technologies. The Internet carries a vast range of information resources and services, such as the inter-[linked](#hyperlink) [hypertext](#hypertext) documents and applications of the World Wide Web (WWW), electronic mail, telephony, and file sharing.

## [Internet Bot](https://en.wikipedia.org/wiki/Internet_bot)
An _Internet bot_, _web robot_, _robot_ or simply _bot_, is a software application that runs automated tasks (scripts) over the [Internet](#internet). Typically, bots perform tasks that are simple and repetitive, much faster than a person could. The most extensive use of bots is for [web crawling](#web-crawler), in which an automated script fetches, analyzes and files information from [web servers](#web-server). More than half of all web traffic is generated by bots.

## [Internet Control Message Protocol](https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol)
The _Internet Control Message Protocol_ (_ICMP_) is a supporting protocol in the [Internet protocol suite](#internet-protocol-suite). It is used by network devices, including routers, to send error messages and operational information indicating success or failure when communicating with another [IP address](#ip-address), for example, an error is indicated when a requested service is not available or that a host or router could not be reached. ICMP differs from transport protocols such as [TCP](#transmission-control-protocol) and [UDP](#user-datagram-protocol) in that it is not typically used to exchange data between systems, nor is it regularly employed by end-user network applications (with the exception of some diagnostic tools like ping and traceroute).

## [Internet Of Things](https://en.wikipedia.org/wiki/Internet_of_things)
The _Internet of things_ (_IoT_) describes the [network](#computer-network) of physical objects - “things” - that are embedded with sensors, software, and other technologies for the purpose of connecting and exchanging data with other devices and systems over the [Internet](#internet).

## [Internet Protocol](https://en.wikipedia.org/wiki/Internet_Protocol)
The _Internet Protocol_ (_IP_) is the principal communications protocol in the Internet protocol suite for relaying datagrams across network boundaries. Its [routing](#routing) function enables internetworking, and essentially establishes the [Internet](#internet).

IP has the task of delivering [packets](#network-packet) from the source host to the destination host solely based on the [IP addresses](#ip-address) in the packet headers. For this purpose, IP defines packet structures that encapsulate the data to be delivered. It also defines addressing methods that are used to label the datagram with source and destination information.

## [Internet Protocol Suite](https://en.wikipedia.org/wiki/Internet_protocol_suite)
The _Internet protocol suite_ is the conceptual model and set of communications protocols used in the [Internet](#internet) and similar [computer networks](#computer-network). It is commonly known as _TCP/IP_ because the foundational protocols in the suite are the [Transmission Control Protocol](#transmission-control-protocol) (TCP) and the [Internet Protocol](#internet-protocol) (IP). During its development, versions of it were known as the Department of Defense (DoD) model because the development of the networking method was funded by the United States Department of Defense through DARPA. Its implementation is a protocol stack.

## [Internet Service Provider](https://en.wikipedia.org/wiki/Internet_service_provider)
An _Internet service provider_ (_ISP_) is an organization that provides services for accessing, using, or participating in the [Internet](#internet). Internet service providers can be organised in various forms, such as commercial, community-owned, non-profit, or otherwise privately owned.

Internet services typically provided by ISPs can include Internet access, Internet transit, domain name registration, web hosting, Usenet service, and colocation.

An ISP typically serves as the access point or the gateway that provides a user, access to everything available on the Internet.

## [Interoperability](https://en.wikipedia.org/wiki/Interoperability)
A characteristic of a product or system, whose interaces are completely understood, to work with other products or systems, at present or in the future, in either implementation or access, without any restrictions. The term was initially defined for information technology or systems engineering services to allow for information exchange.

## [Interpreter](https://en.wikipedia.org/wiki/Interpreter_(computing))
In computer science, an _interpreter_ is a computer program that directly executes instructions written in a programming or scripting language, without requiring them previously to have been compiled into a [machine language](#machine-code) program. An interpreter generally uses one of the following strategies for program execution:

1. Parse the source code and perform its behavior directly;
2. Translate source code into some efficient intermediate representation and immediately execute this;
3. Explicitly execute stored precompiled code made by a compiler which is part of the interpreter system.

## [IP Address](https://en.wikipedia.org/wiki/IP_address)
An _Internet Protocol address_ (_IP address_) is a numerical label assigned to each device connected to a [computer network](#computer-network) that uses the [Internet Protocol](#internet-protocol) for communication. An IP address serves two main functions: host or network interface identification and location addressing.

## [Jaggies](https://en.wikipedia.org/wiki/Jaggies)
_"Jaggies"_ is the informal name for artifacts in [raster images](#raster-scan), most frequently from aliasing, which in turn is often caused by non-linear mixing effects producing high-frequency components, or missing or poor anti-aliasing filtering prior to sampling.

Jaggies are stair-like lines that appear where there should be "smooth" straight lines or curves. For example, when a nominally straight, un-aliased line steps across one pixel either horizontally or vertically, a "dogleg" occurs halfway through the line, where it crosses the threshold from one pixel to the other.


## [JavaScript](https://en.wikipedia.org/wiki/JavaScript)
_JavaScript_, often abbreviated as _JS_, is a programming language that conforms to the ECMAScript specification. JavaScript is high-level, often just-in-time compiled, and multi-paradigm. It has curly-bracket syntax, dynamic typing, prototype-based object-orientation, and first-class functions.

Alongside [HTML](#html) and [CSS](#css), JavaScript is one of the core technologies of the [World Wide Web](#world-wide-web). JavaScript enables interactive [web pages](#web-page) and is an essential part of web applications. The vast majority of websites use it for client-side page behavior, and all major [web browsers](#web-browser) have a dedicated JavaScript engine to execute it.


## [Key Exchange](https://en.wikipedia.org/wiki/Key_exchange)
_Key exchange_ (also _key establishment_) is a method in [cryptography](#cryptography) by which cryptographic keys are exchanged between two parties, allowing use of a cryptographic [algorithm](#algorithm).

If the sender and receiver wish to exchange encrypted messages, each must be equipped to encrypt messages to be sent and decrypt messages received. The nature of the equipping they require depends on the encryption technique they might use. If they use a code, both will require a copy of the same codebook. If they use a cipher, they will need appropriate keys. If the cipher is a [symmetric key cipher](#symmetric-key-algorithm), both will need a copy of the same key. If it is an [asymmetric key cipher](#public-key-cryptography) with the public/private key property, both will need the other's public key.

## [Labeled Data](https://en.wikipedia.org/wiki/Labeled_data)
_Labeled data_ is a group of samples that have been tagged with one or more labels. Labeling typically takes a set of unlabeled data and augments each piece of it with informative tags. For example, a data label might indicate whether a photo contains a horse or a cow, which words were uttered in an audio recording, what type of action is being performed in a video, what the topic of a news article is, what the overall sentiment of a tweet is, or whether a dot in an X-ray is a tumor.

Labels can be obtained by asking humans to make judgments about a given piece of unlabeled data. Labeled data is significantly more expensive to obtain than the raw unlabeled data.

## [Latency](https://en.wikipedia.org/wiki/Latency_(engineering))
_Latency_ from a general point of view is a time delay between the cause and the effect of some physical change in the system being observed, but, known within gaming circles as "lag", latency is a time interval between the input to a simulation and the visual or auditory response, often occurring because of [network](#computer-network) delay in online games.

Latency is physically a consequence of the limited velocity which any physical interaction can propagate. The magnitude of this velocity is always less than or equal to the speed of light. Therefore, every physical system with any physical separation (distance) between cause and effect will experience some sort of latency, regardless of the nature of stimulation that it has been exposed to.

The precise definition of latency depends on the system being observed or the nature of the simulation. In communications, the lower limit of latency is determined by the medium being used to transfer information. In reliable two-way communication systems, latency limits the maximum rate that information can be transmitted, as there is often a limit on the amount of information that is "in-flight" at any one moment. In the field of humanmachine interaction, perceptible latency has a strong effect on user satisfaction and usability.

## [Lethal Autonomous Weapon](https://en.wikipedia.org/wiki/Lethal_autonomous_weapon)
_Lethal autonomous weapons_ (_LAWs_) are a type of autonomous military system that can independently search for and engage targets based on programmed constraints and descriptions. LAWs are also known as _lethal autonomous weapon systems_ (_LAWS_), _autonomous weapon systems_ (_AWS_), _robotic weapons_, _killer robots_ or _slaughterbots_. LAWs may operate in the air, on land, on water, under water, or in space. The autonomy of current systems as of 2018 was restricted in the sense that a human gives the final command to attack - though there are exceptions with certain "defensive" systems.

## [Liquid-Crystal Display](https://en.wikipedia.org/wiki/Liquid-crystal_display)
A _liquid-crystal display_ (_LCD_) is a flat-panel display or other electronically modulated optical device that uses the light-modulating properties of liquid crystals combined with polarizers. Liquid crystals do not emit light directly, instead using a backlight or reflector to produce images in color or monochrome. LCDs are available to display arbitrary images (as in a general-purpose computer display) or fixed images with low information content, which can be displayed or hidden, such as preset words, digits, and seven-segment displays, as in a digital clock. They use the same basic technology, except that arbitrary images are made from a matrix of small pixels, while other displays have larger elements. LCDs can either be normally on (positive) or off (negative), depending on the polarizer arrangement. For example, a character positive LCD with a backlight will have black lettering on a background that is the color of the backlight, and a character negative LCD will have a black background with the letters being of the same color as the backlight. Optical filters are added to white on blue LCDs to give them their characteristic appearance.

## [Local Area Network](https://en.wikipedia.org/wiki/Local_area_network)
A _local area network_ (_LAN_) is a computer network that interconnects computers within a limited area such as a residence, school, laboratory, university campus or office building. By contrast, a [wide area network](#wide-area-network) (WAN) not only covers a larger geographic distance, but also generally involves leased telecommunication circuits.

[Ethernet](#ethernet) and [Wi-Fi](#wi-fi) are the two most common technologies in use for local area networks. Historical network technologies include ARCNET, Token ring, and AppleTalk.

## [Kernel](https://en.wikipedia.org/wiki/Kernel_(operating_system))
The _kernel_ is a computer program at the core of a computer's [operating system](#operating-system) with complete control over everything in the system. It is an integral part of any operating system. It is the "portion of the operating system code that is always resident in memory". It facilitates interactions between hardware and software components. On most systems, it is one of the first programs loaded on startup (after the bootloader). It handles the rest of startup as well as [input/output](#inputoutput) (I/O) requests from software, translating them into data-processing instructions for the central processing unit. It handles memory and peripherals like keyboards, monitors, printers, and speakers.

## [Kernel (Image Processing)](https://en.wikipedia.org/wiki/Kernel_(image_processing))
In image processing, a _kernel_, _convolution matrix_, or _mask_ is a small matrix. It is used for blurring, sharpening, embossing, edge detection, and more. This is accomplished by doing a [convolution](#convolution) between a kernel and an image.

## [Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus)
_Lambda calculus_ (also written as _λ-calculus_) is a formal system in mathematical logic for expressing computation based on function abstraction and application using variable binding and substitution. It is a universal model of computation that can be used to simulate any Turing machine. It was introduced by the mathematician Alonzo Church in the 1930s as part of his research into the foundations of mathematics.

## [Language Model](https://en.wikipedia.org/wiki/Language_model)
A statistical _language model_ is a probability distribution over sequences of words. Given such a sequence, say of length _m_, it assigns a probability _P(w<sub>1</sub>,...,w<sub>m</sub>)_ to the whole sequence.

The language model provides context to distinguish between words and phrases that sound similar. For example, in American English, the phrases "recognize speech" and "wreck a nice beach" sound similar, but mean different things.

## [Latency](https://en.wikipedia.org/wiki/Latency_(engineering))
The time delay between the cause and effect of some physical change in a system being observed.

## [Library](https://en.wikipedia.org/wiki/Library_(computing))
A _library_ is a collection of non-volatile resources used by computer programs, often for software development. These may include configuration data, documentation, help data, message templates, pre-written code and [subroutines](#subroutine), classes, values or type specifications. A _library_ is a collection of implementations of behavior, written in terms of a language, that has a well-defined interface by which the behavior is invoked.

## [Linked List](https://en.wikipedia.org/wiki/Linked_list)
A linear collection of data elements whose order is not given by their physical placement in memory. Instead, each element points to the next. It is a data structure consisting of a collection of nodes which together represent a sequence. In its most basic form, each node contains: data, and a reference (in other words, a _link_) to the next node in the sequence. This structure allows for efficient insertion or removal of elements from any position in the sequence during iteration. More complex variants add additional links, allowing more efficient insertion or removal of nodes at arbitrary positions. A drawback of linked lists is that access time is linear (and difficult to pipeline). Faster access, such as random access, is not feasible. Arrays have better _cache locality_ compared to linked lists.

![Singly-linked list](./singly-linked-list.svg)
<br />
A linked list whose nodes contain two fields: an integer value and a link to the next node. The last node is linked to a terminator used to signify the end of the list.

Linked lists are amongst the simplest and most common data structures. They can be used to implement several other common abstract data types, including lists, [stacks](#stack), [queues](#queue), associative arrays, and S-expressions, though it is not uncommon to implement those data structures directly without using a linked list as the basis.

## [Logic Gate](https://en.wikipedia.org/wiki/Logic_gate)
A physical electronic/electromechanical device implementing a boolean function. It performs a logical operation on one or more binary inputs to produce a single binary output.

## [Lossless Compression](https://en.wikipedia.org/wiki/Lossless_compression)
_Lossless compression_ is a class of [data compression](#data-compression) [algorithms](#algorithm) that allows the original data to be perfectly reconstructed from the compressed data. By contrast, [lossy compression](#lossy-compression) permits reconstruction only of an approximation of the original data, though usually with greatly improved compression rates (and therefore reduced media sizes).

By operation of the [pigeonhole principle](https://en.wikipedia.org/wiki/Pigeonhole_principle), no lossless compression algorithm can efficiently compress all possible data. For this reason, many different algorithms exist that are designed either with a specific type of input data in mind or with specific assumptions about what kinds of redundancy the uncompressed data are likely to contain.

Lossless data compression is used in many applications. For example, it is used in the ZIP file format and in the GNU tool gzip. It is also often used as a component within lossy data compression technologies (e.g. lossless mid/side joint stereo preprocessing by MP3 encoders and other lossy audio encoders).

Lossless compression is used in cases where it is important that the original and the decompressed data be identical, or where deviations from the original data would be unfavourable. Typical examples are executable programs, text documents, and source code. Some image file formats, like PNG or GIF, use only lossless compression, while others like TIFF and MNG may use either lossless or lossy methods. Lossless audio formats are most often used for archiving or production purposes, while smaller lossy audio files are typically used on portable players and in other cases where storage space is limited or exact replication of the audio is unnecessary.

## [Lossy Compression](https://en.wikipedia.org/wiki/Lossy_compression)
In information technology, _lossy compression_ or _irreversible compression_ is the class of [data encoding methods](#data-compression) that uses inexact approximations and partial data discarding to represent the content. These techniques are used to reduce data size for storing, handling, and transmitting content. The amount of data reduction possible using lossy compression is much higher than through [lossless](#lossless-compression) techniques.

Well-designed lossy compression technology often reduces file sizes significantly before degradation is noticed by the end-user. Even when noticeable by the user, further data reduction may be desirable (e.g., for real-time communication, to reduce transmission times, or to reduce storage needs). The most widely used lossy compression [algorithm](#algorithm) is the [discrete cosine transform](https://en.wikipedia.org/wiki/Discrete_cosine_transform) (DCT), first published by [Nasir Ahmed](https://en.wikipedia.org/wiki/N._Ahmed), T. Natarajan and [K. R. Rao](https://en.wikipedia.org/wiki/K._R._Rao) in 1974. Recently, a new family of sinusoidal-hyperbolic transform functions, which have comparable properties and performance with DCT, have been proposed for lossy compression.

Lossy compression is most commonly used to compress multimedia data (audio, video, and images), especially in applications such as streaming media and internet telephony. By contrast, lossless compression is typically required for text and data files, such as bank records and text articles. It can be advantageous to make a master lossless file which can then be used to produce additional copies from. This allows one to avoid basing new compressed copies off of a lossy source file, which would yield additional artifacts and further unnecessary information loss.

## [MAC Address](https://en.wikipedia.org/wiki/MAC_address)
A _media access control address_ (_MAC address_) is a unique identifier assigned to a network interface controller (NIC) for use as a network address in communications within a network segment. This use is common in most IEEE 802 networking technologies, including [Ethernet](#ethernet), [Wi-Fi](#wi-fi), and Bluetooth. Within the Open Systems Interconnection (OSI) network model, MAC addresses are used in the medium access control protocol sublayer of the data link layer. As typically represented, MAC addresses are recognizable as six groups of two hexadecimal digits, separated by hyphens, colons, or without a separator.

## [Machine Code](https://en.wikipedia.org/wiki/Machine_code)
In computer programming, _machine code_, consisting of machine language instructions, is a low-level programming language used to directly control a computer's [central processing unit](#central-processing-unit). Each instruction causes the CPU to perform a specific task such as a _load_, a _store_, a _jump_, or an [arithmetic logic unit (ALU)](../05/README.md) operation on one or more units of of data in the CPU's [registers](#register) or memory.

## [Machine Learning](https://en.wikipedia.org/wiki/Machine_learning)
_Machine learning_ (_ML_) is the study of computer [algorithms](#algorithm) that improve automatically through experience. It is seen as a subset of artificial intelligence. Machine learning algorithms build a model based on sample data, known as "training data", in order to make predictions or decisions without being explicitly programmed to do so. Machine learning algorithms are used in a wide variety of applications, such as email filtering and computer vision, where it is difficult or unfeasible to develop conventional algorithms to perform the needed tasks.

## [Magnetic Core Memory](https://en.wikipedia.org/wiki/Magnetic-core_memory)
_Magnetic-core memory_ was the predominant form of random-access computer memory for 20 years between about 1955 and 1975. Such memory is often just called _core memory_, or, informally, _core_.

Core memory uses toroids (rings) of a hard magnetic material (usually a semi-hard ferrite) as transformer cores, where each wire threaded through the core serves as a transformer winding. Three or four wires pass through each core.

Each core stores one bit of information. A core can be magnetized in either the clockwise or counter-clockwise direction. The value of the bit stored in a core is zero or one according to the direction of that core's magnetization. Electric current pulses in some of the wires through a core allow the direction of the magnetization in that core to be set in either direction, thus storing a one or a zero. Another wire through each core, the sense wire, is used to detect whether the core changed state.

The process of reading the core causes the core to be reset to a zero, thus erasing it. This is called destructive readout. When not being read or written, the cores maintain the last value they had, even if the power is turned off. Therefore they are a type of [non-volatile memory](#non-volatile-memory).

## [Malware](https://en.wikipedia.org/wiki/Malware)
_Malware_ (a portmanteau for _malicious software_) is any software intentionally designed to cause damage to a computer, server, client, or [computer network](#computer-network) (by contrast, software that causes unintentional harm due to some deficiency is typically described as a software bug). A wide variety of malware types exist, including computer viruses, worms, Trojan horses, ransomware, spyware, adware, rogue software, wiper and scareware.

Programs are also considered malware if they secretly act against the interests of the computer user. For example, at one point Sony music Compact discs silently installed a rootkit on purchasers' computers with the intention of preventing illicit copying, but which also reported on users' listening habits, and unintentionally created extra security vulnerabilities.

## [Magnetic Tape](https://en.wikipedia.org/wiki/Magnetic_tape)
_Magnetic tape_ is a medium for magnetic recording, made of a thin, magnetizable coating on a long, narrow strip of plastic film. It was developed in Germany in 1928, based on magnetic wire recording. Devices that record and playback audio and video using magnetic tape are tape recorders and video tape recorders respectively. A device that stores computer data on magnetic tape is known as a tape drive.

## [Message Switching](https://en.wikipedia.org/wiki/Message_switching)
In telecommunications, _message switching_ involves messages routed in their entirety, one hop at a time. It evolved from [circuit switching](#circuit-switching) and was the precursor of [packet switching](#packet-switching).

## [Microarchitecture](https://en.wikipedia.org/wiki/Microarchitecture)
In computer engineering, _microarchitecture_, also called _computer organization_, is the way a given [instruction set architecture (ISA)](https://en.wikipedia.org/wiki/Instruction_set_architecture) is implemented in a particular [processor](#central-processing-unit).

## [Microcomputer](https://en.wikipedia.org/wiki/Microcomputer)
A _microcomputer_ is a small, relatively inexpensive computer with a [microprocessor](#microprocessor) as its [central processing unit](#central-processing-unit) (CPU). It includes a microprocessor, memory and minimal [input/output](#inputoutput) (I/O) circuitry mounted on a single [printed circuit board](#printed-circuit-board) (PCB). Microcomputers became popular in the 1970s and 1980s with the advent of increasingly powerful microprocessors. The predecessors to these computers, mainframes and minicomputers, were comparatively much larger and more expensive (though indeed present-day mainframes such as the IBM System z machines use one or more custom microprocessors as their CPUs). Many microcomputers (when equipped with a keyboard and screen for input and output) are also [personal computers](#personal-computer) (in the generic sense).

The Commodore 64 was one of the most popular microcomputers of its era, and is the best-selling model of home computer of all time.
The abbreviation _micro_ was common during the 1970s and 1980s, but has now fallen out of common usage.

## [Microprocessor](https://en.wikipedia.org/wiki/Microprocessor)
A _microprocessor_ is a [computer processor](#central-processing-unit) that incorporates the functions of a central processing unit on a single (or more) [integrated circuit](#integrated-circuit) (IC) of MOSFET construction. The microprocessor is a multipurpose, clock-driven, register-based, digital integrated circuit that accepts binary data as input, processes it according to instructions stored in its memory, and provides results (also in binary form) as output. Microprocessors contain both combinational logic and sequential digital logic. Microprocessors operate on numbers and symbols represented in the binary number system.

## [Memory Address](https://en.wikipedia.org/wiki/Memory_address)
A reference to a specific memory location used at various levels of software and hardware. Memory addresses are fixed-length sequences of digits conventionally displayed and manipulated as unsigned integers.

## [Memory Hierarchy](https://en.wikipedia.org/wiki/Memory_hierarchy)
In computer architecture, the _memory hierarchy_ separates computer storage into a hierarchy based on response time. Since response time, complexity, and capacity are related, the levels may also be distinguished by their performance and controlling technologies. Memory hierarchy affects performance in computer architectural design, [algorithm](#algorithm) predictions, and lower level programming constructs involving locality of reference.

Designing for high performance requires considering the restrictions of the memory hierarchy, i.e. the size and capabilities of each component. Each of the various components can be viewed as part of a hierarchy of memories (m<sub>1</sub>,m<sub>2</sub>,...,m<sub>n</sub>) in which each member m<sub>i</sub> is typically smaller and faster than the next highest member m<sub>i+1</sub> of the hierarchy. To limit waiting by higher levels, a lower level will respond by filling a buffer and then signaling for activating the transfer.

There are four major storage levels.

1. Internal - [Processor registers](#register) and cache.
2. Main - the system [RAM](#random-access-memory) and controller cards.
3. On-line mass storage -  Secondary storage.
4. Off-line bulk storage - Tertiary and Off-line storage.

This is a general memory hierarchy structuring. Many other structures are useful. For example, a paging algorithm may be considered as a level for virtual memory when designing a computer architecture, and one can include a level of nearline storage between online and offline storage.

## [Memory Protection](https://en.wikipedia.org/wiki/Memory_protection)
_Memory protection_ is a way to control memory access rights on a computer, and is a part of most modern instruction set architectures and [operating systems](#operating-system). The main purpose of memory protection is to prevent a process from accessing memory that has not been allocated to it. This prevents a bug or malware within a process from affecting other processes, or the operating system itself. Protection may encompass all accesses to a specified area of memory, write accesses, or attempts to execute the contents of the area. An attempt to access unauthorized memory results in a hardware fault, e.g., a segmentation fault, storage violation exception, generally causing abnormal termination of the offending process. Memory protection for computer security includes additional techniques such as address space layout randomization and executable space protection.

## [Metadata](https://en.wikipedia.org/wiki/Metadata)
_Metadata_ is "data that provides information about other data". In other words, it is "data about data". Many distinct types of metadata exist, including _descriptive metadata_, _structural metadata_, _administrative metadata_, _reference metadata_ and _statistical metadata_.

* Descriptive metadata is descriptive information about a resource. It is used for discovery and identification. It includes elements such as title, abstract, author, and keywords.
* Structural metadata is metadata about containers of data and indicates how compound objects are put together, for example, how pages are ordered to form chapters. It describes the types, versions, relationships and other characteristics of digital materials.
* Administrative metadata is information to help manage a resource, like resource type, permissions, and when and how it was created.
* Reference metadata is information about the contents and quality of statistical data.
* Statistical metadata, also called process data, may describe processes that collect, process, or produce statistical data.

## [Moore's Law](https://en.wikipedia.org/wiki/Moore%27s_law)
_Moore's Law_ is the observation that the number of transistors in a dense [integrated circuit](#integrated-circuit) (IC) doubles about every two years. Moore's Law is an observation and projection of a historical trend. Rather than a law of physics, it is an empirical relationship linked to gains from experience in production.

## [Motherboard](https://en.wikipedia.org/wiki/Motherboard)
A _motherboard_ (also called _mainboard_, _main circuit board_, _system board_, _baseboard_, _planar board_, _logic board_, and _mobo_) is the main [printed circuit board](#printed-circuit-board) (PCB) in general-purpose computers and other expandable systems. It holds and allows communication between many of the crucial electronic components of a system, such as the [central processing unit](#central-processing-unit) (CPU) and [memory](#random-access-memory), and provides connectors for other peripherals. Unlike a backplane, a motherboard usually contains significant sub-systems, such as the central processor, the chipset's [input/output](#inputoutput) and memory controllers, interface connectors, and other components integrated for general use.

## [Multi-Core Processor](https://en.wikipedia.org/wiki/Multi-core_processor)
A _multi-core processor_ is a [computer processor](#central-processing-unit) [integrated circuit](#integrated-circuit) with two or more separate processing units, called _cores_, each of which reads and executes program instructions, as if the computer had several processors.

## [Multi-Factor Authentication](https://en.wikipedia.org/wiki/Multi-factor_authentication)
_Multi-factor authentication_ (_MFA_; encompassing _Two-factor authentication_ or _2FA_, along with similar terms) is an electronic authentication method in which a computer user is granted access to a website or application only after successfully presenting two or more pieces of evidence (or factors) to an authentication mechanism: knowledge (something only the user knows), possession (something only the user has), and inherence (something only the user is). It protects the user from an unknown person trying to access their data such as personal ID details or financial assets.

## [Multiplexer](https://en.wikipedia.org/wiki/Multiplexer)
A _multiplexer_ (or _mux_/_data selector_) is a device that selects between several analog or digital input signals and forwards it to a single output line.

## [Multitasking](https://en.wikipedia.org/wiki/Computer_multitasking)
_Multitasking_ is the concurrent execution of multiple tasks (also known as processes) over a certain period of time. New tasks can interrupt already started ones before they finish, instead of waiting for them to end. As a result, a computer executes segments of multiple tasks in an interleaved manner, while the tasks share common processing resources such as [central processing units](#central-processing-unit) (CPUs) and main memory. Multitasking automatically interrputs the running program, saving its state (partial results, memory contents and computer register contents) and loading the saved state of another program and transferring control to it. This "context switch" may be initiated at fixed time intervals (pre-emptive multitasking), or the running program may be coded to signal to the supervisory software when it can be interrupted (cooperative multitasking).

## [Natural Language](https://en.wikipedia.org/wiki/Natural_language)
In neuropsychology, linguistics, and the philosophy of language, a _natural language_ or _ordinary language_ is any language that has evolved naturally in humans through use and repetition without conscious planning or premeditation. Natural languages can take different forms, such as speech or signing. They are distinguished from constructed and formal languages such as those used to program computers or to study logic.

## [Natural Language Processing](https://en.wikipedia.org/wiki/Natural_language_processing)
_Natural language processing_ (_NLP_) is a subfield of linguistics, computer science, and [artificial intelligence](#artificial-intelligence) concerned with the interactions between computers and human language, in particular how to program computers to process and analyze large amounts of [natural language](#natural-language) data. The result is a computer capable of "understanding" the contents of documents, including the contextual nuances of the language within them. The technology can then accurately extract information and insights contained in the documents as well as categorize and organize the documents themselves.

## [Negative Feedback](https://en.wikipedia.org/wiki/Negative_feedback)
_Negative feedback_ (or _balancing feedback_) occurs when some function of the output of a system, process, or mechanism is fed back in a manner that tends to reduce the fluctuations in the output, whether caused by changes in the input or by other disturbances.

Whereas positive feedback tends to lead to instability via exponential growth, oscillation or chaotic behavior, negative feedback generally promotes stability. Negative feedback tends to promote a settling to equilibrium, and reduces the effects of perturbations. Negative feedback loops in which just the right amount of correction is applied with optimum timing can be very stable, accurate, and responsive.


## [Net Neutrality](https://en.wikipedia.org/wiki/Net_neutrality)
_Network neutrality_, most commonly called _net neutrality_, is the principle that [Internet service providers](#internet-service-provider) (ISPs) must treat all [Internet](#internet) communications equally, and not discriminate or charge differently based on user, content, website, platform, application, type of equipment, source address, destination address, or method of communication.

With net neutrality, ISPs may not intentionally block, slow down, or charge money for specific online content. Without net neutrality, ISPs may prioritize certain types of traffic, meter others, or potentially block traffic from specific services, while charging consumers for various tiers of service.

## [Network Congestion](https://en.wikipedia.org/wiki/Network_congestion)
_Network congestion_ in data networking and queueing theory is the reduced quality of service that occurs when a network node or link is carrying more data than it can handle. Typical effects include queueing delay, [packet](#network-packet) loss or the blocking of new connections. A consequence of congestion is that an incremental increase in offered load leads either only to a small increase or even a decrease in network throughput.

Network protocols that use aggressive retransmissions to compensate for packet loss due to congestion can increase congestion, even after the initial load has been reduced to a level that would not normally have induced network congestion. Such networks exhibit two stable states under the same level of load. The stable state with low throughput is known as _congestive collapse_.

Networks use _congestion control_ and _congestion avoidance_ techniques to try to avoid collapse. These include: [exponential backoff](#exponential-backoff) in protocols such as CSMA/CA in 802.11 and the similar CSMA/CD in the original [Ethernet](#ethernet), window reduction in [TCP](#transmission-control-protocol), and fair queueing in devices such as routers and [network switches](#network-switch). Other techniques that address congestion include priority schemes which transmit some packets with higher priority ahead of others and the explicit allocation of network resources to specific flows through the use of admission control.

## [Network Layer](https://en.wikipedia.org/wiki/Network_layer)
In the seven-layer OSI model of computer networking, the _network layer_ is _layer 3_. The network layer is responsible for [packet](#network-packet) forwarding including routing through intermediate routers.

## [Network Packet](https://en.wikipedia.org/wiki/Network_packet)
In telecommunications and computer networking, a _network packet_ is a formatted unit of data carried by a [packet-switched](#packet-switching) network. A packet consists of control information and user data; the latter is also known as the payload. Control information provides data for delivering the payload (e.g., source and destination network addresses, error detection codes, or sequencing information). Typically, control information is found in packet [headers](#header) and trailers.

In packet switching, the [bandwidth](#bandwidth) of the transmission medium is shared between multiple communication sessions, in contrast to circuit switching, in which circuits are preallocated for the duration of one session and data is typically transmitted as a continuous bit stream.

## [Network Switch](https://en.wikipedia.org/wiki/Network_switch)
A _network switch_ (also called _switching hub_, _bridging hub_, and, by the IEEE, _MAC bridge_) is networking hardware that connects devices on a [computer network](#computer-networ) by using packet switching to receive and forward data to the destination device.

A network switch is a multi-[port](#port) network bridge that uses MAC addresses to forward data at the data link layer (layer 2) of the OSI model. Some switches can also forward data at the network layer (layer 3) by additionally incorporating [routing](#routing) functionality. Such switches are commonly known as layer-3 switches or multilayer switches.

Switches for [Ethernet](#ethernet) are the most common form of network switch. The first Ethernet switch was introduced by Kalpana in 1990. Switches also exist for other types of networks including Fibre Channel, Asynchronous Transfer Mode, and InfiniBand.

## [Node](https://en.wikipedia.org/wiki/Node_(computer_science))
A _node_ is a basic unit of a data structure, such as a [linked list](#linked-list) or _[tree](#tree)_ data structure. Nodes contain data and also may link to other nodes. Links between nodes are often implemented by [pointers](#pointer).

## [Non-Volatile Memory](https://en.wikipedia.org/wiki/Non-volatile_memory)
A type of computer memory that can retrieve stored data even after having been power cycled.

## [Normal](https://en.wikipedia.org/wiki/Normal_(geometry))
In geometry, a _normal_ is an object such as a line, ray, or vector that is perpendicular to a given object. For example, in two dimensions, the normal line to a curve at a given point is the line perpendicular to the tangent line to the curve at the point. A normal vector may have length one (a unit vector) or its length may represent the curvature of the object (a curvature vector); its algebraic sign may indicate sides (interior or exterior).

In three dimensions, a surface normal, or simply normal, to a surface at point P is a vector perpendicular to the tangent plane of the surface at P. The word "normal" is also used as an adjective: a line normal to a plane, the normal component of a force, the normal vector, etc. The concept of normality generalizes to orthogonality (right angles).

## [Null Character](https://en.wikipedia.org/wiki/Null_character)
The _null character_ (also _null terminator_) is a control character with a value of zero. It is present in many character sets. It is available in nearly all mainstream programming languages. It is often abbreviated as _NUL_ (or _NULL_ though in some contexts that term is used for the _null pointer_, a different object). In 8-bit codes, it is known as a _null byte_.

## [Numerical Control](https://en.wikipedia.org/wiki/Numerical_control)
_Numerical control_ (also _computer numerical control_, and commonly called _CNC_) is the automated control of machining tools (such as drills, lathes, mills) and 3D printers by means of a computer. A CNC machine processes a piece of material (metal, plastic, wood, ceramic, or composite) to meet specifications by following a coded programmed instruction and without a manual operator directly controlling the machining operation.

## [Object-Oriented Programming](https://en.wikipedia.org/wiki/Object-oriented_programming)
A programming paradigm based on the concept of "objects", which can contain data and code: data in the form of _fields_ (often known as _attributes_ or _properties_), and code, in the form of procedures (often known as _methods_).

## [One-Way Function](https://en.wikipedia.org/wiki/One-way_function)
In computer science, a _one-way function_ is a function that is easy to compute on every input, but hard to invert given the image of a random input. Here, "easy" and "hard" are to be understood in the sense of computational complexity theory, specifically the theory of polynomial time problems. Not being one-to-one is not considered sufficient for a function to be called one-way.

The existence of such one-way functions is still an open conjecture. In fact, their existence would prove that the complexity classes P and NP are not equal, thus resolving the foremost unsolved question of theoretical computer science. The converse is not known to be true, i.e. the existence of a proof that P≠NP would not directly imply the existence of one-way functions.

## [Open Architecture](https://en.wikipedia.org/wiki/Open_architecture)
_Open architecture_ is a type of computer architecture or software architecture intended to make adding, upgrading, and swapping components easy. For example, the IBM PC, Amiga 500 and Apple IIe have an open architecture supporting plug-in cards, whereas the Apple IIc computer has a closed architecture. Open architecture systems may use a standardized system bus such as S-100, PCI or ISA or they may incorporate a proprietary bus standard such as that used on the Apple II, with up to a dozen slots that allow multiple hardware manufacturers to produce add-ons, and for the user to freely install them. By contrast, closed architectures, if they are expandable at all, have one or two "expansion ports" using a proprietary connector design that may require a license fee from the manufacturer, or enhancements may only be installable by technicians with specialized tools or training.

## [Open-Source Software](https://en.wikipedia.org/wiki/Open-source_software)
_Open-source software_ (_OSS_) is a type of computer software in which source code is released under a license in which the copyright holder grants users the rights to use, study, change, and distribute the software to anyone and for any purpose. Open-source software may be developed in a collaborative public manner. Open-source software is a prominent example of open collaboration.

## [Operand](https://en.wikipedia.org/wiki/Operand)
In mathematics, an _operand_ is the object of a mathematical operation, i.e., it is the object or quantity that is being operated on.

## [Operating System](https://en.wikipedia.org/wiki/Operating_system)
An _operating system_ (_OS_) is system software that manages computer hardware, software resources, and provides common services for computer programs.

## [Operation Code](https://en.wikipedia.org/wiki/Opcode)
An _operation code_ (also known as _opcode_, _instruction machine code_, _instruction code_, _instruction syllable_, _instruction parcel_, or _opstring_) is the portion of a machine language instruction that specifies the operation to be performed. Besides the opcode itself, most instructions specify the data they will process in the form of [operands](#operand).

## [Orthographic Projection](https://en.wikipedia.org/wiki/Orthographic_projection)
_Orthographic projection_ (sometimes referred to as _orthogonal projection_, used to be called _analemma_) is a means of representing three-dimensional objects in two dimensions. It is a form of parallel projection, in which all the projection lines are orthogonal to the projection plane, resulting in every plane of the scene appearing in affine transformation on the viewing surface. The obverse of an orthographic projection is an oblique projection, which is a parallel projection in which the projection lines are not orthogonal to the projection plane.

## [OSI Model](https://en.wikipedia.org/wiki/OSI_model)
The _Open Systems Interconnection model_ (_OSI model_) is a conceptual model that characterises and standardises the communication functions of a telecommunication or computing system without regard to its underlying internal structure and technology. Its goal is the interoperability of diverse communication systems with standard communication protocols.

## [Out-Of-Order Execution](https://en.wikipedia.org/wiki/Out-of-order_execution)
_Out-of-order execution_ (or _dynamic execution_) is a paradigm used in most high-performance [CPUs](#central-processing-unit) to make use of [instruction cycles](#clock-signal) that would otherwise be wasted. In this paradigm, a processor executes instructions in an order governed by the availability of input data and execution units, rather than by their original order in a program. In doing so, the processor can avoid being idle while waiting for the preceding instruction to complete and can, in the meantime, process the next instructions that are able to run immediately and independently.

## [Packet Switching](https://en.wikipedia.org/wiki/Packet_switching)
In telecommunications, _packet switching_ is a method of grouping data that is transmitted over a digital network into packets. Packets are made of a [header](#header) and a payload. Data in the header is used by networking hardware to direct the packet to its destination, where the payload is extracted and used by application software. Packet switching is the primary basis for data communications in [computer networks](#computer-network) worldwide.

## [Painter's Algorithm](https://en.wikipedia.org/wiki/Painter%27s_algorithm)
The _painter’s algorithm_ (also _depth-sort algorithm_ and _priority fill_) is an [algorithm](#algorithm) for visible surface determination in 3D computer graphics that works on a polygon-by-polygon basis rather than a pixel-by-pixel, row by row, or area by area basis of other Hidden Surface Removal algorithms. The painter’s algorithm creates images by sorting the polygons within the image by their depth and placing each polygon in order from the farthest to the closest object.

## [Parameter](https://en.wikipedia.org/wiki/Parameter_(computer_programming))
In computer programming, a _parameter_ or a _formal argument_, is a special kind of variable, used in a [subroutine](#subroutine) to refer to one of the pieces of data provided as input to the subroutine. These pieces of data are the values of the _arguments_ (often called _actual arguments_ or _actual parameters_) with which the subroutine is going to be called/invoked. An ordered list of parameters is usually included in the definition of a subroutine, so that, each time the subroutine is called, its arguments for that call are evaluated, and the resulting values can be assigned to the corresponding parameters.

## [Parse Tree](https://en.wikipedia.org/wiki/Parse_tree)
A _parse tree_ or _parsing tree_ or _derivation tree_ or _concrete syntax tree_ is an ordered, rooted [tree](#tree) that represents the syntactic structure of a string according to some context-free grammar. The term parse tree itself is used primarily in computational linguistics; in theoretical syntax, the term _syntax tree_ is more common.

## [Parsing](https://en.wikipedia.org/wiki/Parsing)
_Parsing_, _syntax analysis_, or _syntactic analysis_ is the process of analyzing a string of symbols, either in [natural language](#natural-language), computer languages or data structures, conforming to the rules of a formal grammar. The term parsing comes from Latin _pars_ (_orationis_), meaning part (of speech).

The term has slightly different meanings in different branches of linguistics and computer science. Traditional sentence parsing is often performed as a method of understanding the exact meaning of a sentence or word, sometimes with the aid of devices such as sentence diagrams. It usually emphasizes the importance of grammatical divisions such as subject and predicate.

## [Personal Computer](https://en.wikipedia.org/wiki/Personal_computer)
A _personal computer_ (_PC_) is a multi-purpose computer whose size, capabilities, and price make it feasible for individual use. Personal computers are intended to be operated directly by an end user, rather than by a computer expert or technician. Unlike large, costly minicomputers and mainframes, time-sharing by many people at the same time is not used with personal computers.

## [Perspective Projection](https://en.wikipedia.org/wiki/3D_projection#Perspective_projection)
Perspective projection or perspective transformation is a linear projection where three dimensional objects are projected on a picture plane. This has the effect that distant objects appear smaller than nearer objects.

It also means that lines which are parallel in nature (that is, meet at the point at infinity) appear to intersect in the projected image, for example if railways are pictured with perspective projection, they appear to converge towards a single point, called the vanishing point. Photographic lenses and the human eye work in the same way, therefore perspective projection looks most realistic. Perspective projection is usually categorized into one-point, two-point and three-point perspective, depending on the orientation of the projection plane towards the axes of the depicted object.

## [Phishing](https://en.wikipedia.org/wiki/Phishing)
_Phishing_ is the fraudulent attempt to obtain sensitive information or data, such as usernames, passwords and credit card details, by disguising oneself as a trustworthy entity in an electronic communication. Typically carried out by email spoofing, instant messaging, and text messaging, phishing often directs users to enter personal information at a fake website which matches the look and feel of the legitimate site.

Phishing is an example of [social engineering](#social-engineering) techniques used to deceive users. Users are lured by communications purporting to be from trusted parties such as social web sites, auction sites, banks, colleagues/executives, online payment processors or IT administrators.

## [Phoneme](https://en.wikipedia.org/wiki/Phoneme)
In phonology and linguistics, a _phoneme_ is a unit of sound that distinguishes one word from another in a particular language.

Phonemes that are established by the use of minimal pairs, such as _tap_ vs _tab_ or _pat_ vs _bat_, are written between slashes: /p/, /b/. To show pronunciation, linguists use square brackets: [pʰ] (indicating an aspirated _p_ in _pat_).

There are differing views as to exactly what phonemes are and how a given language should be analyzed in phonemic (or phonematic) terms. However, a phoneme is generally regarded as an abstraction of a set (or equivalence class) of speech sounds (phones) that are perceived as equivalent to each other in a given language.

## [Phrase Structure Rules](https://en.wikipedia.org/wiki/Phrase_structure_rules)
_Phrase structure rules_ are a type of rewrite rule used to describe a given language's syntax and are closely associated with the early stages of transformational grammar, proposed by Noam Chomsky in 1957. They are used to break down a [natural language](#natural-language) sentence into its constituent parts, also known as syntactic categories, including both lexical categories (parts of speech) and phrasal categories. A grammar that uses phrase structure rules is a type of phrase structure grammar. Phrase structure rules as they are commonly employed operate according to the constituency relation, and a grammar that employs phrase structure rules is therefore a constituency grammar; as such, it stands in contrast to dependency grammars, which are based on the dependency relation.

## [Physical Layer](https://en.wikipedia.org/wiki/Physical_layer)
In the seven-layer OSI model of [computer networking](#computer-network), the _physical layer_ or _layer 1_ is the first and lowest layer. This layer may be implemented by a PHY chip.

The physical layer defines the means of transmitting raw bits over a physical data link connecting network nodes. The bitstream may be grouped into code words or symbols and converted to a physical signal that is transmitted over a transmission medium. The physical layer provides an electrical, mechanical, and procedural interface to the transmission medium. The shapes and properties of the electrical connectors, the frequencies to broadcast on, the line code to use and similar low-level parameters, are specified by the physical layer.

## [PID Controller](https://en.wikipedia.org/wiki/PID_controller)
A _proportional-integral-derivative controller_ (_PID controller_ or _three-term controller_) is a [control loop](#control-loop) mechanism employing [feedback](#feedback) that is widely used in industrial control systems and a variety of other applications requiring continuously modulated control. A PID controller continuously calculates an _error value_ ___e(t)___ as the difference between a desired setpoint (SP) and a measured process variable (PV) and applies a correction based on proportional, integral, and derivative terms (denoted _P_, _I_, and _D_ respectively), hence the name.

In practical terms it automatically applies an accurate and responsive correction to a control function. An everyday example is the cruise control on a car, where ascending a hill would lower speed if only constant engine power were applied. The controller's PID algorithm restores the measured speed to the desired speed with minimal delay and overshoot by increasing the power output of the engine.

## [Pixel](https://en.wikipedia.org/wiki/Pixel)
In digital imaging, a _pixel_, _pel_, or _picture element_ is a physical point in a raster image, or the smallest addressable element in an all points addressable display device; so it is the smallest controllable element of a picture represented on the screen.

Each pixel is a sample of an original image; more samples typically provide more accurate representations of the original. The intensity of each pixel is variable. In color imaging systems, a color is typically represented by three or four component intensities such as red, green, and blue, or cyan, magenta, yellow, and black.

## [Point and Click](https://en.wikipedia.org/wiki/Point_and_click)
_Point and click_ are the actions of a computer user moving a pointer to a certain location on a screen (pointing) and then pressing a button on a [mouse](#computer-mouse), usually the left button (click), or other pointing device. An example of point and click is in hypermedia, where users click on [hyperlinks](#hyperlink) to navigate from document to document.

Point and click can be used with any number of input devices varying from mouses, touch pads, trackpoint, joysticks, scroll buttons, and roller balls.

User interfaces, for example [graphical user interfaces](#graphical-user-interface), are sometimes described as "point-and-click interfaces", often to suggest that they are very easy to use, requiring that the user simply point to indicate their wishes. These interfaces are sometimes referred to condescendingly (e.g., by [Unix](#unix) users) as "click-and-drool" or "point-and-drool" interfaces.

The use of this phrase to describe software implies that the interface can be controlled solely through the mouse (or some other means such as a stylus), with little or no input from the [keyboard](#computer-keyboard), as with many graphical user interfaces.

## [Pointer](https://en.wikipedia.org/wiki/Pointer_(computer_programming))
An object in many programming languages that stores a memory address. This can be that of another value located in computer memory, or in some cases, that of memory-mapped computer hardware. A pointer _references_ a location in memory, and obtaining the value stored at that location is known as _dereferencing_ the pointer. As an analogy, a page number in a book's index could be considered a pointer to the corresponding page; dereferencing such a pointer would be done by flipping to the page with the given page number and reading the text found on the page. The actual format and content of a pointer variable is dependent on the underlying computer architecture.

## [Polygon Mesh](https://en.wikipedia.org/wiki/Polygon_mesh)
In 3D computer graphics and solid modeling, a _polygon mesh_ is a collection of _vertices_, _edges_ and _faces_ that defines the shape of a polyhedral object. The faces usually consist of triangles (triangle mesh), quadrilaterals (quads), or other simple convex polygons (n-gons), since this simplifies rendering, but may also be more generally composed of concave polygons, or even polygons with holes.

The study of polygon meshes is a large sub-field of computer graphics (specifically 3D computer graphics) and geometric modeling. Different representations of polygon meshes are used for different applications and goals. The variety of operations performed on meshes may include: Boolean logic, smoothing, simplification, and many others. Algorithms also exist for ray tracing, collision detection, and rigid-body dynamics with polygon meshes. If the mesh's edges are rendered instead of the faces, then the model becomes a [wireframe model](#wire-frame-model).

## [Printed Circuit Board](https://en.wikipedia.org/wiki/Printed_circuit_board)
A _printed circuit board_ (_PCB_) mechanically supports and electrically connects electrical or electronic components using conductive tracks, pads, and other features etched from one or more sheet layers of copper laminated onto and/or between sheet layers of a non-conductive substrate. Components are generally soldered onto the PCB to electrically connect and mechanically fasten them to it.

## [Port](https://en.wikipedia.org/wiki/Port_(computer_networking))
In [computer networking](#computer-network), a _port_ is a communication endpoint. At the software level, within an [operating system](#operating-system), a port is a logical construct that identifies a specific process or a type of network service. A port is identified for each transport protocol and address combination by a 16-bit unsigned number, known as the _port number_. The most common transport protocols that use port numbers are the [Transmission Control Protocol](#transmission-control-protocol) (TCP) and the [User Datagram Protocol](#user-datagram-protocol) (UDP).

A port number is always associated with an [IP address](#ip-address) of a host and the type of transport protocol used for communication. It completes the destination or origination network address of a message. Specific port numbers are reserved to identify specific services so that an arriving [packet](#network-packet) can be easily forwarded to a running application. For this purpose, the lowest-numbered 1024 port numbers identify the historically most commonly used services and are called the _well-known port numbers_. Higher-numbered ports are available for general use by applications and are known as _ephemeral ports_.

Ports provide a multiplexing service for multiple services or multiple communication sessions at one network address. In the client-server model of application architecture, multiple simultaneous communication sessions may be initiated for the same service.

## [Positional Notation](https://en.wikipedia.org/wiki/Positional_notation)
A way of representing the values of a positional system (a numeral system in which the contribution of a digit to the value of a number is the product of the value of the digit by a factor determined by the position of the digit).

## [Prefix Code](https://en.wikipedia.org/wiki/Prefix_code)
A _prefix code_ is a type of code system distinguished by its possession of the "prefix property", which requires that there is no whole code word in the system that is a prefix (initial segment) of any other code word in the system. It is trivially true for fixed-length code, so only a point of consideration in variable-length code.

For example, a code with code words {9, 55} has the prefix property; a code consisting of {9, 5, 59, 55} does not, because "5" is a prefix of "59" and also of "55". A prefix code is a uniquely decodable code: given a complete and accurate sequence, a receiver can identify each word without requiring a special marker between words. However, there are uniquely decodable codes that are not prefix codes; for instance, the reverse of a prefix code is still uniquely decodable (it is a suffix code), but it is not necessarily a prefix code.

Prefix codes are also known as _prefix-free codes_, _prefix condition codes_ and _instantaneous codes_. Although [Huffman coding](#huffman-coding) is just one of many [algorithms](#algorithm) for deriving prefix codes, prefix codes are also widely referred to as "Huffman codes", even when the code was not produced by a Huffman algorithm. The term comma-free code is sometimes also applied as a synonym for prefix-free codes but in most mathematical books and articles a comma-free code is used to mean a self-synchronizing code, a subclass of prefix codes.

## [Pseudocode](https://en.wikipedia.org/wiki/Pseudocode)
_Pseudocode_ is a plain language description of the steps in an [algorithm](#algorithm) or another system. Pseudocode often uses structural conventions of a normal programming language, but is intended for human reading rather than machine reading. It typically omits details that are essential for machine understanding of the algorithm, such as variable declarations and language-specific code.

## [Public-Key Cryptography](https://en.wikipedia.org/wiki/Public-key_cryptography)
_Public-key cryptography_, or _asymmetric cryptography_, is a [cryptographic](#cryptography) system that uses pairs of keys: public keys, which may be disseminated widely, and private keys, which are known only to the owner. The generation of such keys depends on cryptographic [algorithms](#algorithm) based on mathematical problems to produce [one-way functions](#one-way-function). Effective security only requires keeping the private key private; the public key can be openly distributed without compromising security.

In such a system, any person can [encrypt](#encryption) a message using the receiver's public key, but that encrypted message can only be decrypted with the receiver's private key. This allows, for instance, a [server](#web-server) to generate a cryptographic key intended for [symmetric-key cryptography](#symmetric-key-algorithm), then use a client's openly-shared public key to encrypt that newly-generated symmetric key. Now, the server can send this encrypted symmetric key on insecure channels to the client, and only the client can decrypt it using the client's private key pair to the public key used by the server to encrypt this message. With the client and server both having the same symmetric key now, they can safely transition to symmetric key encryption to securely communicate back and forth on otherwise-insecure channels. This has the advantage of not having to manually pre-share symmetric keys, while also gaining the higher data throughput advantage of symmetric-key cryptography over asymmetric key cryptography.

## [Punched Card](https://en.wikipedia.org/wiki/Punched_card)
A punched card (also punch card or punched-card) is a piece of stiff paper that can be used to contain digital data represented by the presence or absence of holes in predefined positions. Digital data can be used for data processing applications or used to directly control automated machinery.

Punched cards were widely used through much of the 20th century in the data processing industry, where specialized and increasingly complex unit record machines, organized into semiautomatic data processing systems, used punched cards for data input, output, and storage. The IBM 12-row/80-column punched card format came to dominate the industry. Many early digital computers used punched cards as the primary medium for input of both computer programs and data.

While punched cards are now obsolete as a storage medium, as of 2012, some voting machines still use punched cards to record votes.

## [Punched Tape](https://en.wikipedia.org/wiki/Punched_tape)
Punched tape or perforated paper tape is a form of data storage that consists of a long strip of paper in which holes are punched. It developed from and was subsequently used alongside [punched cards](#punched-card), differing in that the tape is continuous.

It was used throughout the 19th and for much of the 20th centuries for programmable looms, teleprinter communication, for input to computers of the 1950s and 1960s, and later as a storage medium for minicomputers and CNC machine tools.

## [Quality Assurance](https://en.wikipedia.org/wiki/Quality_assurance)
_Quality assurance_ (_QA_) is a way of preventing mistakes and defects in manufactured products and avoiding problems when delivering products or services to customers; which [ISO 9000](https://en.wikipedia.org/wiki/ISO_9000) defines as "part of quality management focused on providing confidence that quality requirements will be fulfilled". This defect prevention in quality assurance differs subtly from defect detetion and rejction in quality control and has been referred to as a _shift left_ since it focuses on quality earlier in the process (i.e., to the left of a linear process diagram reading left to right).

## [Queue](https://en.wikipedia.org/wiki/Queue_(abstract_data_type))
A collection of entities that are maintained in a sequence that can be modified by the addition of entities at one end of the sequence and the removal of entities from the other end of the sequence. By convention, the end of the sequence at which elements are added is called the back, tail, or rear of the queue, and the end at which elements are removed is called the head or front of the queue, analgously to the words used when people line up to wait for goods or services.

## [Random-Access Memory](https://en.wikipedia.org/wiki/Random-access_memory)
_Random-access memory_ (_RAM_) is a form of computer memory that can be read and changed in any order, typically used to store working data and machine code. Most RAM is [volatile](#volatile-memory).

## [Ransomware](https://en.wikipedia.org/wiki/Ransomware)
_Ransomware_ is a type of [malware](#malware) from cryptovirology that threatens to publish the victim's data or perpetually block access to it unless a ransom is paid. While some simple ransomware may lock the system so that it is not difficult for a knowledgeable person to reverse, more advanced malware uses a technique called cryptoviral extortion. It encrypts the victim's files, making them inaccessible, and demands a ransom payment to decrypt them. In a properly implemented cryptoviral extortion attack, recovering the files without the decryption key is an intractable problem  and difficult to trace digital currencies such as paysafecard or Bitcoin and other cryptocurrencies are used for the ransoms, making tracing and prosecuting the perpetrators difficult.

Ransomware attacks are typically carried out using a [Trojan](#trojan-horse) disguised as a legitimate file that the user is tricked into downloading or opening when it arrives as an email attachment. However, one high-profile example, the WannaCry worm, traveled automatically between computers without user interaction.

## [Raster Scan](https://en.wikipedia.org/wiki/Raster_scan)
A _raster scan_, or _raster scanning_, is the rectangular pattern of image capture and reconstruction in television. By analogy, the term is used for raster graphics, the pattern of image storage and transmission used in most computer [bitmap](#bitmap) image systems. The word raster comes from the Latin word _rastrum_ (a rake), which is derived from _radere_ (to scrape). The pattern left by the lines of a rake, when drawn straight, resembles the parallel lines of a raster: this line-by-line scanning is what creates a raster. It is a systematic process of covering the area progressively, one line at a time. Although often a great deal faster, it is similar in the most general sense to how one's gaze travels when one reads lines of text. Picture definition is stored in memory area is called Refresh Buffer or Frame Buffer. This memory area holds the intensity value of all the screen points. Stored intensity values are then retrieved from refresh buffer and pointed on the screen one row at a time.

## [README](https://en.wikipedia.org/wiki/README)
A _README_ file contains information about other files in a directory or archive of computer software. A form of [documentation](#software-documentation), it is usally a simple plain text file called `Read Me`, `READ.ME`, `README.txt`, `README.md` (for a text file using [markdown](https://en.wikipedia.org/wiki/Markdown) markup), `README.1ST` - or simply `README`.

The file's name is generally written in uppercase letters. On [Unix-like systems](#unix) in particular this makes it easily noticed - both because lowercase filenames are more usual, and because traditionally the `ls` sommand sorts and displays files in [ASCII-code](#ascii) order, so that uppercase filenames appear first.

## [Read-Only Memory](https://en.wikipedia.org/wiki/Read-only_memory)
_Read-only memory_ (_ROM_) is a type of [non-volatile memory](#non-volatile-memory) used in computers and other electronic devices. Data stored in ROM cannot be electronically modified after the manufacture of the memory device. Read-only memory is useful for storing software that is rarely changed during the life of the system, also known as firmware. Software applications (like video games) for programmable devices can be distributed as plug-in cartridges containing ROM.

## [Register](https://en.wikipedia.org/wiki/Processor_register)
A _processor register_ (or _CPU register_) is a quickly accessible location available to a computer's processors. Registers usually consist of a small amount of fast storage and may be read-only or write-only. Some registers may have specific hardware functions.

## [Reinforcement Learning](https://en.wikipedia.org/wiki/Reinforcement_learning)
_Reinforcement learning_ (_RL_) is an area of [machine learning](#machine-learning) concerned with how intelligent agents ought to take actions in an environment in order to maximize the notion of cumulative reward. Reinforcement learning is one of three basic machine learning paradigms, alongside supervised learning and unsupervised learning.

Reinforcement learning differs from supervised learning in not needing [labelled input/output](#labeled-data) pairs be presented, and in not needing sub-optimal actions to be explicitly corrected. Instead the focus is on finding a balance between exploration (of uncharted territory) and exploitation (of current knowledge).

## [Relay](https://en.wikipedia.org/wiki/Relay)
Electronically controlled, mechanical switches. In a relay a control wire connected to a coil of wire which is used to create an electromagnetic field when current flows through. This field attracts a metal arm inside the relay, completing a circuit.
![Diagram of a relay](./relay.jpg)

## [Resistor](https://en.wikipedia.org/wiki/Resistor)
A _resistor_ is a passive two-terminal electrical component that implements electrical resistance as a circuit element. In electronic circuits, resistors are used to reduce current flow, adjust signal levels, to divide voltages, bias active elements, and terminate transmission lines, among other uses. High-power resistors that can dissipate many watts of electrical power as heat, may be used as part of motor controls, in power distribution systems, or as test loads for generators. Fixed resistors have resistances that only change slightly with temperature, time or operating voltage. Variable resistors can be used to adjust circuit elements (such as a volume control or light dimmer), or as sensing devices for heat, force, or chemical activity.

## [RGB Color Model](https://en.wikipedia.org/wiki/RGB_color_model)
The _RGB color model_ is an additive color model in which red, green, and blue light are added together in various ways to reproduce a broad array of colors. The name of the model comes from the initials of the three additive primary colors, red, green, and blue.

The main purpose of the RGB color model is for the sensing, representation, and display of images in electronic systems, such as televisions and computers, though it has also been used in conventional photography. Before the electronic age, the RGB color model already had a solid theory behind it, based in human perception of colors.

## [Ripple-Carry Adder](https://en.wikipedia.org/wiki/Adder_(electronics)#Ripple-carry_adder)
A _ripple-carry adder_ is a circuit that utilizes multiple [full adders](#full-adder) to add _N_-bit numbers. Each full adder inputs a C<sub>in</sub>, which is the C<sub>out</sub> of the previous adder. This kind of adder is called a ripper-carry adder (RCA) because each carry bit "ripples" to the next full adder. The first (and only the first) full adder may be replaced with a [half addeComputer Vision]r](#half-adder) under the assumption that C<sub>in</sub> = 0.

## [Robot](https://en.wikipedia.org/wiki/Robot)
A _robot_ is a machine - especially one programmable by a computer capable of carrying out a complex series of actions automatically. Robots can be guided by an external control device or the control may be embedded within. Robots may be constructed on the lines of human form, but most robots are machines designed to perform a task with no regard to their aesthetics.

Robots can be autonomous or semi-autonomous and range from humanoids such as Honda's _Advanced Step in Innovative Mobility_ (_ASIMO_) and TOSY's _TOSY Ping Pong Playing Robot_ (_TOPIO_) to industrial robots, medical operating robots, patient assist robots, dog therapy robots, collectively programmed swarm robots, UAV drones such as General Atomics _MQ-1 Predator_, and even microscopic nano robots. By mimicking a lifelike appearance or automating movements, a robot may convey a sense of intelligence or thought of its own. Autonomous things are expected to proliferate in the coming decade, with home robotics and the autonomous car as some of the main drivers.

## [Routing](https://en.wikipedia.org/wiki/Routing)
_Routing_ is the process of selecting a path for traffic in a network or between or across multiple networks. Broadly, routing is performed in many types of networks, including circuit-switched networks, such as the public switched telephone network (PSTN), and [computer networks](#computer-network), such as the [Internet](#internet).

## [RSA](https://en.wikipedia.org/wiki/RSA_(cryptosystem))
_RSA_ (_RivestShamirAdleman_) is a [public-key cryptosystem](#public-key-cryptography) that is widely used for secure data transmission. It is also one of the oldest. The acronym RSA comes from the surnames of Ron Rivest, Adi Shamir, and Leonard Adleman, who publicly described the algorithm in 1977. An equivalent system was developed secretly, in 1973 at GCHQ (the British signals intelligence agency), by the English mathematician Clifford Cocks. That system was declassified in 1997.

In a public-key cryptosystem, the encryption key is public and distinct from the decryption key, which is kept secret (private). An RSA user creates and publishes a public key based on two large prime numbers, along with an auxiliary value. The prime numbers are kept secret. Messages can be encrypted by anyone, via the public key, but can only be decoded by someone who knows the prime numbers.

## [Run-Length Encoding](https://en.wikipedia.org/wiki/Run-length_encoding)
_Run-length encoding_ (_RLE_) is a form of [lossless](#lossless-compression) [data compression](#data-compression) in which runs of data (sequences in which the same data value occurs in many consecutive data elements) are stored as a single data value and count, rather than as the original run. This is most useful on data that contains many such runs. Consider, for example, simple graphic images such as icons, line drawings, Conway's Game of Life, and animations. It is not useful with files that don't have many runs as it could greatly increase the file size.

## [Scanline Rendering](https://en.wikipedia.org/wiki/Scanline_rendering)
_Scanline rendering_ (also _scan line rendering_ and _scan-line rendering_) is an [algorithm](#algorithm) for visible surface determination, in 3D computer graphics, that works on a row-by-row basis rather than a polygon-by-polygon or [pixel](#pixel)-by-pixel basis. All of the polygons to be rendered are first sorted by the top y coordinate at which they first appear, then each row or scan line of the image is computed using the intersection of a scanline with the polygons on the front of the sorted list, while the sorted list is updated to discard no-longer-visible polygons as the active scan line is advanced down the picture.

The main advantage of this method is that sorting vertices along the normal of the scanning plane reduces the number of comparisons between edges. Another advantage is that it is not necessary to translate the coordinates of all vertices from the main memory into the working memory - only vertices defining edges that intersect the current scan line need to be in active memory, and each vertex is read in only once. The main memory is often very slow compared to the link between the [central processing unit](#central-processing-unit) and [cache memory](#cpu-cache), and thus avoiding re-accessing vertices in main memory can provide a substantial speedup.

## [Scheduling](https://en.wikipedia.org/wiki/Scheduling_(computing))
_Scheduling_ is the method by which work is assigned to resources that complete the work. The work may be virtual computation elements such as threads, processes or data flows, which are in turn scheduled onto hardware resources such as processors, network links or expansion cards.

A scheduler is what carries out the scheduling activity. Schedulers are often implemented so they keep all computer resources busy (as in load balancing), allow multiple users to share system resources effectively, or to achieve a target quality of service. Scheduling is fundamental to computation itself, and an intrinsic part of the execution model of a computer system; the concept of scheduling makes it possible to have computer multitasking with a single [central processing unit](#central-processing-unit).

## [Search Engine](https://en.wikipedia.org/wiki/Search_engine)
A _search engine_ is a software system that is designed to carry out web searches ([Internet](#internet) searches), which means to search the [World Wide Web](#world-wide-web) in a systematic way for particular information specified in a textual web search query. The search results are generally presented in a line of results, often referred to as search engine results pages (SERPs). The information may be a mix of [links](#hyperlink) to [web pages](#web-page), images, videos, infographics, articles, research papers, and other types of [files](#file). Some search engines also mine data available in databases or open directories. Unlike web directories, which are maintained only by human editors, search engines also maintain real-time information by running an [algorithm](#algorithm) on a [web crawler](#web-crawler). Internet content that is not capable of being searched by a web search engine is generally described as the deep web.

## [Security Hacker](https://en.wikipedia.org/wiki/Security_hacker)
A _security hacker_ is someone who explores methods for breaching defenses and [exploiting](#exploit) weaknesses in a computer system or [network](#computer-network). Hackers may be motivated by a multitude of reasons, such as profit, protest, information gathering, challenge, recreation, or to evaluate system weaknesses to assist in formulating defenses against potential hackers. The subculture that has evolved around hackers is often referred to as the "computer underground".

## [Self-Driving Car](https://en.wikipedia.org/wiki/Self-driving_car)
A _self-driving car_, also known as an _autonomous vehicle_ (_AV_), _driverless car_, or _robo-car_ is a vehicle that is capable of sensing its environment and moving safely with little or no human input.

Self-driving cars combine a variety of sensors to perceive their surroundings, such as radar, lidar, sonar, GPS, odometry and inertial measurement units. Advanced control systems interpret sensory information to identify appropriate navigation paths, as well as obstacles and relevant signage.

## [Sequential Access Memory](https://en.wikipedia.org/wiki/Sequential_access_memory)
In computing, sequential access memory (SAM) is a class of data storage devices that read stored data in a sequence. This is in contrast to [random access memory](#random-access-memory) (RAM) where data can be accessed in any order. Sequential access devices are usually a form of magnetic storage or optical storage.

While sequential access memory is read in sequence, arbitrary locations can still be accessed by "seeking" to the requested location. This operation, however, is often relatively inefficient (see seek time, rotational latency).

Magnetic sequential access memory is typically used for secondary storage in general-purpose computers due to their higher density at lower cost compared to RAM, as well as resistance to wear and non-volatility. Magnetic tape is a type of sequential access memory still in use; historically, drum memory has also been used.

## [Sequential Logic](https://en.wikipedia.org/wiki/Sequential_logic)
A type of [logic circuit](#logic-gate) whose output depends not only on the present value of its input signals but on the sequence of past inputs (the input history).

## [Session](https://en.wikipedia.org/wiki/Session_(computer_science))
In computer science and [networking](#computer-network) in particular, a _session_ is a temporary and interactive information interchange between two or more communicating devices, or between a computer and user (see login session). A session is established at a certain point in time, and then ‘torn down’ - brought to an end - at some later point. An established communication session may involve more than one message in each direction. A session is typically stateful, meaning that at least one of the communicating parties needs to hold current state information and save information about the session history in order to be able to communicate, as opposed to stateless communication, where the communication consists of independent requests with responses.

## [Session Layer](https://en.wikipedia.org/wiki/Session_layer)
In the seven-layer OSI model of computer networking, the _session layer_ is _layer 5_.

The session layer provides the mechanism for opening, closing and managing a session between end-user application processes, i.e., a semi-permanent dialogue. Communication sessions consist of requests and responses that occur between applications. Session-layer services are commonly used in application environments that make use of remote procedure calls (RPCs).

## [Shading](https://en.wikipedia.org/wiki/Shading)
_Shading_ refers to the depiction of depth perception in 3D models (within the field of 3D computer graphics) or illustrations (in visual art) by varying the level of darkness. Shading tries to approximate local behavior of light on the object's surface and is not to be confused with techniques of adding shadows, such as shadow mapping or shadow volumes, which fall under global behavior of light.

## [Sneakernet](https://en.wikipedia.org/wiki/Sneakernet)
_Sneakernet_ is an informal term for the transfer of electronic information by physically moving media such as [magnetic tape](#magnetic-tape), [floppy disks](#floppy-disk), optical discs, USB flash drives or external [hard drives](#hard-disk-drive) between computers, rather than transmitting it over a [computer network](#computer-network). The term, a tongue-in-cheek play on net(work) as in [Internet](#internet) or [Ethernet](#ethernet), refers to walking in sneakers as the transport mechanism.

## [Social Engineering](https://en.wikipedia.org/wiki/Social_engineering_(security))
In the context of information security, _social engineering_ is the psychological manipulation of people into performing actions or divulging confidential information. This differs from social engineering within the social sciences, which does not concern the divulging of confidential information. A type of confidence trick for the purpose of information gathering, fraud, or system access, it differs from a traditional "con" in that it is often one of many steps in a more complex fraud scheme.

It has also been defined as "any act that influences a person to take an action that may or may not be in their best interests."

## [Software Documentation](https://en.wikipedia.org/wiki/Software_documentation)
Written text or illustration that accompanies computer software or is embedded in the source code. The documentation either explains how the software operates or how to use it, and may mean different things to people in different roles.

## [Software Engineering](https://en.wikipedia.org/wiki/Software_engineering)
_Software engineering_ is the systemic application of engineering approaches to the development of software. Software engineering is a computing discipline.

## [Solid State Electronics](https://en.wikipedia.org/wiki/Solid-state_electronics)
_Solid-state electronics_ means semiconductor electronics: electronic equipment using semiconductor devices such as [transistors](#transistor), [diodes](#diode) and [integrated circuits](#integrated-circuit) (ICs). The term is also used for devices in which semiconductor electronics which have no moving parts replace devices with moving parts, such as the solid-state relay in which transistor switches are used in place of a moving-arm electromechanical [relay](#relay), or the solid-state drive (SSD) a type of semiconductor memory used in computers to replace [hard disk drives](#hard-disk-drive), which store data on a rotating disk.

The term "solid state" became popular in the beginning of the semiconductor era in the 1960s to distinguish this new technology based on the transistor, in which the electronic action of devices occurred in a solid state, from previous electronic equipment that used [vacuum tubes](#vacuum-tube), in which the electronic action occurred in a gaseous state. A semiconductor device works by controlling an electric current consisting of electrons or holes moving within a solid crystalline piece of semiconducting material such as silicon, while the thermionic vacuum tubes it replaced worked by controlling current conducted by a gas of particles, electrons or ions, moving in a vacuum within a sealed tube.

## [Source-Code Repository](https://en.wikipedia.org/wiki/Comparison_of_source-code-hosting_facilities)
A _source-code repository_ is a file archive and web hosting facility for source code of software, documentation, web pages, and other works, accessible either publicly or privately. They are often used by open-source software projects and other multi-developer projects to maintain revision and version history, or [version control](#version-control). Many repositories provide a bug tracking system, and offer release managment, mailing lists, and wiki-based project documentation. Software authors generally retain their copyright when software is posted to a code hosting facility.

## [Spatial Anti-Aliasing](https://en.wikipedia.org/wiki/Spatial_anti-aliasing)
In digital signal processing, _spatial anti-aliasing_ is a technique for minimizing the distortion artifacts known as aliasing when representing a high-resolution image at a lower resolution. Anti-aliasing is used in digital photography, computer graphics, digital audio, and many other applications.

Anti-aliasing means removing signal components that have a higher frequency than is able to be properly resolved by the recording (or sampling) device. This removal is done before (re)sampling at a lower resolution. When sampling is performed without removing this part of the signal, it causes undesirable artifacts such as black-and-white noise.

In signal acquisition and audio, anti-aliasing is often done using an analog anti-aliasing filter to remove the out-of-band component of the input signal prior to sampling with an analog-to-digital converter. In digital photography, optical anti-aliasing filters made of birefringent materials smooth the signal in the spatial optical domain. The anti-aliasing filter essentially blurs the image slightly in order to reduce the resolution to or below that achievable by the digital sensor (the larger the pixel pitch, the lower the achievable resolution at the sensor level).

## [Speculative Execution](https://en.wikipedia.org/wiki/Speculative_execution)
An optimization technique where a computer system performs some task that may not be needed. Work is done before it is known whether it is actually needed, so as to prevent a delay that would have to be incurred by doing the work after it is known that it is needed. If it turns out the work was not needed after all, most changes made by the work are reverted and the results are ignored.

## [Spectrogram](https://en.wikipedia.org/wiki/Spectrogram)
A _spectrogram_ is a visual representation of the spectrum of frequencies of a signal as it varies with time. When applied to an audio signal, spectrograms are sometimes called _sonographs_, _voiceprints_, or _voicegrams_. When the data is represented in a 3D plot they may be called waterfalls.

Spectrograms are used extensively in the fields of music, linguistics, sonar, radar, speech processing, seismology, and others. Spectrograms of audio can be used to identify spoken words phonetically, and to analyse the various calls of animals.

## [Speech Recognition](https://en.wikipedia.org/wiki/Speech_recognition)
_Speech recognition_ is an interdisciplinary subfield of computer science and computational linguistics that develops methodologies and technologies that enable the recognition and translation of spoken language into text by computers. It is also known as _automatic speech recognition_ (_ASR_), _computer speech recognition_ or _speech to text_ (_STT_). It incorporates knowledge and research in the computer science, linguistics and computer engineering fields.

## [Speech Synthesis](https://en.wikipedia.org/wiki/Speech_synthesis)
_Speech synthesis_ is the artificial production of human speech. A computer system used for this purpose is called a speech computer or speech synthesizer, and can be implemented in software or hardware products. A text-to-speech (TTS) system converts normal language text into speech; other systems render symbolic linguistic representations like phonetic transcriptions into speech.

Synthesized speech can be created by concatenating pieces of recorded speech that are stored in a database. Systems differ in the size of the stored speech units; a system that stores phones or diphones provides the largest output range, but may lack clarity. For specific usage domains, the storage of entire words or sentences allows for high-quality output. Alternatively, a synthesizer can incorporate a model of the vocal tract and other human voice characteristics to create a completely "synthetic" voice output.

## [Stack](https://en.wikipedia.org/wiki/Stack_(abstract_data_type))
An abstract data type that serves as a collection of elements, with two main principal operations:
* push, which adds an element to the collection, and
* pop, which removes the most recently added element that was not yet removed.

The order in which elements come off a stack gives rise to its alternative name, _LIFO_ (_last in, first out_). Additionally, a _peek_ operation may give access to the top without modifying the stack. The name "stack" for this type of structure comes from the analogy to a set of physical items stacked on top of each other. The structure makes it easy to take an item off the top of the stack, while getting to an item deeper in the stack may require taking off multiple other items first.

## [Statement](https://en.wikipedia.org/wiki/Statement_(computer_science))
In _computer programming_ a _statement_ is a syntactic unit of an imperative programming language that expresses some action to be carried out. A program written in cuch a language is formed by a sequence of one or more _statements_. A statement may have internal components (e.g., expressions).

## [Static Random-Access Memory](https://en.wikipedia.org/wiki/Static_random-access_memory)
_Static Random-Access Memory_ (_SRAM_) is a type of [RAM](#random-access-memory) that uses latching circuitry ([flip-flop](#flip-flop)) to store each bit. SRAM is [volatile memory](#volatile-memory): data is lost whenever power is removed.

## [Statistical Classification](https://en.wikipedia.org/wiki/Statistical_classification)
In statistics, _classification_ is the problem of identifying to which of a set of categories (sub-populations) a new observation belongs, on the basis of a training set of data containing observations (or instances) whose category membership is known. Examples are assigning a given email to the "spam" or "non-spam" class, and assigning a diagnosis to a given patient based on observed characteristics of the patient (sex, blood pressure, presence or absence of certain symptoms, etc.). Classification is an example of pattern recognition.

## [String](https://en.wikipedia.org/wiki/String_(computer_science))
Traditionally a sequence of characters, either as a literal constant or as some kind of variable. The latter may allow its elements to be mutated and the length changed, or it may be fixed (after creation). A _string_ is generally considered a data type and is often implemented as an [array](#array) data structure of [bytes](#byte) (or words) that stores a sequence of elements, typically characters, using some character encoding. _String_ may also denote more general arrays or other sequence (or list) data types and structures.

## [Stored Program Computer](https://en.wikipedia.org/wiki/Stored-program_computer)
A stored-program computer is a computer that stores program instructions in electronically or optically accessible memory. This contrasts with systems that stored the program instructions with plugboards or similar mechanisms.

The definition is often extended with the requirement that the treatment of programs and data in memory be interchangeable or uniform.

## [Subroutine](https://en.wikipedia.org/wiki/Subroutine)
A _subroutine_ is a sequence of program instructions that performs a specific task, packaged as a unit. This unit can then be used in programs wherever that particular task should be performed. _Subroutines_ can be defined within programs, or separately in _libraries_ that can be used by many programs. In different programming languages, a _subroutine_ may be called a _routine_, _subprogram_, _function_, _method_, or _procedure_. Technically, these terms all have different definitions. The generic, umbrella term _callable unit_ is sometimes used.

## [Substitution Cipher](https://en.wikipedia.org/wiki/Substitution_cipher)
In [cryptography](#cryptography), a _substitution cipher_ is a method of [encrypting](#encryption) in which units of plaintext are replaced with ciphertext, according to a fixed system; the "units" may be single letters (the most common), pairs of letters, triplets of letters, mixtures of the above, and so forth. The receiver deciphers the text by performing the inverse substitution.

Substitution ciphers can be compared with transposition ciphers. In a transposition cipher, the units of the plaintext are rearranged in a different and usually quite complex order, but the units themselves are left unchanged. By contrast, in a substitution cipher, the units of the plaintext are retained in the same sequence in the ciphertext, but the units themselves are altered.

## [Supercomputer](https://en.wikipedia.org/wiki/Supercomputer)
A _supercomputer_ is a computer with a high level of performance as compared to a general-purpose computer. Supercomputers play an important role in the field of computational science, and are used for a wide range of computationally intensive tasks in various fields, including quantum mechanics, weather forecasting, climate research, oil and gas exploration, molecular modeling, and physical simulations.

## [Superscalar](https://simple.wikipedia.org/wiki/Superscalar)
A _superscalar [CPU](#central-processing-unt)_ design makes a form of parallel computing calle instruction-level parallelism inside a single CPU, which allows more work to be done at the same clock rate. This means the CPU executes more than one instruction during a clock cycle by running multiple instructions at the same time (called _instruction dispatching_) on duplicate functional units.

## [Support Vector Machine](https://en.wikipedia.org/wiki/Support-vector_machine)
In [machine learning](#machine-learning), _support-vector machines_ (_SVMs_, also _support-vector networks_) are supervised learning models with associated learning algorithms that analyze data for [classification](#statistical-classification) and regression analysis. Developed at AT&T Bell Laboratories by Vapnik with colleagues (Boser et al., 1992, Guyon et al., 1993, Vapnik et al., 1997), SVMs are one of the most robust prediction methods, being based on statistical learning frameworks or VC theory proposed by Vapnik and Chervonenkis (1974) and Vapnik (1982, 1995). Given a set of training examples, each marked as belonging to one of two categories, an SVM training algorithm builds a model that assigns new examples to one category or the other, making it a non-probabilistic binary linear classifier (although methods such as Platt scaling exist to use SVM in a probabilistic classification setting). An SVM maps training examples to points in space so as to maximise the width of the gap between the two categories. New examples are then mapped into that same space and predicted to belong to a category based on which side of the gap they fall.

## [Symmetric-Key Algorithm](https://en.wikipedia.org/wiki/Symmetric-key_algorithm)
_Symmetric-key algorithms_ are [algorithms](#algorithm) for [cryptography](#cryptography) that use the same cryptographic keys for both [encryption](#encryption) of plaintext and decryption of ciphertext. The keys may be identical or there may be a simple transformation to go between the two keys. The keys, in practice, represent a shared secret between two or more parties that can be used to maintain a private information link. This requirement that both parties have access to the secret key is one of the main drawbacks of symmetric key encryption, in comparison to [public-key encryption](#public-key-cryptography) (also known as asymmetric key encryption).

## [Syntax](https://en.wikipedia.org/wiki/Syntax_(programming_languages))
The _syntax_ of a computer language is the set of rules that defines th combinations of symbols that are considered to be correctly structured [statements](#statement) and expressions in that language. This applies to both _programming languages_ where the document represents source code, and to _markup languages_, where the document represents data.

## [TCP Congestion Control](https://en.wikipedia.org/wiki/TCP_congestion_control)
[Transmission Control Protocol](#transmission-control-protocol) (TCP) uses a network congestion-avoidance [algorithm](#algorithm) that includes various aspects of an additive increase/multiplicative decrease (AIMD) scheme, along with other schemes including slow start and congestion window, to achieve congestion avoidance. The TCP congestion-avoidance algorithm is the primary basis for congestion control in the [Internet](#internet). Per the end-to-end principle, congestion control is largely a function of internet hosts, not the [network](#computer-network) itself. There are several variations and versions of the algorithm implemented in protocol stacks of operating systems of computers that connect to the Internet.

## [Teleprinter](https://en.wikipedia.org/wiki/Teleprinter)
A _teleprinter_ (_teletypewriter_, _teletype_ or _TTY_) is an electromechanical device that can be used to send and receive typed messages through various communications channels, in both point-to-point and point-to-multipoint configurations. Initially they were used in telegraphy, which developed in the late 1830s and 1840s as the first use of electrical engineering, though teleprinters were not used for telegraphy until 1887 at the earliest. The machines were adapted to provide a user interface to early mainframe computers and minicomputers, sending typed data to the computer and printing the response. Some models could also be used to create [punched tape](#punched-tape) for data storage (either from typed input or from data received from a remote source) and to read back such tape for local printing or transmission.

## [Text Mode](https://en.wikipedia.org/wiki/Text_mode)
_Text mode_ is a computer display mode in which content is internally represented on a computer screen in terms of characters rather than individual [pixels](#pixel). Typically, the screen consists of a uniform rectangular grid of character cells, each of which contains one of the characters of a character set. Text mode is contrasted to all points addressable (APA) mode or other kinds of computer graphics modes.

Text mode applications communicate with the user with command-line interfaces and text user interfaces. Many character sets used in text mode applications also contain a limited set of predefined semi-graphical characters usable for drawing boxes, and other rudimentary graphics which can be used to highlight the content or to simulate widget or control interface objects found in GUI programs.

## [Texture Mapping](https://en.wikipedia.org/wiki/Texture_mapping)
_Texture mapping_ is a method for defining high frequency detail, surface texture, or color information on a computer-generated graphic or 3D model. The original technique was pioneered by Edwin Catmull in 1974.

Texture mapping originally referred to _diffuse mapping_, a method that simply mapped pixels from a texture to a 3D surface ("wrapping" the image around the object). In recent decades, the advent of _multi-pass rendering_, _multitexturing_, _mipmaps_, and more complex mappings such as _height mapping_, _bump mapping_, _normal mapping_, _displacement mapping_, _reflection mapping_, _specular mapping_, _occlusion mapping_, and many other variations on the technique (controlled by a materials system) have made it possible to simulate near-photorealism in real time by vastly reducing the number of polygons and lighting calculations needed to construct a realistic and functional 3D scene.

## [Threat Model](https://en.wikipedia.org/wiki/Threat_model)
_Threat modeling_ is a process by which potential threats, such as structural vulnerabilities or the absence of appropriate safeguards, can be identified, enumerated, and mitigations can be prioritized. The purpose of threat modeling is to provide defenders with a systematic analysis of what controls or defenses need to be included, given the nature of the system, the probable attacker's profile, the most likely attack vectors, and the assets most desired by an attacker. Threat modeling answers questions like _“Where am I most vulnerable to attack?”_, _“What are the most relevant threats?”_, and _“What do I need to do to safeguard against these threats?”_.

Conceptually, most people incorporate some form of threat modeling in their daily life and don't even realize it. Commuters use threat modeling to consider what might go wrong during the morning drive to work and to take preemptive action to avoid possible accidents. Children engage in threat modeling when determining the best path toward an intended goal while avoiding the playground bully. In a more formal sense, threat modeling has been used to prioritize military defensive preparations since antiquity.

## [Three Laws of Robotics](https://en.wikipedia.org/wiki/Three_Laws_of_Robotics)
The _Three Laws of Robotics_ (often shortened to _The Three Laws_ or known as _Asimov's Laws_) are a set of rules devised by science fiction author Isaac Asimov. The rules were introduced in his 1942 short story "Runaround" (included in the 1950 collection _I, Robot_), although they had been foreshadowed in some earlier stories. The Three Laws, quoted from the "Handbook of Robotics, 56th Edition, 2058 A.D.", are:

**First Law**

A robot may not injure a human being or, through inaction, allow a human being to come to harm.

**Second Law**

A robot must obey the orders given it by human beings except where such orders would conflict with the First Law.

**Third Law**

A robot must protect its own existence as long as such protection does not conflict with the First or Second Law.

## [Time-Sharing](https://en.wikipedia.org/wiki/Time-sharing)
In computing, time-sharing is the sharing of a computing resource among many users at the same time by means of multiprogramming and multi-tasking.

Its emergence as the prominent model of computing in the 1970s represented a major technological shift in the history of computing. By allowing many users to interact concurrently with a single computer, time-sharing dramatically lowered the cost of providing computing capability, made it possible for individuals and organizations to use a computer without owning one, and promoted the interactive use of computers and the development of new interactive applications.

## [Traceroute](https://en.wikipedia.org/wiki/Traceroute)
In computing, _traceroute_ and _tracert_ are [computer network](#computer-network) diagnostic commands for displaying possible [routes](#routing) (paths) and measuring transit delays of [packets](#network-packet) across an [Internet Protocol](#internet-protocol) (IP) network. The history of the route is recorded as the round-trip times of the packets received from each successive host (remote node) in the route (path); the sum of the mean times in each hop is a measure of the total time spent to establish the connection. Traceroute proceeds unless all (usually three) sent packets are lost more than twice; then the connection is lost and the route cannot be evaluated. Ping, on the other hand, only computes the final round-trip times from the destination point.

For Internet Protocol Version 6 (IPv6) the tool sometimes has the name traceroute6 or tracert6.

## [Training Dataset](https://en.wikipedia.org/wiki/Training,_validation,_and_test_sets#Training_dataset)
A _training dataset_ is a dataset of examples used during the learning process and is used to fit the parameters (e.g., weights) of, for example, a [classifier](#statistical-classification).

For classification tasks, a supervised learning algorithm looks at the training dataset to determine, or learn, the optimal combinations of variables that will generate a good predictive model. The goal is to produce a trained (fitted) model that generalizes well to new, unknown data. The fitted model is evaluated using “new” examples from the held-out datasets (validation and test datasets) to estimate the model’s accuracy in classifying new data. To reduce the risk of issues such as overfitting, the examples in the validation and test datasets should not be used to train the model.

Most approaches that search through training data for empirical relationships tend to overfit the data, meaning that they can identify and exploit apparent relationships in the training data that do not hold in general.

## [Transistor](https://en.wikipedia.org/wiki/Transistor)
Similar to a [relay](#relay) or [vacuum tube](#vacuum-tube) a _transistor_ is a switch that can be opened or closed via the application of a current to a control wire. Typically a transistor consists of two electrodes separated by a gate electrode (a semiconductor). By changing the electrical charge of the gate, the conductivity of the semiconductor could be manipulated.

## [Transistor Computer](https://en.wikipedia.org/wiki/Transistor_computer)
A _transistor computer_, now often called a _second generation computer_, is a computer which uses [discrete](#discrete-component) [transistors](#transistor) instead of [vacuum tubes](#vacuum-tube). The first generation of electronic computers used vacuum tubes, which generated large amounts of heat, were bulky and unreliable. A second generation of computers, through the late 1950s and 1960s features circuit boards filled with individual transistors and magnetic core memory. These machines remained the mainstream design into the late 1960s, when [integrated circuits](#integrated-circuit) started appearing and led to the third generation computer.

## [Transmission Control Protocol](https://en.wikipedia.org/wiki/Transmission_Control_Protocol)
The _Transmission Control Protocol_ (_TCP_) is one of the main protocols of the [Internet protocol suite](#internet-protocol-suite). It originated in the initial network implementation in which it complemented the [Internet Protocol](#internet-protocol) (IP). Therefore, the entire suite is commonly referred to as _TCP/IP_. TCP provides reliable, ordered, and error-checked delivery of a stream of octets (bytes) between applications running on hosts communicating via an IP network. Major internet applications such as the [World Wide Web](#world-wide-web), email, remote administration, and file transfer rely on TCP, which is part of the Transport Layer of the TCP/IP suite. SSL/TLS often runs on top of TCP.

TCP is connection-oriented, and a connection between client and server is established before data can be sent. The server must be listening (passive open) for connection requests from clients before a connection is established. Three-way handshake (active open), retransmission, and error-detection adds to reliability but lengthens latency. Applications that do not require reliable data stream service may use the [User Datagram Protocol](#user-datagram-protocol) (UDP), which provides a connectionless datagram service that prioritizes time over reliability. TCP employs network congestion avoidance. However, there are vulnerabilities to TCP including denial of service, connection hijacking, TCP veto, and reset attack.

Though TCP is a complex protocol, its basic operation has not changed significantly since its first specification. TCP is still dominantly used for the web, i.e. for the [HTTP protocol](#hypertext-transfer-protocol), and later HTTP/2, while not used by latest standard HTTP/3.

## [Transport Layer](https://en.wikipedia.org/wiki/Transport_layer)
In [computer networking](#computer-network), the _transport layer_ is a conceptual division of methods in the layered architecture of protocols in the network stack in the [Internet protocol suite](#internet-protocol-suite) and the OSI model. The protocols of this layer provide host-to-host communication services for applications. It provides services such as connection-oriented communication, reliability, flow control, and multiplexing.

## [Transposition Cipher](https://en.wikipedia.org/wiki/Transposition_cipher)
In [cryptography](#cryptography), a _transposition cipher_ is a method of [encryption](#encryption) by which the positions held by units of plaintext (which are commonly characters or groups of characters) are shifted according to a regular system, so that the ciphertext constitutes a _permutation_ of the plaintext. That is, the order of the units is changed (the plaintext is reordered). Mathematically a bijective function is used on the characters' positions to encrypt and an inverse function to decrypt.

## [Tree](https://en.wikipedia.org/wiki/Tree_(data_structure))
A _tree_ is a widely used abstract data type that simulates a hierarchical _tree structure_, with a root value and subtrees of children with a parent node, represented as a set of linked [nodes](#node).

![Tree](./tree.svg)
<br/>
A generic, and so non-binary, unsorted, some labels duplicated, arbitrary diagram of a tree.

## [Trojan Horse](https://en.wikipedia.org/wiki/Trojan_horse_(computing))
In computing, a _Trojan horse_ (or simply _trojan_) is any [malware](#malware) which misleads users of its true intent. The term is derived from the Ancient Greek story of the deceptive Trojan Horse that led to the fall of the city of Troy.

Trojans are generally spread by some form of [social engineering](#social-engineering), for example where a user is duped into executing an email attachment disguised to appear not suspicious, (e.g., a routine form to be filled in), or by clicking on some fake advertisement on social media or anywhere else. Although their payload can be anything, many modern forms act as a backdoor, contacting a controller which can then have unauthorized access to the affected computer. Trojans may allow an attacker to access users' personal information such as banking information, passwords, or personal identity. It can also delete a user's files or infect other devices connected to the network. Ransomware attacks are often carried out using a trojan.

Unlike computer viruses, worms, and rogue security software, trojans generally do not attempt to inject themselves into other files or otherwise propagate themselves.

## [Trusted Computing Base](https://en.wikipedia.org/wiki/Trusted_computing_base)
The _trusted computing base_ (_TCB_) of a computer system is the set of all hardware, firmware, and/or software components that are critical to its security, in the sense that bugs or vulnerabilities occurring inside the TCB might jeopardize the security properties of the entire system. By contrast, parts of a computer system outside the TCB must not be able to misbehave in a way that would leak any more privileges than are granted to them in accordance to the security policy.

The careful design and implementation of a system's trusted computing base is paramount to its overall security. Modern [operating systems](#operating-system) strive to reduce the size of the TCB so that an exhaustive examination of its code base (by means of manual or computer-assisted software audit or program verification) becomes feasible.

## [Turing Completeness](https://en.wikipedia.org/wiki/Turing_completeness)
In _computability theory_, a system of data-manipulation rules (such as a computer's instruction set, a programming language, or a cellular automaton) is said to be _Turing-complete_ or _computationally universal_ if it can be used to simulate any [Turing machine](#turing-machine). This means that this system is able to recognize or decide other data-manipulation rule sets. Turing completeness is used as a way to express the power of such a data-manipulation rule set. Virtually all programming languages today are Turing-complete. The concept is named after English mathematician and computer scientist [Alan Turing](https://en.wikipedia.org/wiki/Alan_Turing).

## [Turing Machine](https://en.wikipedia.org/wiki/Turing_machine)
A _Turing machine_ is a mathematical model of computation that defines an abstract machine, which manipulates symbols on a strip of tape according to a table of rules. Despite the model's simplicity, given any computer [algorithm](#algorithm), a Turing machine capable of simulating that algorithm's logic can be constructed.

The machine operates on an infinite memory tape divided into discrete "cells". The machine positions its "head" over a cell and "reads" or "scans" the symbol there. Then, as per the symbol and the machine's own present state in a "finite table" of user-specified instructions, the machine writes a symbol (e.g., a digit or letter fom a finite alphabet) in the cell (some models allow symbol erasure or no writing) then either moves the tape one cell left or right (some models allow no motion, some models move the head) then (as determined by the observed symbol and the machines' own state in the table) either proceeds to a subsequent instruction or halts the computation.

## [Uncanny Valley](https://en.wikipedia.org/wiki/Uncanny_valley)
In aesthetics, the _uncanny valley_ is a hypothesized relationship between the degree of an object's resemblance to a human being and the emotional response to such an object. The concept suggests that humanoid objects which imperfectly resemble actual human beings provoke uncanny or strangely familiar feelings of eeriness and revulsion in observers. "Valley" denotes a dip in the human observer's affinity for the replica, a relation that otherwise increases with the replica's human likeness.

## [Unix](https://en.wikipedia.org/wiki/Unix)
_Unix_ is a family of multitasking, multiuser computer operating systems that derive from the original AT&T Unix, development starting in the 1970s at the Bell Labs research center by Ken Thompson, Dennis Ritchie, and others.

Unix systems are characterized by a modular design that is sometimes called the "Unix philosophy". According to this philosophy, the operating system should provide a set of simple tools, each of which performs a limited, well-defined function. A unified filesystem (the Unix filesystem) and an inter-process communication mechanism known as "pipes" serve as the main means of communication, and a shell scripting and command language (the Unix shell) is used to combine the tools to perform complex workflows.

Unix distinguishes itself from its predecessors as the first portable operating system: almost the entire operating system is written in the C programming language, which allows Unix to operate on numerous platforms.

## [URL](https://en.wikipedia.org/wiki/URL)
A _Uniform Resource Locator_ (_URL_), colloquially termed a web address, is a reference to a web resource that specifies its location on a [computer network](#computer-network) and a mechanism for retrieving it. A URL is a specific type of Uniform Resource Identifier (URI), although many people use the two terms interchangeably. URLs occur most commonly to reference [web pages](#web-page) (http), but are also used for file transfer (ftp), email (mailto), database access (JDBC), and many other applications.

## [Usability](https://en.wikipedia.org/wiki/Usability)
_Usability_ can be described as the capacity of a system to provide a condition for its users to perform the tasks safely, effectively, and efficiently while enjoying the experience. In software engineering, usability is the degree to which a software can be used by specified consumers to achieve quantified objectives with effectiveness, efficiency, and satisfaction in a quantified context of use.

## [User Datagram Protocol](https://en.wikipedia.org/wiki/User_Datagram_Protocol)
In [computer networking](#computer-network), the _User Datagram Protocol_ (_UDP_) is one of the core members of the [Internet protocol suite](#internet-protocol-suite). The protocol was designed by David P. Reed in 1980 and formally defined in RFC 768. With UDP, computer applications can send messages, in this case referred to as datagrams, to other hosts on an [Internet Protocol](#internet-protocol) (IP) network. Prior communications are not required in order to set up communication channels or data paths.

UDP uses a simple connectionless communication model with a minimum of protocol mechanisms. UDP provides checksums for data integrity, and [port](#port) numbers for addressing different functions at the source and destination of the datagram. It has no handshaking dialogues, and thus exposes the user's program to any unreliability of the underlying network; there is no guarantee of delivery, ordering, or duplicate protection. If error-correction facilities are needed at the network interface level, an application may use [Transmission Control Protocol](#transmission-control-protocol) (TCP) or Stream Control Transmission Protocol (SCTP) which are designed for this purpose.

UDP is suitable for purposes where error checking and correction are either not necessary or are performed in the application; UDP avoids the overhead of such processing in the protocol stack. Time-sensitive applications often use UDP because dropping packets is preferable to waiting for packets delayed due to retransmission, which may not be an option in a real-time system.

## [Utah Teapot](https://en.wikipedia.org/wiki/Utah_teapot)
The _Utah teapot_, or the _Newell teapot_, is a 3D test model that has become a standard reference object and an in-joke within the computer graphics community. It is a mathematical model of an ordinary, Melitta-brand teapot that appears solid, cylindrical, and partially convex. Using a teapot model is considered the 3D equivalent of a "Hello, World!" program, a way to create an easy 3D scene with a somewhat complex model acting as the basic geometry for a scene with a light setup. Some programming libraries, such as the OpenGL Utility Toolkit, even have functions dedicated to drawing teapots.

The teapot model was created in 1975 by early computer graphics researcher [Martin Newell](https://en.wikipedia.org/wiki/Martin_Newell_(computer_scientist)), a member of the pioneering graphics program at the University of Utah. It was one of the first to be modeled (using bézier curves) rather than precisely measured.

## [Vacuum Tube](https://en.wikipedia.org/wiki/Vacuum_tube)
A device that controls electric current flow in a high vacuum between electrodes to which an electric potential difference has been applied.

## [Vector Monitor](https://en.wikipedia.org/wiki/Vector_monitor)
A _vector monitor_, _vector display_, or _calligraphic display_ is a display device used for computer graphics up through the 1970s. It is a type of [CRT](#cathode-ray-tube), similar to that of an early oscilloscope. In a vector display, the image is composed of drawn lines rather than a grid of glowing pixels as in raster graphics. The electron beam follows an arbitrary path tracing the connected sloped lines, rather than following the same horizontal raster path for all images. The beam skips over dark areas of the image without visiting their points.

## [Version Control](https://en.wikipedia.org/wiki/Version_control)
In software engineering, _version control_ (also known as _revision control_, _source control_, or _source code management_) is a class of systems responsible for managing changes to computer programs, documents, large web sites, or other collections of information. Version control is a component of software configuration management.

Changes are usually identified by a number or letter code, termed the "revision number", "revision level", or simply "revision". For example, an initial set of files is "revision 1". When the first change is made, the resulting set is "revision 2", and so on. Each revision is associated with a timestamp and the person making the change. Revisions can be compared, restored, and with some types of files, merged.

The need for a logical way to organize and control revisions has existed for almost as long as writing has existed, but revision control became much more important, and complicated, when the era of computing began. The numbering of book editions and of specification revisions are examples that date back to the print-only era. Today, the most capable (as well as complex) revision control systems are those used in software development, where a team of people may concurrently make changes to the same files.

_Version control systems_ (_VCS_) are most commonly run as stand-alone applications, but revision control is also embedded in various types of software such as word processors and spreadsheets, collaborative web docs and various content management systems, e.g., Wikipedia's page history. Revision control allows for the ability to revet a document to a previous revision, which is critical for allowing editors to track each other's edits, correct mistakes, and defend against vandalism and spamming in wikis.

## [Video Card](https://en.wikipedia.org/wiki/Video_card)
A _video card_ (also called a _graphics card_, _display card_, _graphics adapter_, or _display adapter_) is an expansion card which generates a feed of output images to a display device (such as a computer monitor). Frequently, these are advertised as discrete or dedicated graphics cards, emphasizing the distinction between these and integrated graphics. At the core of both is the [graphics processing unit](#graphics-processing-unit) (GPU), which is the main part that does the actual computations, but should not be confused with the video card as a whole, although "GPU" is often used as a metonymic shorthand to refer to video cards

## [Video RAM](https://en.wikipedia.org/wiki/Video_RAM_(dual-ported_DRAM))
_Video RAM_, or _VRAM_, is a dual-ported variant of [dynamic RAM](#dynamic-random-access-memory) (DRAM), which was once commonly used to store the framebuffer in graphics adapters. Note that most computers and game consoles do not use this form of memory, and dual-ported VRAM should not be confused with other forms of video memory.

## [Viola-Jones Object Detection Framework](https://en.wikipedia.org/wiki/Viola%E2%80%93Jones_object_detection_framework)
The _Viola-Jones object detection framework_ is an object detection framework which was proposed in 2001 by Paul Viola and Michael Jones. Although it can be trained to detect a variety of object classes, it was motivated primarily by the problem of face detection.

## [Virtual Memory](https://en.wikipedia.org/wiki/Virtual_memory)
_Virtual memory_ (also _virtual storage_) is a memory management technique that provides an "idealized abstraction of the storage resources that are actually available on a given machine" which "create the illusion of a very large (main) memory."

The computer's [operating system](#operating-system), using a combination of hardware and software, maps memory addresses used by a program, called _virtual addresses_, into _physical addresses_ in computer memory. Main storage, as seen by a process or task, appears as a contiguous address space or collection of contiguous segments. The operating system manages virtual address spaces and the assignment of real memory to virtual memory. Address translation hardware in the [CPU](#central-processing-unit), often referred to as a memory management unit (MMU), automatically translates virtual addresses to physical addresses. Software within the operating system may extend these capabilities to provide a virtual address space that can exceed the capacity of real memory and thus reference more memory than is physically present in the computer.

The primary benefits of virtual memory include freeing applications from having to manage a shared memory space, increased security due to memory isolation, and being able to conceptually use more memory than might be physically available, using the technique of _paging_.

## [Voice User Interface](https://en.wikipedia.org/wiki/Voice_user_interface)
A _voice-user interface_ (_VUI_) makes spoken human interaction with computers possible, using [speech recognition](#speech-recognition) to understand spoken commands and answer questions, and typically text to speech to play a reply. A _voice command device_ (_VCD_) is a device controlled with a voice user interface.

Voice user interfaces have been added to automobiles, home automation systems, [computer operating systems](#operating-system), home appliances like washing machines and microwave ovens, and television remote controls. They are the primary way of interacting with virtual assistants on smartphones and smart speakers. Older automated attendants (which route phone calls to the correct extension) and interactive voice response systems (which conduct more complicated transactions over the phone) can respond to the pressing of keypad buttons via DTMF tones, but those with a full voice user interface allow callers to speak requests and responses without having to press any buttons.

## [Volatile Memory](https://en.wikipedia.org/wiki/Volatile_memory)
Computer memory that requires power to maintain the stored information. It retains its contents while powered on but when the power is interrupted the stored data is quickly lost. In addition to being faster than forms of mass storage, volatility can protect sensitive information (as it becomes unavailable on power-down).

## [Wafer](https://en.wikipedia.org/wiki/Wafer_(electronics))
In electronics, a _wafer_ (also called a _slice_ or _substrate_) is a thin slice of semicondutor, such as crytalline silicon (c-Si), used for the fabrication of [integrated circuits](#integrated-circuit) and, in photovoltaics, to manufacture solar cells. The wafer serves as the substrate for microelectronic devices built in and upon the wafer. It undergoes many microfabrication processes, such as doping, ion implantation, etching, thin-film deposition of various materials, and photolithographic patterning. Finally, the individual microcircuits are separated by wafer dicing and packaged as an integrated circuit.

## [Waveform](https://en.wikipedia.org/wiki/Waveform)
In electronics, acoustics, and related fields, the _waveform_ of a signal is the shape of its graph as a function of time, independent of its time and magnitude scales and of any displacement in time.

In electronics, the term is usually applied to periodically varying voltages, currents, or electromagnetic fields. In acoustics, it is usually applied to steady periodic soundsvariations of pressure in air or other media. In these cases, the waveform is an attribute that is independent of the frequency, amplitude, or phase shift of the signal. The term can also be used for non-periodic signals, like chirps and pulses.

## [Weak AI](https://en.wikipedia.org/wiki/Weak_AI)
_Weak artificial intelligence_ (_weak AI_), is [artificial intelligence](#artificial-intelligence) that implements a limited part of mind, or as _narrow AI_, is focused on one narrow task. In John Searle's terms it “would be useful for testing hypothesis about minds, but would not actually be minds”. Contrast with [strong AI](#artificial-general-intelligence) which is defined as a machine with the ability to apply intelligence to any problem, rather than just one specific problem, sometimes considered to require consciousness, sentience and mind.

## [Web Browser](https://en.wikipedia.org/wiki/Web_browser)
A _web browser_ (commonly referred to as a _browser_) is a software application for accessing information on the [World Wide Web](#world-wide-web). When a user requests a web page from a particular website, the web browser retrieves the necessary content from a [web server](#web-server) and then displays the page on the user's device.

A web browser is not the same thing as a search engine, though the two are often confused. For a user, a search engine is just a website that provides [links](#hyperlink) to other websites. However, to connect to a website's server and display its web pages, a user must have a web browser installed.

## [Web Crawler](https://en.wikipedia.org/wiki/Web_crawler)
A _Web crawler_, sometimes called a _spider_ or _spiderbot_ and often shortened to _crawler_, is an Internet bot that systematically browses the [World Wide Web](#world-wide-web), typically for the purpose of Web indexing (web spidering).

Web [search engines](#search-engine) and some other websites use Web crawling or spidering software to update their web content or indices of other sites' web content. Web crawlers copy pages for processing by a search engine, which indexes the downloaded pages so that users can search more efficiently.

## [Web Indexing](https://en.wikipedia.org/wiki/Web_indexing)
_Web indexing_, or _internet indexing_, comprises methods for indexing the contents of a website or of the [Internet](#internet) as a whole. Individual websites or intranets may use a back-of-the-book index, while [search engines](#search-engine) usually use keywords and [metadata](#metadata) to provide a more useful vocabulary for Internet or onsite searching. With the increase in the number of periodicals that have articles online, web indexing is also becoming important for periodical websites.

## [Web Page](https://en.wikipedia.org/wiki/Web_page)
A _web page_ (or _webpage_) is a specific collection of information provided by a website and displayed to a user in a [web browser](#web-browser). A website typically consists of many web pages [linked](#hyperlink) together in a coherent fashion. The name "web page" is a metaphor of paper pages bound together into a book.

## [Web Server](https://en.wikipedia.org/wiki/Web_server)
A _web server_ is server software, or hardware dedicated to running this software, that can satisfy client requests on the [World Wide Web](#world-wide-web). A web server can, in general, contain one or more websites. A web server processes incoming network requests over HTTP and several other related protocols.

The primary function of a web server is to store, process and deliver web pages to clients. The communication between client and server takes place using the [Hypertext Transfer Protocol](#hypertext-transfer-protocol) (HTTP). Pages delivered are most frequently [HTML](#html) documents, which may include images, style sheets and scripts in addition to the text content.

A user agent, commonly a [web browser](#web-browser) or [web crawler](#web-crawler), initiates communication by making a request for a specific resource using HTTP and the server responds with the content of that resource or an error message if unable to do so. The resource is typically a real file on the server's secondary storage, but this is not necessarily the case and depends on how the web server is implemented.

## [White Hat](https://en.wikipedia.org/wiki/White_hat_(computer_security))
The term _"white hat"_ in [Internet](#internet) slang refers to an ethical [computer hacker](#security-hacker), or a computer security expert, who specializes in penetration testing and in other testing methodologies that ensures the security of an organization's information systems. Ethical hacking is a term meant to imply a broader category than just penetration testing. Contrasted with black hat, a malicious hacker, the name comes from Western films, where heroic and antagonistic cowboys might traditionally wear a white and a black hat respectively. While a white hat hacker hacks under good intentions with permission, and a black hat hacker, most often unauthorized, has malicious intent, there is a third kind known as a grey hat hacker who hacks with good intentions but at times without permission.

White hat hackers may also work in teams called "sneakers and/or hacker clubs", red teams, or tiger teams.

## [Wi-Fi](https://en.wikipedia.org/wiki/Wi-Fi)
_Wi-Fi_ is a family of wireless network protocols, based on the IEEE 802.11 family of standards, which are commonly used for [local area networking](#local-area-network) of devices and Internet access. Wi‑Fi is a trademark of the non-profit Wi-Fi Alliance, which restricts the use of the term Wi-Fi Certified to products that successfully complete interoperability certification testing. As of 2017, the Wi-Fi Alliance consisted of more than 800 companies from around the world. As of 2019, over 3.05 billion Wi-Fi enabled devices are shipped globally each year. Devices that can use Wi-Fi technologies include [personal computer](#personal-computer) desktops and laptops, smartphones and tablets, smart TVs, printers, smart speakers, cars, and drones.

## [Wide Area Network](https://en.wikipedia.org/wiki/Wide_area_network)
A _wide area network_ (_WAN_) is a telecommunications network that extends over a large geographic area for the primary purpose of [computer networking](#computer-network). Wide area networks are often established with leased telecommunication circuits.

Businesses, as well as schools and government entities, use wide area networks to relay data to staff, students, clients, buyers and suppliers from various locations across the world. In essence, this mode of telecommunication allows a business to effectively carry out its daily function regardless of location. The [Internet](#internet) may be considered a WAN.

## [Window](https://en.wikipedia.org/wiki/Window_(computing))
In computing, a _window_ is a graphical control element. It consists of a visual area containing some of the [graphical user interface](#graphical-user-interface) of the program it belongs to and is framed by a window decoration. It usually has a rectangular shape that can overlap with the area of other windows. It displays the output of and may allow input to one or more processes.

Windows are primarily associated with graphical displays, where they can be manipulated with a pointer by employing some kind of pointing device. Text-only displays can also support windowing, as a way to maintain multiple independent display areas, such as multiple buffers in Emacs. Text windows are usually controlled by [keyboard](#computer-keyboard), though some also respond to the [mouse](#computer-mouse).

A graphical user interface (GUI) using windows as one of its main "metaphors" is called a windowing system, whose main components are the display server and the window manager.

## [WIMP](https://en.wikipedia.org/wiki/WIMP_(computing))
In humancomputer interaction, _WIMP_ stands for "[windows](#window), icons, menus, pointer", denoting a style of interaction using these elements of the user interface. Other expansions are sometimes used, such as substituting "[mouse](#computer-mouse)" and "mice" for menus, or "pull-down menu" and "pointing" for pointer.

Though the acronym has fallen into disuse, it has often been likened to the term [graphical user interface](#graphical-user-interface) (GUI). Any interface that uses graphics can be called a GUI, and WIMP systems derive from such systems. However, while all WIMP systems use graphics as a key element (the icon and pointer elements), and therefore are GUIs, the reverse is not true. Some GUIs are not based in windows, icons, menus, and pointers. For example, most mobile phones represent actions as icons and menus, but do not often don't rely on a conventional pointer or containerized windows to host program interactions.

WIMP interaction was developed at [Xerox PARC](https://en.wikipedia.org/wiki/PARC_(company)) (see [Xerox Alto](https://en.wikipedia.org/wiki/Xerox_Alto), developed in 1973) and popularized with [Apple's](https://en.wikipedia.org/wiki/Apple_Inc.) introduction of the [Macintosh](https://en.wikipedia.org/wiki/Macintosh) in 1984, which added the concepts of the "menu bar" and extended window management.

## [Wire-Frame Model](https://en.wikipedia.org/wiki/Wire-frame_model)
A _wire-frame model_, also _wireframe model_, is a visual representation of a three-dimensional (3D) physical object used in 3D computer graphics. It is created by specifying each edge of the physical object where two mathematically continuous smooth surfaces meet, or by connecting an object's constituent vertices using (straight) lines or curves. The object is projected into screen space and rendered by drawing lines at the location of each edge. The term "wire frame" comes from designers using metal wire to represent the three-dimensional shape of solid objects. 3D wire frame computer models allow for the construction and manipulation of solids and solid surfaces. 3D solid modeling efficiently draws higher quality representations of solids than conventional line drawing.

Using a wire-frame model allows for the visualization of the underlying design structure of a 3D model. Traditional two-dimensional views and drawings/renderings can be created by the appropriate rotation of the object, and the selection of hidden line removal via cutting planes.

## [World Wide Web](https://en.wikipedia.org/wiki/World_Wide_Web)
The _World Wide Web_ (_WWW_), commonly known as the _Web_, is an information system where documents and other web resources are identified by Uniform Resource Locators (URLs, such as https://example.com/), which may be [interlinked](#hyperlink) by [hypertext](#hypertext), and are accessible over the [Internet](#internet). The resources of the Web are transferred via the [Hypertext Transfer Protocol](#hypertext-transfer-protocol) (HTTP), may be accessed by users by a software application called a [web browser](#web-browser), and are published by a software application called a [web server](#web-server). The World Wide Web is not synonymous with the Internet, which pre-existed the Web in some form by over two decades and upon whose technologies the Web is built.

English scientist Tim Berners-Lee invented the World Wide Web in 1989. He wrote the first web browser in 1990 while employed at CERN near Geneva, Switzerland. The browser was released outside CERN to other research institutions starting in January 1991, and then to the general public in August 1991. The World Wide Web has been central to the development of the Information Age, and is the primary tool billions of people use to interact on the Internet.

## [WYSIWYG](https://en.wikipedia.org/wiki/WYSIWYG)
In computing, _What You See Is What You Get_ (_WYSIWYG_) is a system where editing software allows content to be edited in a form that resembles its appearance when printed or displayed as a finished product such as a printed document, web page, or slide presentation.

## [Z-Buffering](https://en.wikipedia.org/wiki/Z-buffering)
In computer graphics, _z-buffering_, also known as _depth buffering_, is the management of image depth coordinates in 3D graphics, usually done in hardware, sometimes in software. It is one solution to the visibility problem, which is the problem of deciding which elements of a rendered scene are visible, and which are hidden. Z-buffering was first described in 1974 by Wolfgang Straßer in Chapter 6 (page 6-1) of his PhD thesis. The [painter's algorithm](#painters-algorithm) is another common solution which, though less efficient, can also handle non-opaque scene elements. The z-buffer uses the Image space method for hidden surface detection. A z-buffer can refer to a data structure or to the method used to perform operations on that structure.

## [Z-Fighting](https://en.wikipedia.org/wiki/Z-fighting)
_Z-fighting_, also called _stitching_, is a phenomenon in 3D rendering that occurs when two or more primitives have very similar distances to the camera. This would cause them to have near-similar or identical values in the [z-buffer](#z-buffering), which keeps track of depth. This then means that when a specific pixel is being rendered, it is nearly random which one of the two primitives gets drawn in that pixel because the z-buffer cannot distinguish precisely which one is farther from the other. Traditionally, the farther pixel would be discarded. It is particularly prevalent with coplanar polygons, where two faces occupy essentially the same space, with neither in front. Affected pixels are rendered with fragments from one polygon or the other arbitrarily, in a manner determined by the precision of the z-buffer. It can also vary as the scene or camera is changed, causing one polygon to "win" the z test, then another, and so on. The overall effect is a flickering, noisy rasterization of two polygons which "fight" to color the screen pixels. This problem is usually caused by limited sub-pixel precision and [floating point](#floating-point) and fixed point round-off errors.

The more z-buffer precision one uses, the less likely it is that z-fighting will be encountered. But for coplanar polygons, the problem is inevitable unless corrective action is taken.

## [Zero-Day](https://en.wikipedia.org/wiki/Zero-day_(computing))
A _zero-day_ (also known as _0-day_) vulnerability is a computer-software vulnerability that is unknown to those who should be interested in mitigating the vulnerability (including the vendor of the target software). Until the vulnerability is mitigated, hackers can exploit it to adversely affect computer programs, data, additional computers or a [network](#computer-network). An [exploit](#exploit) directed at a zero-day is called a _zero-day exploit_, or _zero-day attack_.

The term "zero-day" originally referred to the number of days since a new piece of software was released to the public, so "zero-day" software was software that had been obtained by hacking into a developer's computer before release. Eventually the term was applied to the vulnerabilities that allowed this hacking, and to the number of days that the vendor has had to fix them. Once the vendor learns of the vulnerability, the vendor will usually create patches or advise workarounds to mitigate it.

The more recently that the vendor has become aware of the vulnerability, the more likely that no fix or mitigation has been developed. Even after a fix is developed, the fewer the days since then, the higher the probability that an attack against the afflicted software will be successful, because not every user of that software will have applied the fix. For zero-day exploits, unless the vulnerability is inadvertently fixed, e.g. by an unrelated update that happens to fix the vulnerability, the probability that a user has applied a vendor-supplied patch that fixes the problem is zero, so the exploit would remain available. Zero-day attacks are a severe threat.
