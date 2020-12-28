# Electronic Computing
[Video Link](https://youtu.be/LN0ucKNX0hc)

The [Harvard Mark I](https://en.wikipedia.org/wiki/Harvard_Mark_I) was one of the largest electro-mechanical computers ever built. It was completed in 1944 by IBM for the allies during WWII. It contained 765,000 components, 3,000,000 connections, and 500 miles of wire. One of the earliest uses for this technology was running simulations for the Manhattan Project. The brains of these machines were [relays](../glossary/README.md#relay).

Unfortunately, the mechanical arm inside of a relay has mass and cannot move instantly between open and closed states. A good relay in the 1940s might be able to flip back and forth 50 times in a second.

The Harvard Mark I could do 3 additions or substractions per second. It could perform multiplication in 6 seconds. Divisions took 15 seconds. More complex operations, such as triganometric functions could take over a minutes.

In addition to the slow switching speed, relays are also prone to wear and tear. The Harvard Mark I had roughly 3,500 relays. At that scale, the probability of failure of any given relay increases significantly. Due to the upkeep necessary on these massive machines, a need for faster, more reliable alternatives to relays was needed if computing was going to advance further. This alternative already existed:

In 1904, English physicist [John Ambrose Fleming](https://en.wikipedia.org/wiki/John_Ambrose_Fleming) developed an electrical component called a [thermionic valve](https://en.wikipedia.org/wiki/Vacuum_tube) - the first vacuum tube. This thermionic valve housed two electrodes inside an airtight glass bulb. When one of the electrodes was heated it would emit electrons in a process called thermionic emission. The other electrode, if positively charged, would then attract these electrons creating a current. If the electrode instead had a negative or neutral charge, the current would halt. This first vacuum tube, the simplest design, is called a [diode](../glossary/README.md#diode).

Shortly after 1906 American inventor Lee De Forest a third control electrode that sits between the two electrodes in Fleming's design creating the crude form of what would become the _triode_. The control electrode's charge now dictated if the current would flow or be blocked - the circuit can now be opened or closed by applying a charge to the control electrode.

Importantly: vacuum tubes have no moving parts leading to less wear and allowing for them to switch thousands of times per second. Triode vacuum tubes would become the basis for radio, long-distance telephones, and many other electronic devices for nearly half a century. Despite being a bit fragile, expensive, and prone to burning out like a light bulb, vacuum tubes were a big improvement over mechanical relays.

By the 1940s, the cost and reliability of vacuum tubes had improved to the point that they became feasible for use in computers. This marked the shift from electro-mechanical computing to electronic computing.

The first large scale use of vacuum tube computing was the [Colossus Mark I](https://en.wikipedia.org/wiki/Colossus_computer) designed by Engineer Tommy Flowers, completed in December of 1943. This machine was used to help decrypt Nazi communications. The first version of Colossus contained 1,600 vacuum tubes and 10 of these machines were built to assist in code breaking. The Colossus is considered the first programmable, electronic computer. Programming was done by plugging hundreds of wires into plugboards in order to set up the computer to perform the correct operations.

In 1946 the [Electronic Numerical Integrator And Calculator](https://en.wikipedia.org/wiki/ENIAC) (ENIAC) was completed at the University of Pensylvania. This computer was designed by John Mauchly and J. Presper Eckert it was the world's first truly general purpose, programmable, electronic computer. ENIAC could perform 5,000 10-digit additions and subtractions per second, a great many times faster than any machine that came before it. It was operational for ten years, and is estimated to have done more arithmetic than the human race up to that point.

However, with that many vacuum tubes, failures were common. ENIAC was generally only operational for about half-a-day at a time before breaking down. By the 1950s vacuum tube based computing was reaching its limits.

In 1946 at Bell Laboratories American physicists John Bardeen and Walter Brattain invented the [transistor]() while working under William Shockley. The first transistor created at Bell Labs showed tremendous promise: it could switch between on and off states 10,000 times per second, composed of solid material (known as a solid-state component). These transistors could be made smaller than the smallest relays or vacuum tubes which led to smaller and cheaper computers. The [IMB 608](https://en.wikipedia.org/wiki/IBM_608) released in 1957 was the first fully transistor-powered, commercially available computer. It contained 3,000 transistors and could perform 4,500 additions, or ~80 multiplcations/divisions, per second.

Today computers use transistors that are smaller than 50 nanometers in size (for reference: a sheet of paper is roughly 100,000 nanometers thick). They are incredibly fast, capable of switching states milliosn of times per second, and they can run for decades.

A lot of this transistor and semiconductor development happened in the Santa Clara Valley, between San Francisco and San Jose California. Since the most common material used to create semiconductors is silicon, this region became known as Silicon Valley. William Shockley moved to Silicon Valley to found Shockley Semiconductor. His employees later founded Fairchild Semiconductors. The employees of Fairchild Semiconductors later founded Intel: the world's largest computer chip maker today.

| [Previous: Early Computing](../01/README.md) | [Table of Contents](../README.md#table-of-contents) | [Next: Boolean Logic and Logic Gates](../03/README.md) |
| :------------------------------------------: | :-------------------------------------------------: | :----------------------------------------------------: |
