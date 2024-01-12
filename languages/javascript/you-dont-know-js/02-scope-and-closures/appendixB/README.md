# Appendix B: Practice
This appendix aims to provide challenging and interesting exercises to test and solidify an understanding of the main topics from this book. These exercises do not have a specific correct answer - your approach may differ some (or a lot) from the solutions presented. The hope is for you to feel confident tackling these sorts of coding tasks on a strong foundation of knowledge.

## Sections
* [Buckets of Marbles](#buckets-of-marbles)

[◂ Return to Table of Contents](../README.md)

## Buckets of Marbles
Write a program, any program, that satisfies these constraints:
* If you color all of the scopes (including the global scope) different colors, you would need at least 6 colors.
  * Add code comments labeling each scope with its own color
  * Bonus: Identify any implied scopes the code may have
* Each scope has at least one identifier
* Contains at least two function scopes and two block scopes
* At least one variable from an outer scope must be shadowed by a nested scope variable
* At least one variable reference must resolve to a variable declaration at least two levels higher in the scope chain

**TIP**: You can write junk foo/bar/baz code for this exercise, but I suggest you try to come up with some sort of non-trivial code that at least does something reasonable.

My _over-engineered_ solution for this exercise: [bucket.mjs](./bucket.mjs);

[▲ Return to Sections](#sections)

| [Previous: Appendix A - Exploring Further](../appendixA/README.md) | [Table of Contents](../README.md#table-of-contents) |
