# Appendix B: Practice
This appendix aims to provide challenging and interesting exercises to test and solidify an understanding of the main topics from this book. These exercises do not have a specific correct answer - your approach may differ some (or a lot) from the solutions presented. The hope is for you to feel confident tackling these sorts of coding tasks on a strong foundation of knowledge.

## Sections
* [Buckets of Marbles](#buckets-of-marbles)
* [Closure (PART 1)](#closure-part-1)
  * [A Word About Memory](#a-word-about-memory)

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

## Closure (PART 1)
A common computer-math operation is determining if a value is a prime number (has no divisors other than 1 and itself), and generating a list of prime factors (divisors):

```javascript
isPrime(11);        // true
isPrime(12);        // false

factorize(11);      // [ 11 ]
factorize(12);      // [ 3, 2, 2 ] --> 3*2*2=12
```

Below is an implementation of `isPrime(..)` adapted [from the Math.js library](https://github.com/josdejong/mathjs/blob/develop/src/function/utils/isPrime.js):

```javascript
function isPrime(v) {
  if (v <= 3) {
    return v > 1;
  }
  if (v % 2 == 0 || v % 3 == 0) {
    return false;
  }
  var vSqrt = Math.sqrt(v);
  for (let i = 5; i <= vSqrt; i += 6) {
    if (v % i == 0 || v % (i + 2) == 0) {
      return false;
    }
  }
  return true;
}
```

Below is a somewhat basic implemenation of `factorize(..)`:

```javascript
function factorize(v) {
  if (!isPrime(v)) {
    let i = Math.floor(Math.sqrt(v));
    while (v % i != 0) {
      i--;
    }
    return [
      ...factorize(i),
      ...factorize(v / i)
    ];
  }
  return [v];
}
```

**NOTE**: The author calls this implementation _basic_ because it is not performance optimized. It is binary-recursive (which is not tail-call optimizable), and it creates a lot of intermediate array copies. It also does not order the discovered factors in any way.

If `isPrime(4372)` were called multiple times in a program, the above implementation would go through dozens of comparisons/computation steps every time. `factorize(..)` calls `isPrime(..)` many times as it computes the list of factors. There's a good chance most of those calls are repeats.

**Requirements of this exercise**:
* Use closure to implement a cache to remember the the results of `isPrime(..)` so that the primality (`true`/`false`) of a given value is only ever computed once.
* Use a closure cache over `factorize(..)` as well to reduce wasted computations when calling `factorize(..)` on previously provided numbers.
* Use separate closures for caching `isPrime(..)` and `factorize(..)` rather than placing them in the same scope.

My solution for this exercise: [closure1.mjs](./closure1.mjs);

#### A Word About Memory
In saving repeated calls the computation speed is improved (in some cases, by a drastic amount). However, this usage of closure is making an explicit trade-off between computational performance and memory usage. This strategy essentially grows the cache (in memory) unboundedly. This can be a good trade-off if a repetition of common inputs is likely.

If, however, the likelihood is that every call will provide unique inputs, then the cache is an inappropriate technique to employ.

A more sophisticated caching strategy is an LRU (last recently used) cache. This cache limits the cache size and drops values that are least recently used. An LRU is non-trivial in its own right - it is a good idea to use a highly optimized implementation of LRU, and be aware of all the trade-offs at play.

[▲ Return to Sections](#sections)

| [Previous: Appendix A - Exploring Further](../appendixA/README.md) | [Table of Contents](../README.md#table-of-contents) |
