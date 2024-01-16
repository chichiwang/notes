# Appendix B: Practice
This appendix aims to provide challenging and interesting exercises to test and solidify an understanding of the main topics from this book. These exercises do not have a specific correct answer - your approach may differ some (or a lot) from the solutions presented. The hope is for you to feel confident tackling these sorts of coding tasks on a strong foundation of knowledge.

## Sections
* [Buckets of Marbles](#buckets-of-marbles)
* [Closure (PART 1)](#closure-part-1)
  * [A Word About Memory](#a-word-about-memory)
* [Closure (PART 2)](#closure-part-2)
* [Closure (PART 3)](#closure-part-3)
* [Modules](#modules)

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

## Closure (PART 2)
In this exercise create a `toggle(..)` utility that returns a toggled value.

The toggle function will return another function that will alternate/rotate between the passed-in values when called:

```javascript
function toggle(/* .. */) {
  // ..
}

var hello = toggle("hello");
var onOff = toggle("on","off");
var speed = toggle("slow","medium","fast");

hello();      // "hello"
hello();      // "hello"

onOff();      // "on"
onOff();      // "off"
onOff();      // "on"

speed();      // "slow"
speed();      // "medium"
speed();      // "fast"
speed();      // "slow"
```

The corner case of passing no values to `toggle(..)` is not important - that toggler instance could always return `undefined`.

My solution for this exercise: [closure2.js](./closure2.js);

[▲ Return to Sections](#sections)

## Closure (PART 3)
In this exercise: Implement a `calculator()` function will produce an instance of a calculator function which maintains its own state:

```javascript
function calculator() {
  // ..
}

var calc = calculator();
```

Each time `calc(..)` is called it should be provided with a single character that represents a keypress of a calculator. Restrict the calculator to supporting digits (`0`-`9`), arithmetic operations (`+`, `-`, `*` , `/`), and `=` to compute the operation. Operations are processed in the order they are entered and there is no `()` grouping or operator precedence.

Do not support decimals, but the `/` operation can result in them. Do not support negative numbers, but the `-` operation can result in them. If an operation results in a decimal or negative value, operations can continue to operate on those values.

The return value of calling `calc(..)` should mimic what would be shown on an actual calculator: reflecting what was just pressed, or computing the total when pressing `=`.

```javascript
calc("4");     // 4
calc("+");     // +
calc("7");     // 7
calc("3");     // 3
calc("-");     // -
calc("2");     // 2
calc("=");     // 75
calc("*");     // *
calc("4");     // 4
calc("=");     // 300
calc("5");     // 5
calc("-");     // -
calc("5");     // 5
calc("=");     // 0
```

The usage above is clunky, so use a `useCalc(..)` helper, that runs the calculator one character at a time from a string, and computes the display each time:

```javascript
function useCalc(calc, keys) {
  return [...keys].reduce(
    function showDisplay(display,key){
      var ret = String( calc(key) );
      return (
        display +
        (
          (ret != "" && key == "=") ?
            "=" :
            ""
        ) +
        ret
      );
    },
    ""
  );
}

useCalc(calc,"4+3=");           // 4+3=7
useCalc(calc,"+9=");            // +9=16
useCalc(calc,"*8=");            // *5=128
useCalc(calc,"7*2*3=");         // 7*2*3=42
useCalc(calc,"1/0=");           // 1/0=ERR
useCalc(calc,"+3=");            // +3=ERR
useCalc(calc,"51=");            // 51
```

The most sensible usage of `useCalc(..)` helper is to always have `=` be the last character entered.

Some of the totals displayed by the calculator requires special handling - use the following `formatTotal(..)` function, which the calculator should use whenever it's going to return a current computed total:

```javascript
function formatTotal(display) {
  if (Number.isFinite(display)) {
    // constrain display to max 11 chars
    let maxDigits = 11;
    // reserve space for "e+" notation?
    if (Math.abs(display) > 99999999999) {
      maxDigits -= 6;
    }
    // reserve space for "-"?
    if (display < 0) {
      maxDigits--;
    }

    // whole number?
    if (Number.isInteger(display)) {
      display = display
        .toPrecision(maxDigits)
        .replace(/\.0+$/,"");
    }
    // decimal
    else {
      // reserve space for "."
      maxDigits--;
      // reserve space for leading "0"?
      if (
        Math.abs(display) >= 0 &&
        Math.abs(display) < 1
      ) {
        maxDigits--;
      }
      display = display
        .toPrecision(maxDigits)
        .replace(/0+$/,"");
    }
  }
  else {
    display = "ERR";
  }

  return display;
}
```

`formatTotal(..)` mostly handles limiting the calculator display to 11 characters max, even if negatives, repeating decimals, or even the `e+` exponential notation.

My _over-engineered_ solution for this exercise: [closure3.js](./closure3.js);

[▲ Return to Sections](#sections)

## Modules
This exercise is to convert the calculator from [Closure (PART 3)](#closure-part-3) into a module.

Instead of calling a single function `calc(..)`, specific methods will be called on the public API of an instance. The output will remain the same.

`calculator()` will be a classic module factory (not IIFE), so that multiple calculators can be created.

The public API will consist of:
* `number(..)` (the character/number pressed)
* `plus()`
* `minus()`
* `mult()`
* `div()`
* `eq()`

Usage will look like:

```javascript
var calc = calculator();

calc.number("4");     // 4
calc.plus();          // +
calc.number("7");     // 7
calc.number("3");     // 3
calc.minus();         // -
calc.number("2");     // 2
calc.eq();            // 75
```

`formatTotal(..)` will remain the same from the previous exercise, but `useCalc(..)` will be adjusted to work with the module API:

```javascript
function useCalc(calc,keys) {
  var keyMappings = {
    "+": "plus",
    "-": "minus",
    "*": "mult",
    "/": "div",
    "=": "eq"
  };

  return [...keys].reduce(
    function showDisplay(display,key){
      var fn = keyMappings[key] || "number";
      var ret = String( calc[fn](key) );
      return (
        display +
        (
          (ret != "" && key == "=") ?
            "=" :
            ""
        ) +
        ret
      );
    },
    ""
  );
}

useCalc(calc,"4+3=");           // 4+3=7
useCalc(calc,"+9=");            // +9=16
useCalc(calc,"*8=");            // *5=128
useCalc(calc,"7*2*3=");         // 7*2*3=42
useCalc(calc,"1/0=");           // 1/0=ERR
useCalc(calc,"+3=");            // +3=ERR
useCalc(calc,"51=");            // 51
```

Consider the pros/cons of representing the calculator as a module as opposed to the closure-function approach from the previous exercise.

Bonus: write out a few sentences about these considerations.

Bonus #2: Convert the module to other module formats: UMD, CommonJS, and ESM (ES Modules)

[▲ Return to Sections](#sections)

| [Previous: Appendix A - Exploring Further](../appendixA/README.md) | [Table of Contents](../README.md#table-of-contents) |
