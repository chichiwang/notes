# Appendix B: Practice
This appendix aims to provide challenging and interesting exercises to test and solidify an understanding of the main topics from this book. These exercises do not have a specific correct answer - your approach may differ some (or a lot) from the solutions presented. The hope is for you to feel confident tackling these sorts of coding tasks on a strong foundation of knowledge.

## Sections
* [Buckets of Marbles](#buckets-of-marbles)
* [Closure (PART 1)](#closure-part-1)
  * [A Word About Memory](#a-word-about-memory)
* [Closure (PART 2)](#closure-part-2)
* [Closure (PART 3)](#closure-part-3)
* [Modules](#modules)
* [Suggested Solutions](#suggested-solutions)
  * [Suggested: Bucket of Marbles](#suggested-bucket-of-marbles)
  * [Suggested: Closure (PART 1)](#suggested-closure-part-1)
  * [Suggested: Closure (PART 2)](#suggested-closure-part-2)
  * [Suggested: Closure (PART 3)](#suggested-closure-part-3)
  * [Suggested: Modules](#suggested-modules)

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

My classic module solution for this exercise: [modulesClassic.js](./modulesClassic.js);

[▲ Return to Sections](#sections)

## Suggested Solutions
Each suggested solution to the above exercises is just one approach to the problem. They are not _the right answer_ - they illustrate a reasonable approach to each exercise.

The most important benefit from reading these suggested solutions is to compare with your own code and analyze why similar/different approaches were chosen. Focus on the main topic rather than small details.

#### Suggested: Bucket of Marbles
The [Bucket of Marbles](#bucket-of-marbles) exercise can be solved thusly:

```javascript
// RED(1)
const howMany = 100;

// Sieve of Eratosthenes
function findPrimes(howMany) {
  // BLUE(2)
  var sieve = Array(howMany).fill(true);
  var max = Math.sqrt(howMany);

  for (let i = 2; i < max; i++) {
    // GREEN(3)
    if (sieve[i]) {
      // ORANGE(4)
      let j = Math.pow(i,2);
      for (let k = j; k < howMany; k += i) {
        // PURPLE(5)
        sieve[k] = false;
      }
    }
  }

  return sieve
  .map(function getPrime(flag,prime){
    // PINK(6)
    if (flag) return prime;
    return flag;
  })
  .filter(function onlyPrimes(v){
    // YELLOW(7)
    return !!v;
  })
  .slice(1);
}

findPrimes(howMany);
// [
//    2, 3, 5, 7, 11, 13, 17,
//    19, 23, 29, 31, 37, 41,
//    43, 47, 53, 59, 61, 67,
//    71, 73, 79, 83, 89, 97
// ]
```

**NOTE**: Looking at this solution, I am realizing that, in [my solution](./bucket.mjs), I misunderstood the scope-coloring requirement and colored different scopes at the same scope-level (depth) the same color. This made the exercise more difficult, but I did have fun with it so that is fine.

#### Suggested: Closure (PART 1)
[Closure (PART 1)](#closure-part-1) can be solved like this:

```javascript
var isPrime = (function isPrime(v){
  var primes = {};

  return function isPrime(v) {
    if (v in primes) {
      return primes[v];
    }
    if (v <= 3) {
      return (primes[v] = v > 1);
    }
    if (v % 2 == 0 || v % 3 == 0) {
      return (primes[v] = false);
    }
    let vSqrt = Math.sqrt(v);
    for (let i = 5; i <= vSqrt; i += 6) {
      if (v % i == 0 || v % (i + 2) == 0) {
        return (primes[v] = false);
      }
    }
    return (primes[v] = true);
  };
})();

var factorize = (function factorize(v){
  var factors = {};

  return function findFactors(v) {
    if (v in factors) {
      return factors[v];
    }
    if (!isPrime(v)) {
      let i = Math.floor(Math.sqrt(v));
      while (v % i != 0) {
        i--;
      }
      return (factors[v] = [
        ...findFactors(i),
        ...findFactors(v / i)
      ]);
    }
    return (factors[v] = [v]);
  };
})();
```

The author's approach to each utility:
1. Wrap each function in an IIFE to define the scope for the cache variable to reside in.
2. In the underlying call, first check the cache and, if the result is already known, return the result.
3. Assign to the cache the result of the operation, then return the results of the assignment operation (for brevity).

**NOTE**: In [my solution to this exercise](./closure1.mjs) I created a utility function called `memoize(..)` that would do essentially the above for any function passed into it, returning a wrapper function that would cache the inputs/results of calling the original function. [Memoization is a common technique](https://www.geeksforgeeks.org/javascript-memoization/) to trade memory usage for computational efficiency.

#### Suggested: Closure (PART 2)
The exercise for [Closure (PART 2)](#closure-part-2) can be solved in this way:

```javascript
function toggle(...vals) {
  var unset = {};
  var cur = unset;

  return function next(){
    // save previous value back at
    // the end of the list
    if (cur != unset) {
      vals.push(cur);
    }
    cur = vals.shift();
    return cur;
  };
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

**NOTE**: In [my solution to this exercise](./closure2.js) I used a similar strategy, but I opted to track the current index of the value being returned from the original list and update that on each call instead. This was to avoid mutating the passed-in array, a habit from my experience that avoids unnecessary confusion as to the state/value of the parameter at any given time during program execution. The index value at any given time also clearly points to the current toggled value.

#### Suggested: Closure (PART 3)
The [Closure (PART 3)](#closure-part-3) exercise can be solved in the following manner:

```javascript
// from earlier:
//
// function useCalc(..) { .. }
// function formatTotal(..) { .. }

function calculator() {
  var currentTotal = 0;
  var currentVal = "";
  var currentOper = "=";

  return pressKey;

  // ********************

  function pressKey(key){
    // number key?
    if (/\d/.test(key)) {
      currentVal += key;
      return key;
    }
    // operator key?
    else if (/[+*/-]/.test(key)) {
      // multiple operations in a series?
      if (
        currentOper != "=" &&
        currentVal != ""
      ) {
        // implied '=' keypress
        pressKey("=");
      }
      else if (currentVal != "") {
        currentTotal = Number(currentVal);
      }
      currentOper = key;
      currentVal = "";
      return key;
    }
    // = key?
    else if (
      key == "=" &&
      currentOper != "="
    ) {
      currentTotal = op(
        currentTotal,
        currentOper,
        Number(currentVal)
      );
      currentOper = "=";
      currentVal = "";
      return formatTotal(currentTotal);
    }
    return "";
  };

  function op(val1,oper,val2) {
    var ops = {
      // NOTE: using arrow functions
      // only for brevity in the book
      "+": (v1,v2) => v1 + v2,
      "-": (v1,v2) => v1 - v2,
      "*": (v1,v2) => v1 * v2,
      "/": (v1,v2) => v1 / v2
    };
    return ops[oper](val1,val2);
  }
}

var calc = calculator();

useCalc(calc,"4+3=");           // 4+3=7
useCalc(calc,"+9=");            // +9=16
useCalc(calc,"*8=");            // *5=128
useCalc(calc,"7*2*3=");         // 7*2*3=42
useCalc(calc,"1/0=");           // 1/0=ERR
useCalc(calc,"+3=");            // +3=ERR
useCalc(calc,"51=");            // 51
```

**NOTE**: Remember this exercise is about closure, do not focus too much on the actual mechanics of the calculator, but more on if the calculator state is preserved across function calls.

**NOTE**: In [my over-engineered solution to this exercise](./closure3.js) I definitely focused far more on the mechanics of the calculator than the author did. One major difference between my implementation and his is that I did not take a subsequent operator in a sequence as an implied `=` keypress, and therefore did not keep a running total until an explicit `=` keypress was made. The `useCalc(..)` helper wrapping the `calc(..)` instance masks this and it doesn't appear to matter to the output to the end-user which approach was taken.

#### Suggested: Modules
The exercise for [Modules](#modules) can be solved in the following way:

```javascript
// from earlier:
//
// function useCalc(..) { .. }
// function formatTotal(..) { .. }

function calculator() {
  var currentTotal = 0;
  var currentVal = "";
  var currentOper = "=";

  var publicAPI = {
    number,
    eq,
    plus() { return operator("+"); },
    minus() { return operator("-"); },
    mult() { return operator("*"); },
    div() { return operator("/"); }
  };

  return publicAPI;

  // ********************

  function number(key) {
    // number key?
    if (/\d/.test(key)) {
      currentVal += key;
      return key;
    }
  }

  function eq() {
    // = key?
    if (currentOper != "=") {
      currentTotal = op(
        currentTotal,
        currentOper,
        Number(currentVal)
      );
      currentOper = "=";
      currentVal = "";
      return formatTotal(currentTotal);
    }
    return "";
  }

  function operator(key) {
    // multiple operations in a series?
    if (
      currentOper != "=" &&
      currentVal != ""
    ) {
      // implied '=' keypress
      eq();
    }
    else if (currentVal != "") {
      currentTotal = Number(currentVal);
    }
    currentOper = key;
    currentVal = "";
    return key;
  }

  function op(val1,oper,val2) {
    var ops = {
      // NOTE: using arrow functions
      // only for brevity in the book
      "+": (v1,v2) => v1 + v2,
      "-": (v1,v2) => v1 - v2,
      "*": (v1,v2) => v1 * v2,
      "/": (v1,v2) => v1 / v2
    };
    return ops[oper](val1,val2);
  }
}

var calc = calculator();

useCalc(calc,"4+3=");           // 4+3=7
useCalc(calc,"+9=");            // +9=16
useCalc(calc,"*8=");            // *5=128
useCalc(calc,"7*2*3=");         // 7*2*3=42
useCalc(calc,"1/0=");           // 1/0=ERR
useCalc(calc,"+3=");            // +3=ERR
useCalc(calc,"51=");            // 51
```

**NOTE**: The author broke up his logic into separate methods, making his implementation an improvement (in legibility and maintainability from the previous). I [took the lazy route](./modulesClassic.js) and did not do that, simply wrapping the entire API around the original `calculatorInstance(..)` function. It would be better to refactor the logic to make the code cleaner, this more easily read and maintained.

[▲ Return to Sections](#sections)

| [Previous: Appendix A - Exploring Further](../appendixA/README.md) | [Table of Contents](../README.md#table-of-contents) |
