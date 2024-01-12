// [Scope 0: CYAN]
import * as readline from "node:readline/promises";
import {
  stdin as input,
  stdout as output,
} from "node:process";

/**
 * Basic Memoizer
 *  Simple memoizer with no memory optimizations or safeguards
 */

function memoize(fn) {
  const cache = {};

  return function memoizedFunction() {
    console.log(`  memoized ${fn.name}: ${[...arguments]}`); // Console statement added to track function calls

    const args = [...arguments];

    if (!cache.hasOwnProperty(args)) {
      cache[args] = fn(...args);
    }

    return cache[args];
  };
}

/**
 * Provided functions
 */

// Provided implementation of isPrime(..)
function isPrime(v) {
  console.log(`  isPrime: ${v}`); // Console statement added to track function calls
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

const isPrimeM = memoize(isPrime);

// Provided implementation of factorize(..)
function factorize(v) {
  console.log(`  factorize: ${v}`); // Console statement added to track function calls
  if (!isPrimeM(v)) { // Replace call to isPrime(..) with call to memoized function, add bugfix
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

const factorizeM = memoize(factorize);

/**
 * Prompt Utilities
 */

const promptUser = readline.createInterface({ input, output });

function runPrompt() {
  promptUser.question('Factorize which number: ')
    .then(function handlePrompt(resp) {
      const num = Number(resp);

      if (!Number.isInteger(num) || num < 2) {
        throw new Error('Input must be an integer greater than 1!');
      }

      const result = factorizeM(num);

      console.log(`\nResult: ${result}\n`);

      return runPrompt();
    });
}

/**
 * Start the command line
 */

runPrompt();
