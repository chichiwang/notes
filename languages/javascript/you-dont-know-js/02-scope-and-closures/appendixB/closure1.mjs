// [Scope 0: CYAN]
import * as readline from "node:readline/promises";
import {
  stdin as input,
  stdout as output,
} from "node:process";

/**
 * Provided functions
 */

// Provided implementation of isPrime(..)
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

// Provided implementation of factorize(..)
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

/**
 * Prompt Utilities
 */

const promptUser = readline.createInterface({ input, output });

function runPrompt() {
  promptUser.question('Factorize which number: ')
    .then(function handlePrompt(resp) {
      return runPrompt();
    });
}

/**
 * Start the command line
 */

runPrompt();
