// [Scope 0: CYAN]
import * as readline from "node:readline/promises";
import {
  stdin as input,
  stdout as output,
} from "node:process";

// Crypto script - accept input, spit out output, repeat
// Append code key to the start of the string (pos/neg offset flag, offset value (1-8))
//
// Encode:
// 1. Calculate a matrix for the length of the string value
// 2. Convert matrix to ascii values for characters
// 3. Choose random offset (-8 to 8)
// 4. Apply offset to ascii values in ascending/descending order starting with positive value
// 5. Flip columns to rows for matrix
// 6. Convert ascii values back to characters
// 7. Convert offset value flag and starting value to characters, prepend to string
// 8. Return the string
//
// Decode:
// 1. Remove encoded offset value flag and starting value from the start of string
// 2. Convert offset value flag character and starting value character to number
// 3. Calculate matrix for the length of the remaining string value
// 4. Convert matrix to ascii values for characters
// 5. Flip rows to columns for matrix
// 6. Apply offset to ascii values in reverse ascending/descending order starting with positive value
// 7. Convert ascii values back to characters
// 8. Return the string

/**
 * Matrix Utilities
 */

function getMatrixFor(str) {
  // [Scope 1: YELLOW]
  const sqrt = Math.sqrt(str.length);
  let rowCount = Math.floor(sqrt);
  let colCount = Math.ceil(sqrt);

  if (rowCount * colCount < str.length) {
    // [Scope 2: MAGENTA]
    rowCount += 1;
  }

  return [...new Array(rowCount)]
    .map(function createEmptyArrayLiteral(_, rowIdx) {
      // [Scope 2: MAGENTA]
      return [...new Array(colCount)]
        .map(function populateCols(_, colIdx) {
          // [Scope 3: BLACK]
          return str[(rowIdx * colCount) + colIdx] || ' ';
        });
    });
}

/**
 * Encrytion/Decryption Utilities
 */

function encodeMatrix(matrix) {
  // [Scope 1: YELLOW]
}

function encodeStr(str) {
  // [Scope 1: YELLOW]
  console.log(`Encoding "${str}"...`);
  console.log(getMatrixFor(str));
}

function decodeStr(str) {
  // [Scope 1: YELLOW]
  console.log(`Decoding "${str}"...`);
}

/**
 * Prompt Utilities
 */

const promptUser = readline.createInterface({ input, output });

function runPrompt() {
  // [Scope 1: YELLOW]
  promptUser.question('(e)ncode/(d)ecode/e(x)it: ')
    .then(function promptString(encodeOrDecode) {
      // [Scope 2: MAGENTA]
      const response = encodeOrDecode.trim().toLowerCase();
      const shouldEncode = response === 'encode' || response === 'e';
      const shouldDecode = response === 'decode' || response === 'd';
      const shouldExit = response === 'exit' || response === 'x';
      const isResponseValid = shouldEncode || shouldDecode;
      const operationString = shouldEncode ? 'encode' : 'decode';

      if (shouldExit) {
        // [Scope 3: BLACK]
        process.exit();
      }

      if (!isResponseValid) {
        // [Scope 3: BLACK]
        console.log('\nInvalid option!\n');
        return runPrompt();
      }

      return promptUser.question(`Text to ${operationString}: `)
        .then(function operateOnString(str) {
          // [Scope 3: BLACK]
          if (shouldEncode) {
            // [Scope 4: GREEN]
            encodeStr(str);
          } else {
            // [Scope 4: GREEN]
            decodeStr(str);
          }

          return runPrompt();
        });
    });
}

/**
 * Start the command line
 */

runPrompt();
