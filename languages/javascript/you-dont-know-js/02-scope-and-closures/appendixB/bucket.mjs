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
        process.exit();
      }

      if (!isResponseValid) {
        console.log('\nInvalid option!\n');
        return runPrompt();
      }

      return promptUser.question(`Text to ${operationString}: `)
        .then(function operateOnString(providedString) {
          // [Scope 3: BLACK]
          if (shouldEncode) {
            console.log(`Encoding "${providedString}"...`);
          } else {
            console.log(`Decoding "${providedString}"...`);
          }

          return runPrompt();
        });
    });
}

/**
 * Start the command line
 */

runPrompt();
