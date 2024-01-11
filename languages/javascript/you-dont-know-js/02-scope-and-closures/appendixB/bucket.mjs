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
// 3. Choose random offset (-45 to 45)
// 4. Apply offset to ascii values in ascending/descending order starting with the random offset
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
const ASCIILowerBound = 32;
const ASCIIUpperBound = 126;
const ASCIIRangeHalf = ASCIILowerBound + ((ASCIIUpperBound - ASCIIUpperBound)/2);
const minOffset = -45;
const maxOffset = 45;


function getRandomInt(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

function offsetASCIICode(code, offset) {
  let offsetCode = code + offset;

  if (offsetCode < ASCIILowerBound) {
    offsetCode = ASCIIUpperBound + 1 - (ASCIILowerBound - offsetCode);
  } else if (offsetCode > ASCIIUpperBound) {
    offsetCode = ASCIILowerBound - 1 + (offsetCode - ASCIIUpperBound);
  }

  return offsetCode;
}

function validateASCIIValue(val) {
  return val >= ASCIILowerBound && val <= ASCIIUpperBound;
}

function validateMatrix(matrix) {
  // [Scope 1: YELLOW]
  let colSize; // Shadowed identifier

  if (!Array.isArray(matrix) || !matrix.length) {
    // [Scope 2: MAGENTA]
    return false;
  }

  for (const row of matrix) {
    // [Scope 2: MAGENTA]
    if (!Array.isArray(row) || !row.length) {
      // [Scope 3: BLACK]
      return false;
    }

    colSize = colSize || row.length;

    if (row.length !== colSize) {
      // [Scope 3: BLACK]
      return false;
    }

    return true;
  }
}

function createMatrix(rowSize, colSize, matrixValFn) {
  // [Scope 1: YELLOW]
  return [...new Array(rowSize)]
    .map(function populateRows(_, rowIdx) {
      // [Scope 2: MAGENTA]
      return [...new Array(colSize)]
        .map(function populateCols(_, colIdx) {
          // [Scope 3: BLACK]
          return matrixValFn(rowIdx, colIdx);
        });
    });
}

function strToMatrix(str) {
  // [Scope 1: YELLOW]
  const sqrt = Math.sqrt(str.length);
  let rowSize = Math.floor(sqrt);
  let colSize = Math.ceil(sqrt);

  if (rowSize * colSize < str.length) {
    // [Scope 2: MAGENTA]
    rowSize += 1;
  }

  return createMatrix(rowSize, colSize, function applyValues(rowIdx, colIdx) {
    // [Scope 2: MAGENTA]
    return str[(rowIdx * colSize) + colIdx] || ' ';
  });
}

function invertMatrix(matrix) {
  // [Scope 1: YELLOW]
  let colSize, rowSize;

  if (!validateMatrix(matrix)) {
    // [Scope 2: MAGENTA]
    throw new Error('Invalid Matrix!');
  }

  rowSize = matrix.length;
  colSize = matrix[0].length;

  return createMatrix(colSize, rowSize, function invertValues(rowIdx, colIdx) {
    // [Scope 2: MAGENTA]
    return matrix[colIdx][rowIdx];
  });
}

function matrixCharToASCII(matrix) {
  // [Scope 1: YELLOW]

  if (!validateMatrix(matrix)) {
    // [Scope 2: MAGENTA]
    throw new Error('Invalid Matrix!');
  }

  const rowSize = matrix.length;
  const colSize = matrix[0].length;

  return createMatrix(rowSize, colSize, function charToASCII(rowIdx, colIdx) {
    // [Scope 2: MAGENTA]
    return matrix[rowIdx][colIdx].charCodeAt(0);
  });
}

function matrixASCIIToChar(matrix) {
  // [Scope 1: YELLOW]

  if (!validateMatrix(matrix)) {
    // [Scope 2: MAGENTA]
    throw new Error('Invalid Matrix!');
  }

  const rowSize = matrix.length;
  const colSize = matrix[0].length;

  return createMatrix(rowSize, colSize, function ASCIIToChar(rowIdx, colIdx) {
    // [Scope 2: MAGENTA]
    return String.fromCharCode(matrix[rowIdx][colIdx]);
  });
}

function encodeASCIIMatrix(matrix) {
  // [Scope 1: YELLOW]

  if (!validateMatrix(matrix)) {
    // [Scope 2: MAGENTA]
    throw new Error('Invalid Matrix!');
  }

  const rowSize = matrix.length;
  const colSize = matrix[0].length;

  const startOffset = getRandomInt(minOffset, maxOffset);
  const iterator = getRandomInt(1, 100) > 50 ? 1 : -1;

  const encodedStartOffset = maxOffset + startOffset + ASCIILowerBound;
  const encodedIterator = iterator < 0 ?
    getRandomInt(ASCIILowerBound, ASCIIRangeHalf -1) :
    getRandomInt(ASCIIRangeHalf + 1, ASCIIUpperBound);

  let offset = startOffset;

  const encodedMatrix = createMatrix(rowSize, colSize, function encodeASCIIValues(rowIdx, colIdx) {
    const offsetCode = offsetASCIICode(matrix[rowIdx][colIdx], offset);
    offset += iterator;
    if (offset < minOffset) {
      offset = maxOffset;
    } else if (offset > maxOffset) {
      offset = minOffset;
    }

    return offsetCode;
  });

  return [[encodedStartOffset, encodedIterator], encodedMatrix];
}

function decodeASCIIMatrix(matrix, encodedOffset, encodedIterator) {
  // [Scope 1: YELLOW]

  if (!validateMatrix(matrix)) {
    // [Scope 2: MAGENTA]
    throw new Error('Invalid Matrix!');
  }

  const rowSize = matrix.length;
  const colSize = matrix[0].length;

  let offset = (encodedOffset - maxOffset - ASCIILowerBound) * -1;
  const iterator = encodedIterator <= ASCIIRangeHalf ? 1 : -1;

  const decodedMatrix = createMatrix(rowSize, colSize, function decodeASCIIValues(rowIdx, colIdx) {
    const decodedVal = offsetASCIICode(matrix[rowIdx][colIdx], offset);
    offset += iterator;
    if (offset < minOffset) {
      offset = maxOffset;
    } else if (offset > maxOffset) {
      offset = minOffset;
    }

    return decodedVal;
  });

  return decodedMatrix;
}

function ASCIIToStr(val) {
  const isValidASCII = validateASCIIValue(val);

  if (!isValidASCII) {
    console.log(val);
    throw new Error('ASCII value out of range!');
  }

  return String.fromCharCode(val);
}

function ASCIIMatrixToStr(matrix) {
  // [Scope 1: YELLOW]

  if (!validateMatrix(matrix)) {
    // [Scope 2: MAGENTA]
    throw new Error('Invalid Matrix!');
  }

  return matrix
    .map(function rowsToStr(row) {
      return row.map(function colsToStr(ASCIIVal) {
        return ASCIIToStr(ASCIIVal);
      }).join('');
    }).join('');
}

/**
 * Encrytion/Decryption Utilities
 */

function encodeStr(str) {
  // [Scope 1: YELLOW]
  const ASCIIMatrix = matrixCharToASCII(strToMatrix(str));
  const [[encodedOffsetASCII, encodedIteratorASCII], encodedASCIIMatrix] = encodeASCIIMatrix(ASCIIMatrix);

  const encodedStr = `${ASCIIToStr(encodedOffsetASCII)}${ASCIIToStr(encodedIteratorASCII)} ${ASCIIMatrixToStr(encodedASCIIMatrix)}`;

  console.log(`\n${encodedStr}\n`);
}

function decodeStr(str) {
  // [Scope 1: YELLOW]
  const [encodedOffsetASCII, encodedIteratorASCII] = str
    .slice(0,2).split('')
    .map(function charToASCII(char) {
      return char.charCodeAt(0);
    });
  const encodedASCIIMatrix = matrixCharToASCII(strToMatrix(str.slice(3)));

  const decodedStr = ASCIIMatrixToStr(decodeASCIIMatrix(encodedASCIIMatrix, encodedOffsetASCII, encodedIteratorASCII));

  console.log(`\n${decodedStr}\n`);
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
