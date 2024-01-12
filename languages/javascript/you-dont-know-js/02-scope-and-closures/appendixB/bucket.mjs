// [Scope 0: CYAN]
import * as readline from "node:readline/promises";
import {
  stdin as input,
  stdout as output,
} from "node:process";

/**
 * Prompt Utilities
 */

const promptUser = readline.createInterface({ input, output });

function runPrompt() {
  // [Scope 1: YELLOW]
  promptUser.question('(e)ncode/(d)ecode/e(x)it: ')
    .then(function promptString(encodeOrDecode) { // Implied Scope: parameter and function name
      // [Scope 2: MAGENTA]
      const response = encodeOrDecode.trim().toLowerCase();
      const shouldEncode = response === 'encode' || response === 'e';
      const shouldDecode = response === 'decode' || response === 'd';
      const shouldExit = response === 'exit' || response === 'x';
      const isValid = shouldEncode || shouldDecode;
      const operationString = shouldEncode ? 'encode' : 'decode';

      if (shouldExit) {
        // [Scope 3: BLACK]
        process.exit();
      }

      if (!isValid) {
        // [Scope 3: BLACK]
        console.log('\nInvalid option!\n');
        return runPrompt();
      }

      /**
       * Matrix Encryption Utilities
       */
      const MatrixCrpyto = (function createMatrixSingleton() { // Implied Scope: function name
        // [Scope 3: BLACK]
        const ASCIILowerBound = 32;
        const ASCIIUpperBound = 126;
        const ASCIIRangeHalf = ASCIILowerBound + ((ASCIIUpperBound - ASCIIUpperBound)/2);
        const minOffset = -45;
        const maxOffset = 45;

        function getRandomInt(min, max) { // Implied Scope: parameter and function name
          // [Scope 4: GREEN]
          return Math.floor(Math.random() * (max - min + 1)) + min;
        }

        function offsetASCIIValue(ASCIIVal, offset) { // Implied Scope: parameter and function name
          // [Scope 4: GREEN]
          let offsetCode = ASCIIVal + offset;

          if (offsetCode < ASCIILowerBound) {
            // [Scope 5: PURPLE]
            offsetCode = ASCIIUpperBound + 1 - (ASCIILowerBound - offsetCode); // References variable declared 2+ scopes higher
          } else if (offsetCode > ASCIIUpperBound) {
            // [Scope 5: PURPLE]
            offsetCode = ASCIILowerBound - 1 + (offsetCode - ASCIIUpperBound); // References variable declared 2+ scopes higher
          }

          return offsetCode;
        }

        function validateASCIIValue(ASCIIVal) { // Implied Scope: parameter and function name
          // [Scope 4: GREEN]
          return ASCIIVal >= ASCIILowerBound && ASCIIVal <= ASCIIUpperBound;
        }

        function createMatrix(rowSize, colSize, matrixValFn) { // Implied Scope: parameter and function name
          // [Scope 4: GREEN]
          return [...new Array(rowSize)]
            .map(function populateRows(_, rowIdx) { // Implied Scope: parameter and function name
              // [Scope 5: PURPLE]
              return [...new Array(colSize)]
                .map(function populateCols(_, colIdx) { // Implied Scope: parameter and function name
                  // [Scope 6: ORANGE]
                  return matrixValFn(rowIdx, colIdx);
                });
            });
        }

        function strToMatrix(str) { // Implied Scope: parameter and function name
          // [Scope 4: GREEN]
          const sqrt = Math.sqrt(str.length);
          let rowSize = Math.floor(sqrt);
          let colSize = Math.ceil(sqrt);

          if (rowSize * colSize < str.length) {
            // [Scope 5: PURPLE]
            rowSize += 1;
          }

          return createMatrix(rowSize, colSize, function applyValues(rowIdx, colIdx) { // Implied Scope: parameter and function name
            // [Scope 5: PURPLE]
            return str[(rowIdx * colSize) + colIdx] || ' ';
          });
        }

        function invertMatrix(matrix) { // Implied Scope: parameter and function name
          // [Scope 4: GREEN]
          let rowSize = matrix.length;
          let colSize = matrix[0].length;

          return createMatrix(colSize, rowSize, function invertValues(rowIdx, colIdx) { // Implied Scope: parameter and function name
            // [Scope 5: PURPLE]
            return matrix[colIdx][rowIdx];
          });
        }

        function matrixCharToASCII(matrix) { // Implied Scope: parameter and function name
          // [Scope 4: GREEN]
          const rowSize = matrix.length;
          const colSize = matrix[0].length;

          return createMatrix(rowSize, colSize, function charToASCII(rowIdx, colIdx) { // Implied Scope: parameter and function name
            // [Scope 5: PURPLE]
            return matrix[rowIdx][colIdx].charCodeAt(0);
          });
        }

        function matrixASCIIToChar(matrix) { // Implied Scope: parameter and function name
          // [Scope 4: GREEN]
          const rowSize = matrix.length;
          const colSize = matrix[0].length;

          return createMatrix(rowSize, colSize, function ASCIIToChar(rowIdx, colIdx) { // Implied Scope: parameter and function name
            // [Scope 5: PURPLE]
            return String.fromCharCode(matrix[rowIdx][colIdx]);
          });
        }

        function encodeASCIIMatrix(matrix) { // Implied Scope: parameter and function name
          // [Scope 4: GREEN]
          const rowSize = matrix.length;
          const colSize = matrix[0].length;

          const startOffset = getRandomInt(minOffset, maxOffset);
          const iterator = getRandomInt(1, 100) > 50 ? 1 : -1;

          const encodedStartOffset = maxOffset + startOffset + ASCIILowerBound;
          const encodedIterator = iterator < 0 ?
            getRandomInt(ASCIILowerBound, ASCIIRangeHalf -1) :
            getRandomInt(ASCIIRangeHalf + 1, ASCIIUpperBound);

          let offset = startOffset;

          const encodedMatrix = createMatrix(rowSize, colSize, function encodeASCIIValues(rowIdx, colIdx) { // Implied Scope: parameter and function name
            // [Scope 5: PURPLE]
            const offsetCode = offsetASCIIValue(matrix[rowIdx][colIdx], offset);
            offset += iterator;
            if (offset < minOffset) { // References variable declared 2+ scopes higher
              // [Scope 6: ORANGE]
              offset = maxOffset;
            } else if (offset > maxOffset) { // References variable declared 2+ scopes higher
              // [Scope 6: ORANGE]
              offset = minOffset;
            }

            return offsetCode;
          });

          return [[encodedStartOffset, encodedIterator], encodedMatrix];
        }

        function decodeASCIIMatrix(matrix, encodedOffset, encodedIterator) { // Implied Scope: parameter and function name
          // [Scope 4: GREEN]
          const rowSize = matrix.length;
          const colSize = matrix[0].length;

          let offset = (encodedOffset - maxOffset - ASCIILowerBound) * -1;
          const iterator = encodedIterator <= ASCIIRangeHalf ? 1 : -1;

          const decodedMatrix = createMatrix(rowSize, colSize, function decodeASCIIValues(rowIdx, colIdx) { // Implied Scope: parameter and function name
            // [Scope 5: PURPLE]
            const decodedVal = offsetASCIIValue(matrix[rowIdx][colIdx], offset);
            offset += iterator;
            if (offset < minOffset) { // References variable declared 2+ scopes higher
              // [Scope 6: ORANGE]
              offset = maxOffset;
            } else if (offset > maxOffset) { // References variable declared 2+ scopes higher
              // [Scope 6: ORANGE]
              offset = minOffset;
            }

            return decodedVal;
          });

          return decodedMatrix;
        }

        function ASCIIToStr(val) { // Implied Scope: parameter and function name
          // [Scope 4: GREEN]
          const isValid = validateASCIIValue(val); // Mirrors isValid variable from Scope 2

          if (!isValid) {
            // [Scope 5: PURPLE]
            console.log(val);
            throw new Error('ASCII value out of range!');
          }

          return String.fromCharCode(val);
        }

        function ASCIIMatrixToStr(matrix) { // Implied Scope: parameter and function name
          // [Scope 4: GREEN]
          return matrix
            .map(function rowsToStr(row) { // Implied Scope: parameter and function name
              // [Scope 5: PURPLE]
              return row.map(function colsToStr(ASCIIVal) { // Implied Scope: parameter and function name
                // [Scope 6: ORANGE]
                return ASCIIToStr(ASCIIVal);
              }).join('');
            }).join('');
        }

        function encodeStr(str) { // Implied Scope: parameter and function name
          // [Scope 4: GREEN]
          const ASCIIMatrix = matrixCharToASCII(strToMatrix(str));
          const [[encodedOffsetASCII, encodedIteratorASCII], encodedASCIIMatrix] = encodeASCIIMatrix(ASCIIMatrix);

          const encodedStr = `${ASCIIToStr(encodedOffsetASCII)}${ASCIIToStr(encodedIteratorASCII)} ${ASCIIMatrixToStr(encodedASCIIMatrix)}`;

          console.log(`\n${encodedStr}\n`);
        }

        function decodeStr(str) { // Implied Scope: parameter and function name
          // [Scope 4: GREEN]
          const [encodedOffsetASCII, encodedIteratorASCII] = str
            .slice(0,2).split('')
            .map(function charToASCII(char) { // Implied Scope: parameter and function name
              // [Scope 5: PURPLE]
              return char.charCodeAt(0);
            });
          const encodedASCIIMatrix = matrixCharToASCII(strToMatrix(str.slice(3)));

          const decodedStr = ASCIIMatrixToStr(decodeASCIIMatrix(encodedASCIIMatrix, encodedOffsetASCII, encodedIteratorASCII)).trim();

          console.log(`\n${decodedStr}\n`);
        }

        return {
          encodeStr,
          decodeStr,
        };
      })();

      return promptUser.question(`Text to ${operationString}: `)
        .then(function operateOnString(str) { // Implied Scope: parameter and function name
          // [Scope 3: BLACK]
          if (shouldEncode) {
            // [Scope 4: GREEN]
            MatrixCrpyto.encodeStr(str);
          } else {
            // [Scope 4: GREEN]
            MatrixCrpyto.decodeStr(str);
          }

          return runPrompt();
        });
    });
}

/**
 * Start the command line
 */

runPrompt();
