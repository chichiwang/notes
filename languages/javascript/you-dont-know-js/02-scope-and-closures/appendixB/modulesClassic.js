/**
 * Provided formatTotal(..) function
 *   Call from calc(..) to format results after `=` is pressed
 */
function formatTotal(display) {
  if (Number.isFinite(display)) {
    // constrain display to max 11 chars
    let maxDigits = 11;
    // reserve space for 'e+' notation?
    if (Math.abs(display) > 99999999999) {
      maxDigits -= 6;
    }
    // reserve space for '-'?
    if (display < 0) {
      maxDigits--;
    }

    // whole number?
    if (Number.isInteger(display)) {
      display = display
        .toPrecision(maxDigits)
        .replace(/\.0+$/,'');
    }
    // decimal
    else {
      // reserve space for '.'
      maxDigits--;
      // reserve space for leading '0'?
      if (
        Math.abs(display) >= 0 &&
        Math.abs(display) < 1
      ) {
        maxDigits--;
      }
      display = display
        .toPrecision(maxDigits)
        .replace(/0+$/,'');
    }
  }
  else {
    display = 'ERR';
  }

  return display;
}

/**
 * Supply implemenation for calculator(..)
 *   Use closure to return a calculator function that will provide
 *    calculations according to the requirements:
 *    * Accept single character inputs 0-9, +, -, *, /, and =
 *    * Return the valid inputs until = is encountered, then calculate and return result
 *    * Do not respect operational precedence, do not accept negative or decimal values
 */
function calculator() {

  /**
   * Helper functions
   */
  const digitRegExpStr = '\\d';
  const mathRegExpStr = '[\\+\\-\\*\\/]';
  const calculateRegExpStr = '=';

  function isNumber(char) {
    const numberRegExp = new RegExp(`^${digitRegExpStr}$`);
    return numberRegExp.test(char);
  }

  function isMathOperator(char) {
    const mathRegExp = new RegExp(`^${mathRegExpStr}$`);
    return mathRegExp.test(char);
  }

  function isCalculateOperator(char) {
    const calculateRegExp = new RegExp(`^${calculateRegExpStr}$`);
    return /^=$/.test(char);
  }

  function calculate(num1, num2, operatorStr) {
    if (operatorStr === '+') {
      return num1 + num2;
    } else if (operatorStr === '-') {
      return num1 - num2;
    } else if (operatorStr === '*') {
      return num1 * num2;
    } else if (operatorStr === '/') {
      return num1 / num2;
    }

    return false;
  }

  function extractFirstNumber(str) {
    const endIdx = str.search(/[^\d]/);

    if (endIdx === -1) {
      return [str, ''];
    }

    return [str.substring(0, endIdx), str.substring(endIdx)];
  }
  const errValue = 'ERR';

  const defaultVal = '';
  let lastResult = defaultVal;
  let operation = defaultVal;

  function errResponse() {
    lastResult = defaultVal;
    operation = defaultVal;
    return formatTotal(errValue);
  }

  function calculatorInstance(keyPress) {
    const isKeyNumber = isNumber(keyPress);
    const isKeyMath = isMathOperator(keyPress);
    const isKeyCalculate = isCalculateOperator(keyPress);
    const isValidInput = isKeyNumber || isKeyMath || isKeyCalculate;

    if (!isValidInput) {
      return errResponse();
    } else if (!isKeyCalculate) {
      operation = operation + keyPress;
      return keyPress;
    } else {
      const chainedOperatorsRegExp = new RegExp(`${mathRegExpStr}{2,}`);
      const onlyDigitsRegExp = new RegExp(`^${digitRegExpStr}+$`);

      const isFirstOperation = lastResult === defaultVal;
      const isOperationEmpty = operation === defaultVal;
      const startsWithCalculateOperator = !isOperationEmpty && isCalculateOperator(operation[0]);
      const startsWithMathOperator = !isOperationEmpty && isMathOperator(operation[0]);
      const endsWithMathOperator = !isOperationEmpty && isMathOperator(operation[operation.length -1]);
      const startsWithNumber = !isOperationEmpty && isNumber(operation[0]);

      const hasChainedOperators = chainedOperatorsRegExp.test(operation);
      const hasOnlyDigits = onlyDigitsRegExp.test(operation);

      const isInvalidOperation =
        (isFirstOperation && startsWithCalculateOperator) ||
        (isFirstOperation && startsWithMathOperator) ||
        endsWithMathOperator ||
        hasChainedOperators ||
        isOperationEmpty;

      if (isInvalidOperation) {
        return errResponse();
      } else {
        if (isFirstOperation || startsWithNumber) {
          const [newResult, newOperation] = extractFirstNumber(operation);
          lastResult = Number(newResult);
          operation = newOperation;
        }

        while (operation.length > 0) {
          const operator = operation[0];
          operation = operation.substring(1);
          const [nextNumber, newOperation] = extractFirstNumber(operation);
          operation = newOperation;

          lastResult = calculate(lastResult, Number(nextNumber), operator);
        }
      }

      return formatTotal(lastResult);
    }
  }

  function number(val) {
    return calculatorInstance(val);
  }

  function plus() {
    return calculatorInstance('+');
  }

  function minus() {
    return calculatorInstance('-');
  }

  function mult() {
    return calculatorInstance('*');
  }

  function div() {
    return calculatorInstance('/');
  }

  function eq() {
    return calculatorInstance('=');
  }

  const publicAPI = {
    number,
    plus,
    minus,
    mult,
    div,
    eq,
  };

  return publicAPI;
}

var calc = calculator();

/*
  Benefits of module factory model over the closure-function approach:
    * Explicit API
    * Easier to test (test individual methods over all functionality on a common function)
    * Easier to refactor, maintain, break down into smaller functions

  NOTE: I did not refactor this as much as I could have. This is me being lazy.
    This factory could be much more heavily streamlined.
*/

/**
 * Provided useCalc(..) function
 *   Helper function to accept input as string and feed to calc(..) one
 *    input at a time.
 */
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

/**
 * Expected results for calls to useCalc(..)
 *  Provided in the exercise
 */
console.log(useCalc(calc,'4+3='));           // 4+3=7
console.log(useCalc(calc,'+9='));            // +9=16
console.log(useCalc(calc,'*8='));            // *5=128
console.log(useCalc(calc,'7*2*3='));         // 7*2*3=42
console.log(useCalc(calc,'1/0='));           // 1/0=ERR
console.log(useCalc(calc,'+3='));            // +3=ERR
console.log(useCalc(calc,'51='));            // 51
