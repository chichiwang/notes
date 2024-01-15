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
}

var calc = calculator();

/**
 * Provided useCalc(..) function
 *   Helper function to accept input as string and feed to calc(..) one
 *    input at a time.
 */
function useCalc(calc, keys) {
  return [...keys].reduce(
    function showDisplay(display,key){
      var ret = String( calc(key) );
      return (
        display +
        (
          (ret != '' && key == '=') ?
            '=' :
            ''
        ) +
        ret
      );
    },
    ''
  );
}

/**
 * Expected results for calls to useCalc(..)
 */
console.log(useCalc(calc,'4+3='));           // 4+3=7
console.log(useCalc(calc,'+9='));            // +9=16
console.log(useCalc(calc,'*8='));            // *5=128
console.log(useCalc(calc,'7*2*3='));         // 7*2*3=42
console.log(useCalc(calc,'1/0='));           // 1/0=ERR
console.log(useCalc(calc,'+3='));            // +3=ERR
console.log(useCalc(calc,'51='));            // 51
