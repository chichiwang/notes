function range(start,end) {
  function createRange(finalValue) {
    const firstValue = start;

    if (firstValue > finalValue) {
      return [];
    }

    return [...Array(finalValue - firstValue + 1)]
      .map(function assignValues(_, idx) {
        return firstValue + idx;
      });
  }

  return end === undefined ? createRange : createRange(end);
}

console.log(range(3,3));    // [3]
console.log(range(3,8));    // [3,4,5,6,7,8]
console.log(range(3,0));    // []

var start3 = range(3);
var start4 = range(4);

console.log(start3(3));     // [3]
console.log(start3(8));     // [3,4,5,6,7,8]
console.log(start3(0));     // []

console.log(start4(6));     // [4,5,6]
