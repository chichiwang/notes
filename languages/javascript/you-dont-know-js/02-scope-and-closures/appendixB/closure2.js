function toggle(...values) {
  console.log(values);
  let idx = 0;

  return function toggler() {
    const currVal = values[idx];
    idx = idx == values.length - 1 ? 0 : idx + 1;
    return currVal;
  }
}

var hello = toggle("hello");
var onOff = toggle("on","off");
var speed = toggle("slow","medium","fast");
var empty = toggle();

console.log(hello());      // "hello"
console.log(hello());      // "hello"

console.log(onOff());      // "on"
console.log(onOff());      // "off"
console.log(onOff());      // "on"
console.log(onOff());      // "off"

console.log(speed());      // "slow"
console.log(speed());      // "medium"
console.log(speed());      // "fast"
console.log(speed());      // "slow"
console.log(speed());      // "medium"

console.log(empty());      // undefined
console.log(empty());      // undefined
console.log(empty());      // undefined
