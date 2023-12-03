# Chapter 7: Using Closures
Closures are central to using scope effectively in the structure of a program. Closure builds on the approach that, for variables necessary for use over time, instead of placing them in outer scopes, these variables can be encapsulated in a narrower scope while preserving access to them from within functions. Functions maintain access to referenced scoped variables via closure.

Closure is one of the most important language characteristics ever invented in programming. It underlies major programming paradigms including Functional Programming (FP), modules, and even a bit of class-oriented design.

## Sections
* [See the Closure](#see-the-closure)
  * [Pointed Closure](#pointed-closure)
  * [Adding Up Closures](#adding-up-closures)
  * [Live Link, Not a Snapshot](#live-link-not-a-snapshot)

[◂ Return to Table of Contents](../README.md)

## See the Closure
Closure is originally a mathematical concept from labmda calculus. Closure is a behavior of functions and only functions: an object or a class cannot have closure. For closure to be observed, a function must be invoked, and specifically it must be invoked in a different branch of the scope chain from where it was originally defined.

The following example has each scope annotated with their relevant [scope bubble colors](../02/README.md#marbles-and-buckets-and-bubbles-oh-my):

```javascript
// outer/global scope: RED(1)

function lookupStudent(studentID) {
  // function scope: BLUE(2)

  var students = [
    { id: 14, name: "Kyle" },
    { id: 73, name: "Suzy" },
    { id: 112, name: "Frank" },
    { id: 6, name: "Sarah" }
  ];

  return function greetStudent(greeting){
    // function scope: GREEN(3)

    var student = students.find(
      student => student.id == studentID
    );

    return `${ greeting }, ${ student.name }!`;
  };
}

var chosenStudents = [
  lookupStudent(6),
  lookupStudent(112)
];

// accessing the function's name:
chosenStudents[0].name;
// greetStudent

chosenStudents[0]("Hello");
// Hello, Sarah!

chosenStudents[1]("Howdy");
// Howdy, Frank!
```

The `lookupStudent(..)` outer function creates and returns an inner function `greetStudent(..)`. `lookupStudent(..)` is called twice creating two separate instances of the inner `greetStudent(..)` function.

In academic terms, each instance of `greetStudent(..)` _closes_ over the variables in its outer scope: `students` and `studentID`. The references in `greetStudent(..)` to these variables from its outer scope is called a _closure_.

Closure allows `greetStudent(..)` to continue to access these outer variables even after the outer scope is finished (when each call to `lookupStudent(..)` completes). The variables `students` and `studentID` do not get garbage collected (GC'd) so long as a reference continues to exist to the inner function that references them.

#### Pointed Closure
The `student => student.id == studentID` arrow function creates another function scoep inside of the `greetStudent(..)` function scope.

An accurate color-diagram for this code would include a fourth scope bucket inside the arrow function:

```javascript
var student = students.find(
  student =>
    // function scope: ORANGE(4)
    student.id == studentID
);
```

The BLUE(2) `studentID` reference is actually held by the ORANGE(4) scope rather than the `greetStudent(..)` GREEN(3) scope. It is actually the arrow function passed to the array's `find(..)` method that holds closure over `studentID`, not `greetStudent(..)`.

#### Adding Up Closures
The following is a canonical example often cited for closure:

```javascript
function adder(num1) {
  return function addTo(num2){
    return num1 + num2;
  };
}

var add10To = adder(10);
var add42To = adder(42);

add10To(15);    // 25
add42To(9);     // 51
```

Each instance of the inner `addTo(..)` function is closing over its own `num1` variable.

Closure is associated with an instance of a function rather than its single lexical definition. Every time the `adder(..)` function runs, a _new_ inner `addTo(..)` function is created, and for each new instance a new closure. Each inner function instance (`add10To(..)` and `add42To(..)`) has its own closure over its own instance of the scope environment from that execution of `adder(..)`.

Even though closure is based on lexical scope (which is handled at compile time), closure is observed as a runtime characteristic of function instances.

#### Live Link, Not a Snapshot
It is a common misconception that closures are snapshots of a value at a given moment. Instead, a closure is a live link, preserving access to the full variable itself. By closing over a variable, that variable can be accessed (read and write) as long as the inner function reference exists within the program.

Taking another example:

```javascript
function makeCounter() {
  var count = 0;

  return function getCurrent() {
    count = count + 1;
    return count;
  };
}

var hits = makeCounter();

// later

hits();     // 1

// later

hits();     // 2
hits();     // 3
```

The variable `count` is closed over by the inner function `getCurrent()`, which prevents `count` from being GC'd after `makeCounter()` is done running. The `hits()` function call accesses and updates the `count` variable, incrementing it with each call.

Although the enclosing scope of a closure is typically a function, it doesn't necessarily have to be. There only needs to be an inner function present inside an outer scope:

```javascript
var hits;
{   // an outer scope (but not a function)
  let count = 0;
  hits = function getCurrent(){
    count = count + 1;
    return count;
  };
}
hits();     // 1
hits();     // 2
hits();     // 3
```

**NOTE**: In this example `getCurrent()` is defined as a function expression rather than a function declaration due to the quirks of [FiB](../06/README.md#function-declarations-in-blocks-fib).

It is common to mistake closure as value-oriented instead of variable-oriented:

```javascript
var studentName = "Frank";

var greeting = function hello() {
  // we are closing over `studentName`,
  // not "Frank"
  console.log(
    `Hello, ${ studentName }!`
  );
}

// later

studentName = "Suzy";

// later

greeting();
// Hello, Suzy!
```

In this example, whenever `greeting()` is invoked, the current value of `studentName` will be reflected.

The classic illustration of this mistake is defining functions inside of a loop:

```javascript
var keeps = [];

for (var i = 0; i < 3; i++) {
  keeps[i] = function keepI(){
    // closure over `i`
    return i;
  };
}

keeps[0]();   // 3 -- WHY!?
keeps[1]();   // 3
keeps[2]();   // 3
```

In this example each saved function returns `3` because they all close over the same `i` variable, which is assigned a value of `3` at the end of the loop iterations.

**NOTE**: This closure illustration typically uses a `setTimeout(..)` or other function that accepts a callback handler. This example was simplified by storing function references in an array, to avoid considering asychronous timing in the analysis. The closure principle is the same.

To preserve the value at each iteration instead, a different variable will need to be closed over in each iteration of the loop:

```javascript
var keeps = [];

for (var i = 0; i < 3; i++) {
  // new `j` created each iteration, which gets
  // a copy of the value of `i` at this moment
  let j = i;

  // the `i` here isn't being closed over, so
  // it's fine to immediately use its current
  // value in each loop iteration
  keeps[i] = function keepEachJ(){
    // close over `j`, not `i`!
    return j;
  };
}
keeps[0]();   // 0
keeps[1]();   // 1
keeps[2]();   // 2
```

Each iteration of the `for`-loop creates a new variable `j` which never gets reassigned.

[Recalling](../05/README.md#loops) that a `let` declaration for the iterator in a `for`-loop creates a new variable for each iteration of the loop:

```javascript
var keeps = [];

for (let i = 0; i < 3; i++) {
  // the `let i` gives us a new `i` for
  // each iteration, automatically!
  keeps[i] = function keepEachI(){
    return i;
  };
}
keeps[0]();   // 0
keeps[1]();   // 1
keeps[2]();   // 2
```

Using `let` to declare the iterator `i`, a new `i` is created for each iteration of the loop, so each closure works as expected.

[▲ Return to Sections](#sections)

| [Previous: Chapter 6 - Limiting Scope Exposure](../06/README.md) | [Table of Contents](../README.md#table-of-contents) |
