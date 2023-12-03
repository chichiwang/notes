# Chapter 7: Using Closures
Closures are central to using scope effectively in the structure of a program. Closure builds on the approach that, for variables necessary for use over time, instead of placing them in outer scopes, these variables can be encapsulated in a narrower scope while preserving access to them from within functions. Functions maintain access to referenced scoped variables via closure.

Closure is one of the most important language characteristics ever invented in programming. It underlies major programming paradigms including Functional Programming (FP), modules, and even a bit of class-oriented design.

## Sections
* [See the Closure](#see-the-closure)
  * [Pointed Closure](#pointed-closure)
  * [Adding Up Closures](#adding-up-closures)

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

[▲ Return to Sections](#sections)

| [Previous: Chapter 6 - Limiting Scope Exposure](../06/README.md) | [Table of Contents](../README.md#table-of-contents) |
