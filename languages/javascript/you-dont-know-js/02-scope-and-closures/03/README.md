# Chapter 3: The Scope Chain
The connections between scopes that are nested within other scopes is called the _scope chain_, which determines the path along which variables can be accessed. Lookup moves upwards/outwards only.

## Sections
* ["Lookup" Is (Mostly) Conceptual](#lookup-is-mostly-conceptual)
* [Shadowing](#shadowing)
  * [Global Unshadowing Trick](#global-unshadowing-trick)

[◂ Return to Table of Contents](../README.md)

## "Lookup" Is (Mostly) Conceptual
The scope where the declaration of any given identifier access originates is _usually determined_ during the initial compilation process. This information will likely be stored or accessible from each variable's entry in the AST. That information is then used explicitly by the executable instructions that constitute the program's runtime.

This means that, in practical execution, the _Engine_ from [Chapter 2](../02/README.md#a-conversation-among-friends) does not actually look through scopes to locate where an identifier is declared. Avoiding this runtime lookup is a key optimization benefit of lexical scope.

However, there is a case where scope needs to be deferred to runtime: any variable that isn't declared in any lexically available scope in a given file may have been declared in the shared global scope in a different JavaScript file. Because of this case: any variable that is initially undeclared is left as unscoped during that file's compilation. These variables are left unscoped until other relevant files have been compiled and the application runtime commences. This deferred lookup will eventually resolve to whichever scope the variable is found to have been declared (likely the global scope). This looup is only needed once per variable at most - nothing else at runtime could later change that variable's scope.

[▲ Return to Sections](#sections)

## Shadowing
A program where all the variable names used are unique does not necessarily benefit from having multiple scopes. Every variable could conceivably share the same global scope bucket without conflict.

A program containing multiple variables, each in different scopes, with the same lexical names benefits greatly from lexical scope. A single scope cannot contain two or more variables with the same name - multiple references would be assumed as just one variable.

Consider:

```javascript
var studentName = "Suzy";

function printStudent(studentName) {
  studentName = studentName.toUpperCase();
  console.log(studentName);
}

printStudent("Frank");
// FRANK

printStudent(studentName);
// SUZY

console.log(studentName);
// Suzy
```

The `studentName` variable on line one occupies the global scope bucket. The `studentName` defined as parameter on line 3 belongs to the `printStudent(..)` function bucket.

The references to `studentName` on lines 4 and 5, within the `printStudent(..)` function all refer to the `studentName` defined in the parameter on line 3, scoped to the `printStudent(..)` function bucket.

This is a key aspect of lexical scope behavior known as _shadowing_. The `printStudent(..)`-scoped `studentName` variable _shadows_ the global-scoped `stuentName` defined on line 1. The parameter `studentName` is _shadowing_ the (_shadowed_) global variable `studentName`.

One side-effect of shadowing a variable from an outer scope is that it becomes lexically impossible to reference that shadowed variable from the outer scope.

#### Global Unshadowing Trick
It is possible to access a global variable from a scope where that variable has been shadowed. In the global scope `var` and `function` declared variables also expose themselves as properties on the _global object_ (an object representation of the global scope). In the browser, developers recognize the global object as `window` (this is not _entirely_ accurate but good enough for this discussion).

Consider this program, executed as a standalone `.js` file in a browser environment:

```javascript
var studentName = "Suzy";

function printStudent(studentName) {
  console.log(studentName);
  console.log(window.studentName);
}

printStudent("Frank");
// "Frank"
// "Suzy"
```

The expression `window.studentName` is accessing the global variable `studentName` as a property on the `window` object (for the purposes of this discussion: synonymous with the global object). This is the only way to access a shadowed variable from inside a scope where the shadowing variable is present.

`window.studentName` is a mirror of the global variable `studentName` - changes to one will be reflected in the other. `window.studentName` can be thought of as a setter/getter for the actual `studentName` variable.

A variable can be added to the global scope by setting/creating a property on the global object.

**NOTE**: As a matter of good practice do not: shadow a variable from an outer-scope if access is required, access a global variable that has been shadowed, nor create variables as `window` properties instead of with formal declarations.

This trick only works for accessing the global variable scope (not a shadowed variable from a nested scope), and only for variables declared with `var` or `function`. Other declarations do not create mirrored global object properties:

```javascript
var one = 1;
let notOne = 2;
const notTwo = 3;
class notThree {}

console.log(window.one);       // 1
console.log(window.notOne);    // undefined
console.log(window.notTwo);    // undefined
console.log(window.notThree);  // undefined
```

Variables that exist in any scope other than the global scope are completely inaccessible from a scope where they have been shadowed:

```javascript
var special = 42;

function lookingFor(special) {
  // The identifier `special` (parameter) in this
  // scope is shadowed inside keepLooking(), and
  // is thus inaccessible from that scope.

  function keepLooking() {
    var special = 3.141592;
    console.log(special);
    console.log(window.special);
  }

  keepLooking();
}

lookingFor(112358132134);
// 3.141592
// 42
```

[▲ Return to Sections](#sections)

| [Previous: Chapter 2 - Illustrating Lexical Scope](../02/README.md) | [Table of Contents](../README.md#table-of-contents) |
