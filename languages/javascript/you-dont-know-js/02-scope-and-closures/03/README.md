# Chapter 3: The Scope Chain
The connections between scopes that are nested within other scopes is called the _scope chain_, which determines the path along which variables can be accessed. Lookup moves upwards/outwards only.

## Sections
* ["Lookup" Is (Mostly) Conceptual](#lookup-is-mostly-conceptual)
* [Shadowing](#shadowing)
  * [Global Unshadowing Trick](#global-unshadowing-trick)
  * [Copying Is Not Accessing](#copying-is-not-accessing)
  * [Illegal Shadowing](#illegal-shadowing)
* [Function Name Scope](#function-name-scope)

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

#### Copying Is Not Accessing

Consider:

```javascript
var special = 42;

function lookingFor(special) {
  var another = {
    special: special
  };

  function keepLooking() {
    var special = 3.141592;
    console.log(special);
    console.log(another.special);  // Ooo, tricky!
    console.log(window.special);
  }

  keepLooking();
}

lookingFor(112358132134);
// 3.141592
// 112358132134
// 42
```

Although `another.special` seems to be a workaround to allow access to a nested shadowed variable, it does not actually bypass the rule. `another.special` is now a copy of the parameter `special` defined by the function `lookingFor(..)`. The value stored in the parameter `special` scoped to `lookingFor(..)` cannot be reassigned from within `keepLooking(..)` through the `another.special` copy.

#### Illegal Shadowing

Not all combinations of shadowing are allowed: `let` can shadow `var`, but `var` cannot shadow `let`:

```javascript
function something() {
  var special = "JavaScript";

  {
    let special = 42;   // totally fine shadowing

    // ..
  }
}

function another() {
  // ..

  {
    let special = "JavaScript";

    {
      var special = "JavaScript";
      // ^^^ Syntax Error

      // ..
    }
  }
}
```

The syntax error thrown in `another()` indicates that `special` has already been defined. The reason this is raised as a `SyntaxError` is because `var` is attempting to "cross the boundary":  create a function scoped variable `special` over-top the `let` declared variable of the same name.

This boundary-crossing prohibition stops at each function boundary, so this example raises no exception:

```javascript
function another() {
  // ..

  {
    let special = "JavaScript";

    ajax("https://some.url",function callback(){
      // totally fine shadowing
      var special = "JavaScript";

      // ..
    });
  }
}
```

Summary:
* Within an inner scope `let` can always shadow an outer scope's `var`.
* Within an inner scope `var` can only shadow an outer scope's `let` if there is a function boundary in between.

[▲ Return to Sections](#sections)

## Function Name Scope
```javascript
function askQuestion() {
  // ..
}
```

A `function` declaration like this will create an identifier (`askQuestion`) in the enclosing scope (in this case, the global scope).

```javascript
var askQuestion = function(){
  // ..
};
```

This `var` declared variable `askQuestion` will also create a global-scoped identifier. However, since this is a `function` expression (and not a declaration) the function itself will not _hoist_.

```javascript
var askQuestion = function ofTheTeacher(){
  // ..
};
```

One major difference between `function` declarations and `function` expressions is the behavior of the name identifier of the function. In the above _named_ `function` expression the `askQuestion` variable belongs to the global scope. However, the variable `ofTheTeacher` is declared as an identifier within the function itself:

```javascript
var askQuestion = function ofTheTeacher() {
  console.log(ofTheTeacher);
};

askQuestion();
// function ofTheTeacher()...

console.log(ofTheTeacher);
// ReferenceError: ofTheTeacher is not defined
```

**NOTE**: `ofTheTeacher` does not exactly belong to the function scope. [Appendix A: Implied Scopes](../appendixA/README.md#implied-scopes) goes into greater detail.

`ofTheTeacher` is not only declared within the function, rather than outside, it is also defined as read-only:

```javascript
var askQuestion = function ofTheTeacher() {
  "use strict";
  ofTheTeacher = 42;   // TypeError

  //..
};

askQuestion();
// TypeError
```

In strict-mode the assignment of `ofTheTeacher` fails as a `TypeError`, in non-strict-mode the assignment fails silently.

In the case of an _anonymous_ function expression:

```javascript
var askQuestion = function(){
  // ..
};
```

Anonymous function expressions have no named identifier that affects either scope.

[▲ Return to Sections](#sections)

| [Previous: Chapter 2 - Illustrating Lexical Scope](../02/README.md) | [Table of Contents](../README.md#table-of-contents) |
