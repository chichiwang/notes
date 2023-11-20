# Chapter 2: Illustrating Lexical Scope
JavaScript scope is determined during code compilation, a model called _lexical scope_. The term "lexical" refers to the first stage of compilation (lexing/parsing). This chapter will illustrate scope with several metaphors with the goal of creating more accurate mental models of how a program is handled by the JavaScript engine.

## Sections
* [Marbles, and Buckets, and Bubbles... Oh My!](#marbles-and-buckets-and-bubbles-oh-my)

[◂ Return to Table of Contents](../README.md)

## Marbles, and Buckets, and Bubbles... Oh My!
An effective metaphor for understanding scope is sorting colored marbles into buckets of the corresponding color. Imagine a pile of marbles that are colored red, blue, and green. Imagine sorting the marbles into buckets of the same color: red marbles in a red bucket, blue in blue, and green in green.

In this metaphor the marbles are variables in a program, and the buckets are scopes (functions and blocks).

Annotating the example program from [Chapter 1](../01/README.md#compiler-speak) with scope color labels:

```javascript
// outer/global scope: RED

var students = [
  { id: 14, name: "Kyle" },
  { id: 73, name: "Suzy" },
  { id: 112, name: "Frank" },
  { id: 6, name: "Sarah" }
];

function getStudentName(studentID) {
// function scope: BLUE

  for (let student of students) {
    // loop scope: GREEN

    if (student.id == studentID) {
      return student.name;
    }
  }
}

var nextStudent = getStudentName(73);
console.log(nextStudent);   // Suzy
```

Three scopes have been designated with colors: RED (outermost global scope), BLUE (scope of the function `getStudentName(..)`), and GREEN (scope of/inside the `for` loop).

This is easier to visualize with color coding:

![Figure 2: Colored Scope Bubbles](./fig2.png)

**Note**: Technically, the parameter `studentID` is not exactly in the BLUE scope. This is explained in greater detail in "Implied Scopes" in Appendix A. For the purposes of this metaphor it is close enough to label `studentID` as belonging to the BLUE scope.

Scope bubbles are determined at compilation according to where the functions/blocks are written and their nesting. Each scope bubble is contained entirely within its parent scope bubble. A scope is never partially in two different outer scopes.

Each variable/identifier is colored based on the scope that it is declared in, not the scope it may be accessed from.

Scopes can nest inside each other to any depth of nesting as the program dictates. References to variables/identifiers are allowed if there's a matching declaration in either the current scope, or in any scope above/outside the current scope - but not for delcarations in lower/nested scopes.

**Note**: The JavaScript engine does not look up the scope of variable references at runtime. At compilation most or all variable references have their scope-access determined and stored to avoid avoid unnecessary lookups at runtime. This is covered in greater detail in Chapter 3.

Key take-aways:
* Variables are declared in specific scopes, which can be thought of as colored marbles from matching-color buckets.
* Any variable reference that appears either in the scope it was declared, or a deeper-nested scope, will be labeled as a marble of the same color as the scope.
  * The exception is if an intervening scope _shadows_ the variable declaration (_shadowing_ covered in Chapter 3).
* The determination of colored buckets, and the marbles in them, happens during compilation. This information is used for varaible lookups during execution.

[▲ Return to Sections](#sections)

| [Previous: Chapter 1 - What's The Scope?](../01/README.md) | [Table of Contents](../README.md#table-of-contents) |
