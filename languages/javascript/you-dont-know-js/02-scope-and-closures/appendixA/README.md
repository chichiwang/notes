# Appendix A: Exploring Further
This appendix is optional, supporting material.

Disclaimer: The discussions contained within are more heavily influenced by the author's opinions than the main text is.

## Sections
* [Implied Scopes](#implied-scopes)
  * [Parameter Scope](#parameter-scope)

[◂ Return to Table of Contents](../README.md)

## Implied Scopes
Scopes are occasionally created in non-obvious places. These implied scopes do not often impact program behavior, but it is still useful to know about them:
* Parameter scope
* Function name scope

#### Parameter Scope

[Chapter 2](../02/README.md#a-conversation-among-friends) implies that function parameters are the same as locally declared variables in the function scope. This is not always true.

```javascript
// outer/global scope: RED(1)

function getStudentName(studentID) {
  // function scope: BLUE(2)

  // ..
}
```

In the above example, `studentID` is considered a _simple_ parameter, and therefore does behave as a member of the BLUE(2) function scope. This is not technically the case with non-simple parameters.

Non-simple parameters include:
* Parameters with default values
* Rest parameters (using the spread operator `...`)
* Destructured parameters

```javascript
// outer/global scope: RED(1)

function getStudentName(/*BLUE(2)*/ studentID = 0) {
  // function scope: GREEN(3)

  // ..
}
```

In this example, `studentID` is given a default value of `0`. The parameter list, in this case, essentially becomes its own scope and the function scope is nested inside of it.

Why does this matter? Consider:

```javascript
function getStudentName(studentID = maxID, maxID) {
  // ..
}
```

The code above produces a [TDZ error](../05/README.md#unitialized-variables-aka-tdz) because, assuming left-to-right operations, the default assignment `= maxID` to `studentID` requires a `maxID` variable to already exist. `maxID` is declared within the parameter scope, but it has not yet been initialized due to the order of the paramters. If the parameter list is flipped, no TDZ error occurs:

```javascript
function getStudentName(maxID,studentID = maxID) {
  // ..
}
```

It gets even trickier when the default parameter is assigned a function expression, which can then create its own closure within this implied parameter scope:

```javascript
function whatsTheDealHere(id,defaultID = () => id) {
  id = 5;
  console.log( defaultID() );
}

whatsTheDealHere(3);
// 5
```

In the above code, `defaultID`'s default function behavior holds closure over the `id` variable in the parameter, which is then reassigned to `5`.

The implied parameter scope can be revealed with shadowing thusly:

```javascript
function whatsTheDealHere(id,defaultID = () => id) {
  var id = 5;
  console.log( defaultID() );
}

whatsTheDealHere(3);
// 3
```

Using a `var` declaration to assign `id` to `5` creates a variable `id` that shadows the `id` in the parameter list. The closure of `defaultID` is over the `id` within the parameter list, not the `id` in the function scope, proving there's a scope bubble around the parameter list.

Strange, unexpected behaviors can occur with this implied parameter scope, though:

```javascript
function whatsTheDealHere(id,defaultID = () => id) {
  var id;

  console.log(`local variable 'id': ${ id }`);
  console.log(
    `parameter 'id' (closure): ${ defaultID() }`
  );

  console.log("reassigning 'id' to 5");
  id = 5;

  console.log(`local variable 'id': ${ id }`);
  console.log(
    `parameter 'id' (closure): ${ defaultID() }`
  );
}

whatsTheDealHere(3);
// local variable 'id': 3   <--- Huh!? Weird!
// parameter 'id' (closure): 3
// reassigning 'id' to 5
// local variable 'id': 5
// parameter 'id' (closure): 3
```

The first console message should reference the function-scoped `id`, which at the point of execution should be `undefined` (assuming the variable declaration `var id` auto-initialized `id` to `undefined`).

In this corner case, JavaScript does not auto-initialize `id` to `undefined`, but rather to the value of the `id` parameter (`3`). The reason for this behavior is for legacy compatibility. However, it is clear there are still 2 `id` variables in different scopes, as observed after reassigning the function-scoped `id` to `5`.

The author's advice, in order to avoid these strange behaviors:
* Never shadow parameters with local variables.
* Avoid using a default parameter that closes over any of the parameters.

[▲ Return to Sections](#sections)

| [Previous: Chapter 8 - The Module Pattern](../08/README.md) | [Table of Contents](../README.md#table-of-contents) |
