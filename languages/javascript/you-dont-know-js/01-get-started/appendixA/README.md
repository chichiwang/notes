# Appendix A: Exploring Further
This appendix will explore some of the topics from the main chapter text in greater detail.

## Sections
* [Values vs. References](#values-vs-references)

[◂ Return to Table of Contents](../README.md)

## Values vs. References
In many languages a developer chooses between assigning/passing a value as either the value itself or as a reference to the value. In JavaScript this decision is entirely determined by the kind of value.

Assigning or passing the value itself will make a copy of that value:

```javascript
var myName = "Kyle";

var yourName = myName;
```

In the above example `yourName` references a copy of the string `"Kyle"` separate from the string that the `myName` variable contains. This is because the value is a primitive (a string in this case) and primitive values are always assigned or passed as _value copies_.

The proof that primitives are assigned/passed as values:

```javascript
var myName = "Kyle";

var yourName = myName;

myName = "Frank";

console.log(myName);
// Frank

console.log(yourName);
// Kyle
```

In the above example `yourName` is not affected by the reassignment of `myName`. This is because each variable holds its own copy of the value.

Reference would mean that variables point to the same shared value - changes to the shared value would be reflected across all variables that reference it. In JavaScript object values (arrays, objects, functions, etc) are treated as references:

```javascript
var myAddress = {
  street: "123 JS Blvd",
  city: "Austin",
  state: "TX",
};

var yourAddress = myAddress;

// I've got to move to a new house!
myAddress.street = "456 TS Ave";

console.log(yourAddress.street);
// 456 TS Ave
```

Both `myAddress` and `yourAddress` contain references to the same object, so the property reassignment made to `myAddress.street` is reflected by access to `yourAddress.street`.

JavaScript chooses value-copy vs. reference-copy based on the value type: primitives are held by value, objects are held by reference.

[▲ Return to Sections](#sections)

| [Previous: Chapter 4 - The Bigger Picture](../04/README.md) | [Table of Contents](../README.md#table-of-contents) |
