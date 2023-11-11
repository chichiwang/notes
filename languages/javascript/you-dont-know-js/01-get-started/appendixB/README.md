# Appendix B: Practice, Practice, Practice!
This appendix explores some exercises and their suggested solutions to provide practice with some of the concepts in this book.

My own solutions will be stored as JS files in this directory.

## Sections
* [Practicing Comparisons](#practicing-comparisons)

[◂ Return to Table of Contents](../README.md)

## Practicing Comparisons
Practice working with value types and comparions found in [Chapter 4, Pillar 3](../04/README.md#pillar-3-types-and-coercion) where coercion needs to be involved.

`scheduleMeeting(..)` should take a start time in a 24-hour format as a string "hh:mm" and a meeting duration in minutes. It should return `true` if a meeting falls entirely within the workday (according to times specified in `dayStart` and `dayEnd`). Return `false` if the meeting violates the work day bounds:

```javascript
const dayStart = "07:30";
const dayEnd = "17:45";

function scheduleMeeting(startTime,durationMinutes) {
  // ..TODO..
}

scheduleMeeting("7:00",15);     // false
scheduleMeeting("07:15",30);    // false
scheduleMeeting("7:30",30);     // true
scheduleMeeting("11:30",60);    // true
scheduleMeeting("17:00",45);    // true
scheduleMeeting("17:30",30);    // false
scheduleMeeting("18:00",15);    // false
```

My _over-engineered_ solution for this exercise: [comparions.mjs](./comparisons.mjs);

[▲ Return to Sections](#sections)

| [Previous: Appendix A - Exploring Further](../appendixA/README.md) | [Table of Contents](../README.md#table-of-contents) |
