# Appendix B: Practice, Practice, Practice!
This appendix explores some exercises and their suggested solutions to provide practice with some of the concepts in this book.

My own solutions will be stored as JS files in this directory.

## Sections
* [Practicing Comparisons](#practicing-comparisons)
* [Practicing Closure](#practicing-closure)
* [Practicing Prototypes](#practicing-prototypes)
* [Suggested Solutions](#suggested-solutions)

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

## Practicing Closure
Practice working with closure as described in [Chapter 4, Pillar 1](#pillar-1-scope-and-closure).

The `range(..)` function takes a number as its first argument representing the first number in a desired range of numbers. The second number represents the end of the desired range (inclusive). If the second argument is omitted then a function is returned to accept that argument:

```javascript
function range(start,end) {
  // ..TODO..
}

range(3,3);    // [3]
range(3,8);    // [3,4,5,6,7,8]
range(3,0);    // []

var start3 = range(3);
var start4 = range(4);

start3(3);     // [3]
start3(8);     // [3,4,5,6,7,8]
start3(0);     // []

start4(6);     // [4,5,6]
```

My solution for this exercise: [closure.js](./closure.js);

[▲ Return to Sections](#sections)

## Practicing Prototypes
Practice working with the `this` keyword and objects linked via prototype as described in [Chapter 4, Pillar 2](#pillar-2-prototypes).

Define a slot machine with three reels that can `spin()` and then `display()` the current contents of all the reels.

The behavior of a single reel is defined in the `reel` object. The slot machine needs to have individual reels which each have a `position` property.

A reel only knows how to `display()` its current slot symbol. A slot machine typically shows three symbols per reel: current slot `position`, one slot above (`position`-1) and one slot below (`position`+1). Displaying the slot machine should display a 3x3 grid of symbols.

```javascript
function randMax(max) {
  return Math.trunc(1E9 * Math.random()) % max;
}

var reel = {
  symbols: [
    "♠", "♥", "♦", "♣", "☺", "★", "☾", "☀"
  ],
  spin() {
    if (this.position == null) {
      this.position = randMax(
        this.symbols.length - 1
      );
    }
    this.position = (
      this.position + 100 + randMax(100)
    ) % this.symbols.length;
  },
  display() {
    if (this.position == null) {
      this.position = randMax(
        this.symbols.length - 1
      );
    }
    return this.symbols[this.position];
  }
};

var slotMachine = {
  reels: [
    // this slot machine needs 3 separate reels
    // hint: Object.create(..)
  ],
  spin() {
    this.reels.forEach(function spinReel(reel){
      reel.spin();
    });
  },
  display() {
    // TODO
  }
};

slotMachine.spin();
slotMachine.display();
// ☾ | ☀ | ★
// ☀ | ♠ | ☾
// ♠ | ♥ | ☀

slotMachine.spin();
slotMachine.display();
// ♦ | ♠ | ♣
// ♣ | ♥ | ☺
// ☺ | ♦ | ★
```

Hints:
* Use the `%` modulo operator for wrapping `position` to access symbols circularly around a reel.
* Use `Object.create(..)` to create an object and prototype-link it to another object. Once linked delegation allows the objects to share methods while perserving their individual `this` contexts.
* Instead of using the `reel` object to directly show each of the three positions, another temporary object can be used to delegate from.

My _over-engineered_ solution for this exercise: [prototypes.mjs](./prototypes.mjs);

[▲ Return to Sections](#sections)

## Suggested Solutions
There are many ways to solve these practice problems, these are just some suggested approaches:

Suggested solution for [Practicing Comparisons](#practicing-comparisons):

```javascript
const dayStart = "07:30";
const dayEnd = "17:45";

function scheduleMeeting(startTime,durationMinutes) {
  var [ , meetingStartHour, meetingStartMinutes ] =
    startTime.match(/^(\d{1,2}):(\d{2})$/) || [];

  durationMinutes = Number(durationMinutes);

  if (
    typeof meetingStartHour == "string" &&
    typeof meetingStartMinutes == "string"
  ) {
    let durationHours =
      Math.floor(durationMinutes / 60);
    durationMinutes =
      durationMinutes - (durationHours * 60);
    let meetingEndHour =
      Number(meetingStartHour) + durationHours;
    let meetingEndMinutes =
      Number(meetingStartMinutes) +
      durationMinutes;

    if (meetingEndMinutes >= 60) {
      meetingEndHour = meetingEndHour + 1;
      meetingEndMinutes =
        meetingEndMinutes - 60;
    }

    // re-compose fully-qualified time strings
    // (to make comparison easier)
    let meetingStart = `${
      meetingStartHour.padStart(2,"0")
    }:${
      meetingStartMinutes.padStart(2,"0")
    }`;
    let meetingEnd = `${
      String(meetingEndHour).padStart(2,"0")
    }:${
      String(meetingEndMinutes).padStart(2,"0")
    }`;

    // NOTE: since expressions are all strings,
    // comparisons here are alphabetic, but it's
    // safe here since they're fully qualified
    // time strings (ie, "07:15" < "07:30")
    return (
      meetingStart >= dayStart &&
      meetingEnd <= dayEnd
    );
  }

  return false;
}

scheduleMeeting("7:00",15);     // false
scheduleMeeting("07:15",30);    // false
scheduleMeeting("7:30",30);     // true
scheduleMeeting("11:30",60);    // true
scheduleMeeting("17:00",45);    // true
scheduleMeeting("17:30",30);    // false
scheduleMeeting("18:00",15);    // false
```

---

Suggested solution for [Practicing Closure](#practicing-closure):

```javascript
function range(start,end) {
  start = Number(start) || 0;

  if (end === undefined) {
    return function getEnd(end) {
      return getRange(start,end);
    };
  }
  else {
    end = Number(end) || 0;
    return getRange(start,end);
  }


  // **********************

  function getRange(start,end) {
    var ret = [];
    for (let i = start; i <= end; i++) {
      ret.push(i);
    }
    return ret;
  }
}

range(3,3);    // [3]
range(3,8);    // [3,4,5,6,7,8]
range(3,0);    // []

var start3 = range(3);
var start4 = range(4);

start3(3);     // [3]
start3(8);     // [3,4,5,6,7,8]
start3(0);     // []

start4(6);     // [4,5,6]
```

---

Suggested solution for [Practicing Prototypes](#practicing-prototypes):

```javascript
function randMax(max) {
  return Math.trunc(1E9 * Math.random()) % max;
}

var reel = {
  symbols: [
    "♠", "♥", "♦", "♣", "☺", "★", "☾", "☀"
  ],
  spin() {
    if (this.position == null) {
      this.position = randMax(
        this.symbols.length - 1
      );
    }
    this.position = (
      this.position + 100 + randMax(100)
    ) % this.symbols.length;
  },
  display() {
    if (this.position == null) {
      this.position = randMax(
        this.symbols.length - 1
      );
    }
    return this.symbols[this.position];
  }
};

var slotMachine = {
  reels: [
    Object.create(reel),
    Object.create(reel),
    Object.create(reel)
  ],
  spin() {
    this.reels.forEach(function spinReel(reel){
      reel.spin();
    });
  },
  display() {
    var lines = [];

    // display all 3 lines on the slot machine
    for (
      let linePos = -1; linePos <= 1; linePos++
    ) {
      let line = this.reels.map(
        function getSlot(reel){
          var slot = Object.create(reel);
          slot.position = (
            reel.symbols.length +
            reel.position +
            linePos
          ) % reel.symbols.length;
          return slot.display();
        }
      );
      lines.push(line.join(" | "));
    }

    return lines.join("\n");
  }
};

slotMachine.spin();
slotMachine.display();
// ☾ | ☀ | ★
// ☀ | ♠ | ☾
// ♠ | ♥ | ☀

slotMachine.spin();
slotMachine.display();
// ♦ | ♠ | ♣
// ♣ | ♥ | ☺
// ☺ | ♦ | ★
```

| [Previous: Appendix A - Exploring Further](../appendixA/README.md) | [Table of Contents](../README.md#table-of-contents) |
