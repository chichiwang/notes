import * as readline from "node:readline/promises";
import {
  stdin as input,
  stdout as output,
} from "node:process";

/**
 * Slot machine utilities
 */
function randMax(max) {
  return Math.trunc(1E9 * Math.random()) % max;
}

var reel = {
  symbols: [
    "♠", "♥", "♦", "♣", "☺", "★", "☾", "☀"
  ],

  initializePosition() {
    if (this.position == null) {
      this.position = randMax(
        this.symbols.length - 1
      );
    }
  },

  spin() {
    this.initializePosition();

    this.position = (
      this.position + 100 + randMax(100)
    ) % this.symbols.length;
  },

  displayPrev() {
    this.initializePosition();

    const prevPosition = this.position === 0 ? this.symbols.length - 1 : this.position - 1;

    return this.symbols[prevPosition];
  },

  display() {
    this.initializePosition();

    return this.symbols[this.position];
  },

  displayNext() {
    this.initializePosition();

    const nextPosition = (this.position + 1) % this.symbols.length;

    return this.symbols[nextPosition];
  },
};

var slotMachine = {
  reels: [
    Object.create(reel),
    Object.create(reel),
    Object.create(reel),
  ],

  spin() {
    this.reels.forEach(function spinReel(reel){
      reel.spin();
    });
  },

  display() {
    const reelsMap = this.reels.map(function getDisplay(reel) {
      return [reel.displayPrev(), reel.display(), reel.displayNext()];
    });

    console.log(`\n
      ${ reelsMap[0][0] } | ${ reelsMap[1][0] } | ${ reelsMap[2][0] }\n
      ${ reelsMap[0][1] } | ${ reelsMap[1][1] } | ${ reelsMap[2][1] }\n
      ${ reelsMap[0][2] } | ${ reelsMap[1][2] } | ${ reelsMap[2][2] }\n
    `);
  },
};

/**
 * Prompt Utilities
 */

const promptUser = readline.createInterface({ input, output });

function runPrompt() {
  promptUser.question("Spin the slot machine (y)/n? ")
    .then(function spinToWin(response) {
      if (response.toLowerCase() === "n") {
        return promptUser.close();
      }

      slotMachine.spin();
      slotMachine.display();
      return runPrompt();
    });
}

/**
 * Start the command line
 */

runPrompt();

// slotMachine.spin();
// slotMachine.display();
// ☾ | ☀ | ★
// ☀ | ♠ | ☾
// ♠ | ♥ | ☀

// slotMachine.spin();
// slotMachine.display();
// ♦ | ♠ | ♣
// ♣ | ♥ | ☺
// ☺ | ♦ | ★
