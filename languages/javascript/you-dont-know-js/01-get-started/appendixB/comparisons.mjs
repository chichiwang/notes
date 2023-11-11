import * as readline from "node:readline/promises";
import {
  stdin as input,
  stdout as output,
} from "node:process";

/**
 * Scheduling Utilities
 */

const dayStart = "07:30";
const dayEnd = "17:45";

function isValidTimeString(timeString) {
  const timeRegex = /^[0-2]?[0-9]:[0-6][0-9]$/;

  return Boolean(timeString.match(timeRegex));
}

function timeToNumbers(time) {
  return time.split(":").map(Number);
}

function isValidTime([hours, minutes]) {
  const isHoursValid = (hours >= 0) && (hours <= 24);
  const isMinutesValid = (minutes >= 0) && (minutes < 60) && !(hours === 24 && minutes > 0);

  return isHoursValid && isMinutesValid;
}

function isValidDuration(duration) {
  return Number.isInteger(duration);
}

function addTimeMinutes([hours, minutes], minutesToAdd) {
  const maxMinutes = 60;

  let newMinutes = minutes + minutesToAdd;
  let newHours = hours;

  while (newMinutes >= maxMinutes) {
    newMinutes = newMinutes - maxMinutes;
    newHours = newHours === 23 ? 0 : newHours + 1;
  }

  return [newHours, newMinutes];
}

function timeEqual([hoursA, minutesA], [hoursB, minutesB]) {
  return (hoursA === hoursB) && (minutesA === minutesB);
}

function timeLessThanEqual([hoursA, minutesA], [hoursB, minutesB]) {
  const isEqual = timeEqual([hoursA, minutesA], [hoursB, minutesB]);
  const isLessThan = !isEqual && (hoursA === hoursB && minutesA < minutesB) || hoursA < hoursB;

  return isEqual || isLessThan;
}

function timeGreaterThanEqual([hoursA, minutesA], [hoursB, minutesB]) {
  const isEqual = timeEqual([hoursA, minutesA], [hoursB, minutesB]);
  const isGreaterThan = !isEqual && (hoursA === hoursB && minutesA > minutesB) || hoursA > hoursB;

  return isEqual || isGreaterThan;
}

function scheduleMeeting(startTime,durationMinutes) {
  const startTimeNumbers = timeToNumbers(startTime);
  const endTimeNumbers = addTimeMinutes(startTimeNumbers, durationMinutes);
  const dayStartNumbers = timeToNumbers(dayStart);
  const dayEndNumbers = timeToNumbers(dayEnd);

  const [startTimeHours, startTimeMinutes] = startTimeNumbers;
  const isStartTimeLegal = (startTimeHours === 24 && startTimeMinutes === 0) || (
    (startTimeHours >= 0 && startTimeHours < 24) &&
    (startTimeMinutes >= 0 && startTimeMinutes < 60)
  );

  if (!isStartTimeLegal) {
    throw new Error(`Invalid start time: ${startTime}, please provide a valid start time in the format HH:MM`);
  }

  const isStartTimeValid = timeGreaterThanEqual(startTimeNumbers, dayStartNumbers)
    && timeLessThanEqual(startTimeNumbers, dayEndNumbers);
  const isEndTimeValid = timeGreaterThanEqual(endTimeNumbers, dayStartNumbers)
    && timeLessThanEqual(endTimeNumbers, dayEndNumbers);

  return isStartTimeValid && isEndTimeValid;
}

/**
 * Prompt User
 */

const promptInput = readline.createInterface({ input, output });

function promptDuration(time) {
  if (!isValidTimeString(time)) {
    throw new Error(`Invalid start time: ${time}, please use the format HH:MM`);
  }

  return promptInput.question("Duration in Minutes: ")
    .then(function processSchedule(duration) {
      const numericDurationRegex = /^\d+$/;

      if (!duration.match(numericDurationRegex)) {
        throw new Error(`Invalid duration provided: ${duration}, please provide a number representing the number of minutes`);
      }

      const durationNumber = parseInt(duration);

      if (durationNumber === 0) {
        throw new Error(`Cannot set a meeting for ${duration} minutes`);
      }

      return scheduleMeeting(time, durationNumber);
    });
}

function scheduleOutput(response) {
  console.log("Meeting Scheduled: ", response);
}

function terminatePrompt() {
  promptInput.close();
}

function logError(e) {
  console.error(e);
}

promptInput.question("Start time: ")
  .then(promptDuration)
  .then(scheduleOutput)
  .catch(logError)
  .finally(terminatePrompt);

// scheduleMeeting("7:00",15);     // false
// scheduleMeeting("07:15",30);    // false
// scheduleMeeting("7:30",30);     // true
// scheduleMeeting("11:30",60);    // true
// scheduleMeeting("17:00",45);    // true
// scheduleMeeting("17:30",30);    // false
// scheduleMeeting("18:00",15);    // false
