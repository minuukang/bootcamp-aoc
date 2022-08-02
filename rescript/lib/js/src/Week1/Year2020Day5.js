// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Js_exn = require("rescript/lib/js/js_exn.js");
var Js_math = require("rescript/lib/js/js_math.js");
var Process = require("process");
var Caml_array = require("rescript/lib/js/caml_array.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");
var Caml_option = require("rescript/lib/js/caml_option.js");
var Caml_splice_call = require("rescript/lib/js/caml_splice_call.js");

function createSeatParser(lowerSpec, upperSpec, min, max, seatRows) {
  var match = seatRows.reduce((function (result, seatRow) {
          if (seatRow === lowerSpec) {
            return {
                    min: result.min,
                    max: Js_math.floor_int((result.max + result.min | 0) / 2.0)
                  };
          } else if (seatRow === upperSpec) {
            return {
                    min: Js_math.ceil_int((result.max + result.min | 0) / 2.0),
                    max: result.max
                  };
          } else {
            return Js_exn.raiseError("seat row can be '" + lowerSpec + "' or '" + upperSpec + "'");
          }
        }), {
        min: min,
        max: max
      });
  var min$1 = match.min;
  if (min$1 !== match.max) {
    Js_exn.raiseError("min & max should be equal");
  }
  return min$1;
}

function parseRow(param) {
  return createSeatParser("F", "B", 0, 127, param);
}

function parseColumn(param) {
  return createSeatParser("L", "R", 0, 7, param);
}

function getSeatId(row, column) {
  return (row << 3) + column | 0;
}

function parseSeat(seatStr) {
  var row = parseRow(seatStr.substring(0, 7).split(""));
  var column = parseColumn(seatStr.substr(-3).split(""));
  return {
          id: getSeatId(row, column),
          row: row,
          column: column
        };
}

var input = Fs.readFileSync(Process.cwd() + "/rescript/input/Week1/Year2020Day5.input.txt", "utf8");

var inputSeats = input.split("\n").map(parseSeat);

var inputSeatIds = inputSeats.map(function (seat) {
        return seat.id;
      }).sort();

var stepOneAnswer = Caml_splice_call.spliceApply(Math.max, [inputSeatIds]);

var stepTwoAnswer = Belt_Option.flatMap(Caml_option.undefined_to_opt(inputSeatIds.find(function (id, index) {
              return Caml_array.get(inputSeatIds, index + 1 | 0) === (id + 2 | 0);
            })), (function (value) {
        return value + 1 | 0;
      }));

console.log({
      stepOneAnswer: stepOneAnswer,
      stepTwoAnswer: stepTwoAnswer
    });

var maxColumnNumber = 7;

var maxRowNumber = 127;

var seatIdSpecificNumber = 8;

exports.maxColumnNumber = maxColumnNumber;
exports.maxRowNumber = maxRowNumber;
exports.seatIdSpecificNumber = seatIdSpecificNumber;
exports.createSeatParser = createSeatParser;
exports.parseRow = parseRow;
exports.parseColumn = parseColumn;
exports.getSeatId = getSeatId;
exports.parseSeat = parseSeat;
exports.input = input;
exports.inputSeats = inputSeats;
exports.inputSeatIds = inputSeatIds;
exports.stepOneAnswer = stepOneAnswer;
exports.stepTwoAnswer = stepTwoAnswer;
/* input Not a pure module */
