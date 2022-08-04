// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Js_exn = require("rescript/lib/js/js_exn.js");
var Js_math = require("rescript/lib/js/js_math.js");
var Process = require("process");
var Belt_List = require("rescript/lib/js/belt_List.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");
var Caml_splice_call = require("rescript/lib/js/caml_splice_call.js");

var _map = {"lower":"F","upper":"B","lower":"L","upper":"R"};

var _revMap = {"F":"lower","B":"upper","L":"lower","R":"upper"};

function seatDirectionMapToJs(param) {
  return _map[param];
}

function seatDirectionMapFromJs(param) {
  return _revMap[param];
}

var _map$1 = {"row":"F","row":"B","column":"L","column":"R"};

var _revMap$1 = {"F":"row","B":"row","L":"column","R":"column"};

function seatTypeMapToJs(param) {
  return _map$1[param];
}

function seatTypeMapFromJs(param) {
  return _revMap$1[param];
}

function createSeatParser(min, max, seatRows) {
  var match = Belt_Array.reduce(seatRows, {
        min: min,
        max: max
      }, (function (result, seatRow) {
          if (seatRow === "upper") {
            return {
                    min: Js_math.ceil_int((result.max + result.min | 0) / 2.0),
                    max: result.max
                  };
          } else {
            return {
                    min: result.min,
                    max: Js_math.floor_int((result.max + result.min | 0) / 2.0)
                  };
          }
        }));
  var min$1 = match.min;
  if (min$1 !== match.max) {
    Js_exn.raiseError("min & max should be equal");
  }
  return min$1;
}

function parseRow(param) {
  return createSeatParser(0, 127, param);
}

function parseColumn(param) {
  return createSeatParser(0, 7, param);
}

function getSeatId(row, column) {
  return (row << 3) + column | 0;
}

function parseSeat(seatStr) {
  var seatSpecs = seatStr.split("");
  var row = createSeatParser(0, 127, Belt_Array.keepMap(seatSpecs, (function (spec) {
              var t = seatTypeMapFromJs(spec);
              if (t === "row") {
                return seatDirectionMapFromJs(spec);
              }
              
            })));
  var column = createSeatParser(0, 127, Belt_Array.keepMap(seatSpecs, (function (spec) {
              var t = seatTypeMapFromJs(spec);
              if (t === "column") {
                return seatDirectionMapFromJs(spec);
              }
              
            })));
  return {
          id: getSeatId(row, column),
          row: row,
          column: column
        };
}

var input = Fs.readFileSync(Process.cwd() + "/rescript/input/Week1/Year2020Day5.input.txt", "utf8");

var inputSeats = Belt_Array.map(input.split("\n"), parseSeat);

var inputSeatIds = Belt_List.toArray(Belt_List.sort(Belt_List.fromArray(Belt_Array.map(inputSeats, (function (seat) {
                    return seat.id;
                  }))), (function (a, b) {
            return a - b | 0;
          })));

var stepOneAnswer = Caml_splice_call.spliceApply(Math.max, [inputSeatIds]);

var stepTwoAnswer = Belt_Option.flatMap(Belt_Array.get(Belt_Array.keepWithIndex(inputSeatIds, (function (id, index) {
                return Belt_Option.mapWithDefault(Belt_Array.get(inputSeatIds, index + 1 | 0), false, (function (value) {
                              return value === (id + 2 | 0);
                            }));
              })), 0), (function (value) {
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
exports.seatDirectionMapToJs = seatDirectionMapToJs;
exports.seatDirectionMapFromJs = seatDirectionMapFromJs;
exports.seatTypeMapToJs = seatTypeMapToJs;
exports.seatTypeMapFromJs = seatTypeMapFromJs;
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
