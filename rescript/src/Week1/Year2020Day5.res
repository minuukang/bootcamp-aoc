let maxColumnNumber = 7
let maxRowNumber = 127
let seatIdSpecificNumber = 8

type minmax = {
  min: int,
  max: int,
}

type seat = {
  id: int,
  row: int,
  column: int
}

let createSeatParser = (~lowerSpec: string, ~upperSpec: string, ~min: int, ~max: int) => 
  (seatRows: array<string>) => {
    let {min, max} = seatRows
      ->Belt.Array.reduce(
        { min, max },
        (result, seatRow) => {
          if (seatRow == lowerSpec) {
            { min: result.min, max: Js.Math.floor_int((result.max + result.min)->Belt.Int.toFloat /. 2.0) }
          } else if (seatRow == upperSpec) {
            { min: Js.Math.ceil_int((result.max + result.min)->Belt.Int.toFloat /. 2.0), max: result.max }
          } else {
            Js.Exn.raiseError(`seat row can be '${lowerSpec}' or '${upperSpec}'`)
          }
        }
      )
    if min != max {
      Js.Exn.raiseError("min & max should be equal")
    }
    min
  }

let parseRow = createSeatParser(~lowerSpec="F", ~upperSpec="B", ~min=0, ~max=maxRowNumber)
let parseColumn = createSeatParser(~lowerSpec="L", ~upperSpec="R", ~min=0, ~max=maxColumnNumber)
let getSeatId = (~row, ~column) => row * seatIdSpecificNumber + column

let parseSeat = (seatStr: string) => {
  let row = parseRow(seatStr->Js.String2.substring(~from=0, ~to_=7)->Js.String2.split(""))
  let column = parseColumn(seatStr->Js.String2.substr(~from=-3)->Js.String2.split(""))
  {
    id: getSeatId(~row, ~column),
    row,
    column
  }
}

/* Comment */

let input = Node.Fs.readFileAsUtf8Sync(Node.Process.cwd() ++ "/rescript/input/Week1/Year2020Day5.input.txt")

let inputSeats = input
  ->Js.String2.split("\n")
  ->Belt.Array.map(parseSeat)

let inputSeatIds = inputSeats
  ->Belt.Array.map(seat => seat.id)
  ->Belt.List.fromArray
  ->Belt.List.sort((a, b) => a - b)
  ->Belt.List.toArray

let stepOneAnswer = inputSeatIds->Js.Math.maxMany_int
let stepTwoAnswer = inputSeatIds
  ->Belt.Array.keepWithIndex((id, index) => inputSeatIds[index + 1] == id + 2)
  ->Belt.Array.get(0)
  ->Belt.Option.flatMap(value => Some(value + 1))

Js.log({
  "stepOneAnswer": stepOneAnswer,
  "stepTwoAnswer": stepTwoAnswer
})