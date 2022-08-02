let maxColumnNumber = 7.0
let maxRowNumber = 127.0
let seatIdSpecificNumber = 8

type minmax = {
  min: float,
  max: float,
}

type seat = {
  id: int,
  row: int,
  column: int
}

let createSeatParser = (~lowerSpec: string, ~upperSpec: string, ~min: float, ~max: float) => 
  (seatRows: array<string>) => {
    let {min, max} = seatRows
      ->Js.Array2.reduce(
        (result, seatRow) => {
          if (seatRow == lowerSpec) {
            { min: result.min, max: Js.Math.floor_float((result.max +. result.min) /. 2.0) }
          } else if (seatRow == upperSpec) {
            { min: Js.Math.ceil_float((result.max +. result.min) /. 2.0), max: result.max }
          } else {
            Js.Exn.raiseError(`seat row can be '${lowerSpec}' or '${upperSpec}'`)
          }
        },
        { min, max }
      )
    if min != max {
      Js.Exn.raiseError("min & max should be equal")
    }
    min->Belt.Int.fromFloat
  }

let parseRow = createSeatParser(~lowerSpec="F", ~upperSpec="B", ~min=0.0, ~max=maxRowNumber)
let parseColumn = createSeatParser(~lowerSpec="L", ~upperSpec="R", ~min=0.0, ~max=maxColumnNumber)
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
  ->Js.Array2.map(parseSeat)

let inputSeatIds = inputSeats
  ->Js.Array2.map(seat => seat.id)
  ->Js.Array2.sortInPlace

let stepOneAnswer = inputSeatIds->Js.Math.maxMany_int
let stepTwoAnswer = inputSeatIds
  ->Js.Array2.findi((id, index) => inputSeatIds[index + 1] == id + 2)
  ->Belt.Option.flatMap(value => Some(value + 1))

Js.log({
  "stepOneAnswer": stepOneAnswer,
  "stepTwoAnswer": stepTwoAnswer
})