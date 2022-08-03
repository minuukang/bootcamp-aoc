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

type seatDirection = Lower | Upper
type seatSpec = Column(seatDirection) | Row(seatDirection)

let parseSpec = char => {
  switch char {
    | "F" => Some(Row(Lower))
    | "B" => Some(Row(Upper))
    | "L" => Some(Column(Lower))
    | "R" => Some(Column(Upper))
    | _ => None
  }
}

let createSeatParser = (~min: int, ~max: int) => 
  (seatRows: array<seatDirection>) => {
    let {min, max} = seatRows
      ->Belt.Array.reduce(
        { min, max },
        (result, seatRow) => {
          switch (seatRow) {
            | Lower => { min: result.min, max: Js.Math.floor_int((result.max + result.min)->Belt.Int.toFloat /. 2.0) }
            | Upper => { min: Js.Math.ceil_int((result.max + result.min)->Belt.Int.toFloat /. 2.0), max: result.max }
          }
        }
      )
    if min != max {
      Js.Exn.raiseError("min & max should be equal")
    }
    min
  }

let parseRow = createSeatParser(~min=0, ~max=maxRowNumber)
let parseColumn = createSeatParser(~min=0, ~max=maxColumnNumber)
let getSeatId = (~row, ~column) => row * seatIdSpecificNumber + column

let parseSeat = (seatStr: string) => {
  let seatSpecs = seatStr
    ->Js.String2.split("")
    ->Belt.Array.keepMap(parseSpec)
  let row = parseRow(seatSpecs->Belt.Array.keepMap(seat => switch seat {
    | Row(direction) => Some(direction)
    | _ => None
  }))
  let column = parseColumn(seatSpecs->Belt.Array.keepMap(seat => switch seat {
    | Column(direction) => Some(direction)
    | _ => None
  }))
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
  ->Belt.Array.keepWithIndex((id, index) => {
    inputSeatIds
      ->Belt.Array.get(index + 1)
      ->Belt.Option.mapWithDefault(false, value => value == id + 2)
  })
  ->Belt.Array.get(0)
  ->Belt.Option.flatMap(value => Some(value + 1))

Js.log({
  "stepOneAnswer": stepOneAnswer,
  "stepTwoAnswer": stepTwoAnswer
})