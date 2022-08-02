let tree = "#"

let countTreeByDirection = (
  ~map: array<array<string>>, ~x: int, ~y: int
) => {
  let yPositions = Belt.Array.rangeBy(y, map->Js.Array2.length - 1, ~step=y)
  let positions = Belt.Array.zip(
    Belt.Array.makeBy(
      yPositions->Js.Array2.length,
      i => mod((i + 1) * x, map[0]->Js.Array2.length)
    ),
    yPositions
  )
  let treeCount = positions
    ->Js.Array2.map(((x, y)) => map[y][x] == tree ? 1 : 0)
    ->Js.Array2.reduce((result, num) => result + num, 0)
  treeCount
}

/* Comment */
let input = Node.Fs.readFileAsUtf8Sync(Node.Process.cwd() ++ "/rescript/input/Week1/Year2020Day3.input.txt")
let map = input
    ->Js.String2.trim
    ->Js.String2.split("\n")
    ->Js.Array2.map(line => line->Js.String2.split(""))

let stepOneAnswer = countTreeByDirection(
  ~map,
  ~x=3,
  ~y=1,
);

type direction = { x: int, y: int }
let stepTwoDirections: array<direction> = [
  { x: 1, y: 1 },
  { x: 3, y: 1 },
  { x: 5, y: 1 },
  { x: 7, y: 1 },
  { x: 1, y: 2 }
]
let stepTwoAnswer = stepTwoDirections
  ->Js.Array2.map(({ x, y }) => countTreeByDirection(~map, ~x, ~y)->Belt.Int.toFloat)
  ->Js.Array2.reduce(
    (result, count) => result *. count,
    1.0
  )

Js.log({
  "stepOneAnswer": stepOneAnswer,
  "stepTwoAnswer": stepTwoAnswer
})
