let tree = "#"

let countTreeByDirection = (
  ~map: array<array<string>>, ~x: int, ~y: int
) => {
  let yPositions = Belt.Array.rangeBy(y, map->Belt.Array.length - 1, ~step=y)
  let positions = Belt.Array.zip(
    Belt.Array.makeBy(
      yPositions->Belt.Array.length,
      i => mod((i + 1) * x, map[0]->Belt.Array.length)
    ),
    yPositions
  )
  let treeCount = positions
    ->Belt.Array.map(((x, y)) => map[y][x] == tree ? 1 : 0)
    ->Belt.Array.reduce(0, (result, num) => result + num)
  treeCount
}

/* Comment */
let input = Node.Fs.readFileAsUtf8Sync(Node.Process.cwd() ++ "/rescript/input/Week1/Year2020Day3.input.txt")
let map = input
    ->Js.String2.trim
    ->Js.String2.split("\n")
    ->Belt.Array.map(line => line->Js.String2.split(""))

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
  ->Belt.Array.map(({ x, y }) => countTreeByDirection(~map, ~x, ~y)->Belt.Int.toFloat)
  ->Belt.Array.reduce(
    1.0,
    (result, count) => result *. count,
  )

Js.log({
  "stepOneAnswer": stepOneAnswer,
  "stepTwoAnswer": stepTwoAnswer
})
