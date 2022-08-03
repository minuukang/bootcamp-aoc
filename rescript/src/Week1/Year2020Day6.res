let parseSurveyGroup = response => response->Js.String2.split("\n\n")
let parseSurveyPerson = group => group->Js.String2.split("\n")
let parseSurveyAnswers = person => 
  person
    ->Js.String2.split("")
    ->Belt.Set.String.fromArray

let parseSurveyResponse = response => {
  response
    ->parseSurveyGroup
    ->Belt.Array.map(group => {
      group
        ->parseSurveyPerson
        ->Belt.Array.map(parseSurveyAnswers)
    })
}

let input = Node.Fs.readFileAsUtf8Sync(Node.Process.cwd() ++ "/rescript/input/Week1/Year2020Day6.input.txt")
let answerGroup = input->parseSurveyResponse

let stepOneAnswer = answerGroup
  ->Belt.Array.map(answers => {
    answers
      ->Belt.Array.reduce(
        Belt.Set.String.empty,
        (result, answer) => result->Belt.Set.String.union(answer)
      )
      ->Belt.Set.String.size
  })
  ->Belt.Array.reduce(0, (res, len) => res + len)

let stepTwoAnswer = answerGroup
  ->Belt.Array.map(answers => {
    answers
      ->Belt.Array.reduce(
        answers[0],
        (result, answer) => result->Belt.Set.String.intersect(answer)
      )
      ->Belt.Set.String.size
  })
  ->Belt.Array.reduce(0, (res, len) => res + len)

Js.log({
  "stepOneAnswer": stepOneAnswer,
  "stepTwoAnswer": stepTwoAnswer
})