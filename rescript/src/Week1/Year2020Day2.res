type passwordRule = {
  min: int,
  max: int,
  required: string,
  password: string
}

let parsePasswordRule = (str): option<passwordRule> => {
  switch str->Js.String2.split(" ") {
    | [rangeParts, requiredParts, password] => {
      switch (
        rangeParts->Js.String2.split("-")->Belt.Array.map(Belt.Int.fromString),
        requiredParts->Js.String2.split(":")->Belt.Array.get(0)
      ) {
        | ([Some(min), Some(max)], Some(required)) => Some({
          min,
          max,
          required,
          password
        })
        | _ => None
      }
    }
    | _ => None
  }
}

let validatePasswordOldRule = (rule) => {
  let { password, min, max, required } = rule
  let sizeOfRequired = password
    ->Js.String2.split("")
    ->Belt.Array.keep(char => char == required)
    ->Belt.Array.length
  sizeOfRequired >= min && sizeOfRequired <= max
}

let validatePasswordNewRule = (rule) => {
  let { password, min, max, required } = rule
  let matchMinRequired = password->Js.String2.charAt(min - 1) === required
  let matchMaxRequired = password->Js.String2.charAt(max - 1) === required
  let validate = (matchMinRequired || matchMaxRequired) && !(matchMinRequired && matchMaxRequired)
  validate
}

// Comment
let input = Node.Fs.readFileAsUtf8Sync(Node.Process.cwd() ++ "/rescript/input/Week1/Year2020Day2.input.txt")
let passwordRules = input
  ->Js.String2.split("\n")
  ->Belt.Array.keepMap(parsePasswordRule)

let stepOneAnswer = passwordRules
  ->Belt.Array.keep(validatePasswordOldRule)
  ->Belt.Array.length

let stepTwoAnswer = passwordRules
  ->Belt.Array.keep(validatePasswordNewRule)
  ->Belt.Array.length

Js.log({
  "stepOneAnswer": stepOneAnswer,
  "stepTwoAnswer": stepTwoAnswer
})