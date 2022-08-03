type passwordRule = {
  min: int,
  max: int,
  required: string,
  password: string
}

let parsePasswordRule = (str) => {
  let parts = str->Js.String2.split(" ")
  let rangeInt = parts
    ->Belt.Array.get(0)
    ->Belt.Option.mapWithDefault([], range => {
      range
        ->Js.String2.split("-")
        ->Belt.Array.map(Belt.Int.fromString)
    })
  let required = parts
    ->Belt.Array.get(1)
    ->Belt.Option.mapWithDefault(None, rule => {
      rule
        ->Js.String2.split(":")
        ->Belt.Array.get(0)
    })
  let password = parts->Belt.Array.get(2)
  let min = rangeInt->Belt.Array.get(0)
  let max = rangeInt->Belt.Array.get(1)
  switch (min, max, required, password) {
    | (Some(Some(min)), Some(Some(max)), Some(required), Some(password)) => Belt.Result.Ok({
      min,
      max,
      required,
      password
    })
    | _ => Belt.Result.Error("Invalid password rule") 
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
  ->Belt.Array.map(parsePasswordRule)
  ->Belt.Array.keepMap(result => {
    switch result {
      | Belt.Result.Ok(rule) => Some(rule)
      | _ => None
    }
  })

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