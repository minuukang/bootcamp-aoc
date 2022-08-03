type passwordRule = {
  min: option<int>,
  max: option<int>,
  required: option<string>,
  password: option<string>,
}

type passwordSafeRule = {
  min: int,
  max: int,
  required: string,
  password: string
}

let parsePasswordRule = (str): passwordRule => {
  // [range, required, password]
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
  {
    min: rangeInt->Belt.Array.getUnsafe(0),
    max: rangeInt->Belt.Array.getUnsafe(1),
    required,
    password,
  }
}

let getSafePasswordRule = (rule: passwordRule): passwordSafeRule => {
  let min = switch rule.min {
    | Some(m) => m
    | None => Js.Exn.raiseTypeError("min cannot be optional")
  }
  let max = switch rule.max {
    | Some(m) => m
    | None => Js.Exn.raiseTypeError("max cannot be optional")
  }
  let required = switch rule.required {
    | Some(m) => m
    | None => Js.Exn.raiseTypeError("required cannot be optional")
  }
  let password = switch rule.password {
    | Some(m) => m
    | None => Js.Exn.raiseTypeError("required cannot be optional")
  }
  {
    min,
    max,
    required,
    password
  }
}

let validatePasswordOldRule = (rule: passwordRule) => {
  let { password, min, max, required } = rule->getSafePasswordRule
  let sizeOfRequired = password
    ->Js.String2.split("")
    ->Belt.Array.keep(char => char == required)
    ->Belt.Array.length
  sizeOfRequired >= min && sizeOfRequired <= max
}

let validatePasswordNewRule = (rule: passwordRule) => {
  let { password, min, max, required } = rule->getSafePasswordRule
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