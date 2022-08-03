// --- Day 4: Passport Processing ---
// part1

/*
1. passport 타입을 생각해봅시다. *문제와 동일하게* record로 선언하세요.
*/
type passport = {
  byr: string,
  iyr: string,
  eyr: string,
  hgt: string,
  hcl: string,
  ecl: string,
  pid: string,
  cid: option<string>,
}

let eyecolors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

let passportRules: array<(.passport) => bool> = [
  // validate birth
  (.passport) => {
    switch passport.byr->Belt.Int.fromString {
      | Some(birth) => birth >= 1920 && birth <= 2002
      | _ => false
    }
  },
  // validate iyr
  (.passport) => {
    switch passport.iyr->Belt.Int.fromString {
      | Some(iyr) => iyr >= 2010 && iyr <= 2020
      | _ => false
    }
  },
  // validate eyr
  (.passport) => {
    switch passport.eyr->Belt.Int.fromString {
      | Some(eyr) => eyr >= 2020 && eyr <= 2030
      | _ => false
    }
  },
  // validate height
  (.passport) => {
    let value = passport.hgt
      ->Js.String2.substring(
        ~from=0,
        ~to_=passport.hgt->Js.String2.length - 2
      )
      ->Belt.Int.fromString
    let suffix = passport.hgt->Js.String2.substr(~from=-2)
    switch suffix {
      | "cm" => {
        switch value {
          | Some(cm) => cm >= 150 && cm <= 193
          | _ => false
        }
      }
      | "in" => {
        switch value {
          | Some(inch) => inch >= 59 && inch <= 76
          | _ => false
        }
      }
      | _ => false
    }
  },
  // validate hcl
  (.passport) => {
    %re("/^#[0-9a-f]{6}$/")->Js.Re.test_(passport.hcl)
  },
  // validate ecl
  (.passport) => {
    eyecolors
      ->Belt.Array.some(color => passport.ecl == color)
  },
  // validate pid
  (.passport) => {
    %re("/^[0-9]{9}$/")->Js.Re.test_(passport.pid)
  }
]


/*
2. string 타입의 입력을 passport 타입으로 파싱하는 parsePassport 함수를 작성해보세요.
   (우선 parsePassport 타입의 타입 시그니처를 생각해보세요)
*/
let parsePassport = str => {
  let entires = str
    ->Js.String2.splitByRe(%re("/\s/"))
    ->Belt.Array.keepMap(pair => {
      let keyAndValue = pair
        ->Belt.Option.getWithDefault("")
        ->Js.String2.splitAtMost(":", ~limit=2)
      switch (keyAndValue) {
        | [key, value] => Some((key, value))
        | _ => None
      }
    })
    ->Belt.Map.String.fromArray
  let cid = entires->Belt.Map.String.get("cid")
  switch (
    entires->Belt.Map.String.get("byr"),
    entires->Belt.Map.String.get("iyr"),
    entires->Belt.Map.String.get("eyr"),
    entires->Belt.Map.String.get("hgt"),
    entires->Belt.Map.String.get("hcl"),
    entires->Belt.Map.String.get("ecl"),
    entires->Belt.Map.String.get("pid")
  ) {
    | (Some(byr), Some(iyr), Some(eyr), Some(hgt), Some(hcl), Some(ecl), Some(pid)) => {
      Belt.Result.Ok({
        byr,
        iyr,
        eyr,
        hgt,
        hcl,
        ecl,
        pid,
        cid
      })
    }
    | _ => Belt.Result.Error("")
  }
}

let validatePassport = passportResult => {
  switch (passportResult) {
    | Belt.Result.Ok(passport) => {
      passportRules
        ->Belt.Array.every(rule => rule(.passport))
    }
    | _ => false
  }
}

/*
3. 올바른 Passport를 세는 countPassport 함수를 만들어서 문제를 해결해봅시다.
*/
let countPassport = passports => {
  passports
    ->Belt.Array.keepMap(passportResult => {
      switch (passportResult) {
        | Belt.Result.Ok(passport) => Some(passport)
        | _ => None
      }
    })
    ->Belt.Array.length
}

let input = Node.Fs.readFileAsUtf8Sync(Node.Process.cwd() ++ "/rescript/input/Week2/Year2020Day4.input.txt")
let passports = input
  ->Js.String2.split("\n\n")
  ->Belt.Array.map(parsePassport)

let stepOneAnswer = passports->countPassport
// part2
/*
4. part1과 동일하게, *문제를 그대로* 코드로 옮겨보세요.
*/
let stepTwoAnswer = passports
  ->Belt.Array.keep(validatePassport)
  ->Belt.Array.length

/*
참고 링크
- https://rescript-lang.org/docs/manual/latest/record
- https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/
*/
Js.log({
  "stepOneAnswer": stepOneAnswer,
  "stepTwoAnswer": stepTwoAnswer
})