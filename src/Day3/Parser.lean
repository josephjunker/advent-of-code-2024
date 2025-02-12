import Parser

open Parser Char

inductive Command where
  | mul (x y : Int) : Command
  | enable : Command
  | disable : Command
  | nothing : Command
deriving Repr

namespace Day3Parser

protected abbrev Parser := Parser Unit Substring Char

protected def threeDigitNumber : Day3Parser.Parser Int :=
  let asString := (takeUpTo 3 ASCII.numeric)
  asString >>= fun arr =>
    if arr.size = 0
      then throwUnexpected
      else return String.toInt! (String.join ((arr.map String.singleton).toList))

protected def parseMul : Day3Parser.Parser (Int × Int) := do
  let _ ← string "mul("
  let left ← Day3Parser.threeDigitNumber
  let _ ← string ","
  let right ← Day3Parser.threeDigitNumber
  let _ ← string ")"
  return ⟨ left, right ⟩

protected def parsePart1 : Day3Parser.Parser (Array (Option (Int × Int))) :=
  let one := first [
    (Option.some .) <$> Day3Parser.parseMul,
    anyToken *> pure Option.none
  ]
  (Array.filter Option.isSome .) <$> takeMany one

protected def parsePart2 : Day3Parser.Parser (Array Command) :=
  let one := first [
    (fun ⟨x, y⟩ => Command.mul x y) <$> Day3Parser.parseMul,
    string "don't" *> pure Command.disable,
    string "do" *> pure Command.enable,
    anyToken *> pure Command.nothing
  ]
  takeMany one

end Day3Parser

def parsePart1 (s : String) : Option (Array (Option (Int × Int))) :=
  match Parser.run (Day3Parser.parsePart1 <* Parser.endOfInput) s with
  | .ok _ res => some res
  | _ => none

def parsePart2 (s : String) : Option (Array Command) :=
  match Parser.run (Day3Parser.parsePart2 <* Parser.endOfInput) s with
  | .ok _ res => some res
  | _ => none
