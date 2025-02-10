import Mathlib.Std.Data.HashMap

def getInput : IO String := do
  let handle ← IO.FS.Handle.mk "problem-inputs/day1.input" IO.FS.Mode.read
  IO.FS.Handle.readToEnd handle

def allSomeHelper (xs : List (Option α)) (reversed : List α) : Option (List α) :=
  match xs with
  | [] => Option.some reversed
  | Option.none :: _ => Option.none
  | Option.some x :: xs => allSomeHelper xs (x :: reversed)

def allSome (xs: List (Option α)) : Option (List α) :=
  match allSomeHelper xs [] with
  | Option.some xs => xs.reverse
  | _ => Option.none

def toNums (left : String) (right : String) : Option (Int × Int) :=
  match (left.toNat?, right.toNat?) with
  | (.some x, .some y) => .some (x, y)
  | _ => .none

def parseInput (input : String): Option (List (Int × Int)) :=
  let lines := input.splitOn "\n"
  let columns := lines.map fun line => (line.splitOn " ").filter fun str => not str.isEmpty
  let pairs := columns.map fun
    | [] => Option.none
    | x1 :: x2 :: [] => toNums x1 x2
    | _ => Option.none
  allSome pairs

def inputToArrays (input : List (Int × Int)) : (Array Int) × (Array Int) :=
  input.toArray.unzip

def assertInputIsValid (parsedInput : Option (List α)) : IO (List α) :=
  match parsedInput with
  | .none => IO.Process.exit 1
  | .some a => if a.length = 1000 then return a else IO.Process.exit 1

def solvePartOne (input : (Array Int) × (Array Int)) : Int :=
  let first := input.fst.insertionSort fun a b => a ≤ b
  let second := input.snd.insertionSort fun a b => a ≤ b
  let rezipped := Array.zip first second
  rezipped.foldl (fun acc x => (acc + Int.natAbs (x.fst - x.snd))) 0

def countOccurrances (arr: Array Int) (x : Int) : Int :=
  arr.foldl (fun acc y => if x = y then acc + 1 else acc) 0

def countArrayEntries (arr: Array Int) : Std.HashMap Int Int :=
  arr.foldl (fun acc x => match acc[x]? with
  | .some _ => acc
  | .none => acc.insert x (countOccurrances arr x)) Std.HashMap.empty

def solvePartTwo (input : (Array Int) × (Array Int)): Int :=
  let arr := input.fst
  let frequencies := countArrayEntries input.snd
  arr.foldl (fun acc x => acc + x * (frequencies[x]?.getD 0)) 0

def main : IO Unit := do
  let fileContent ← getInput
  let parsed := (parseInput fileContent)
  let parsedGood ← assertInputIsValid parsed
  let arrays := inputToArrays parsedGood
  IO.println (solvePartOne arrays)
  IO.println (solvePartTwo arrays)
