import Day3.Parser

def getInput : IO String := do
  let handle ← IO.FS.Handle.mk "problem-inputs/day3.input" IO.FS.Mode.read
  IO.FS.Handle.readToEnd handle

def computePartOne (input : Array (Option (Int × Int))) : Int :=
  let multiplied := input.map fun
    | .none => 0
    | .some ⟨x, y⟩ => x * y
  multiplied.foldl (. + .) 0

def computePartTwo (input : Array Command) : Int :=
  let ⟨_, total⟩ := input.foldl (fun acc command => match command with
  | .nothing => acc
  | .enable => ⟨true, acc.snd⟩
  | .disable => ⟨false, acc.snd⟩
  | .mul x y => if acc.fst then ⟨true, acc.snd + (x * y)⟩ else acc
  ) (Prod.mk true 0)
  total

def main : IO Unit := do
  let input ← getInput
  match parsePart1 input with
  | .none => IO.Process.exit 1
  | .some arr =>
    IO.println (computePartOne arr)
    match parsePart2 input with
    | .none => IO.Process.exit 1
    | .some arr =>
      IO.println (computePartTwo arr)
