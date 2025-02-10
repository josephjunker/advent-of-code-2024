import Day2.Parser

def getInput : IO String := do
  let handle ← IO.FS.Handle.mk "problem-inputs/day2.input" IO.FS.Mode.read
  IO.FS.Handle.readToEnd handle

def windowFoldl2 (xs : Array α) (fn : β → α → α → β) (acc : β) : β :=
  if h: xs.size ≤ 1 then acc else
    windowFoldl2Helper 0 (by omega) acc
where
  windowFoldl2Helper (i : Nat) (hi: i < xs.size - 1) (acc: β) : β :=
    let thisStep := fn acc xs[i] xs[i.succ]
    if h: i + 1 < xs.size - 1 then
      windowFoldl2Helper (i + 1) h thisStep else thisStep

def isSafeIncreasing (xs : Array Int) : Bool :=
  windowFoldl2 xs (fun acc x y => acc && (y > x) && (y - x <= 3)) true

def isSafeDecreasing (xs : Array Int) : Bool :=
  windowFoldl2 xs (fun acc x y => acc && (x > y) && (x - y <= 3)) true

def solvePartOne (xss : Array (Array Int)) : Int :=
  (xss.filter fun xs => (isSafeIncreasing xs) || (isSafeDecreasing xs)).size


def main : IO Unit := do
  let input ← getInput
  let parsed := parseDay2 input
  match parsed with
  | .none => IO.Process.exit 1
  | .some xss =>
    IO.println (solvePartOne xss)
