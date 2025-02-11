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

def isSafe (xs : Array Int) : Bool :=
  isSafeIncreasing xs || isSafeDecreasing xs

def solvePartOne (xss : Array (Array Int)) : Int :=
  (xss.filter isSafe).size

def iterateOmit (xs : Array α) : (Array (Array α)) :=
  if h: xs.size = 0 then #[] else
  iterateOmitHelper 0 #[]
where
  iterateOmitHelper (i : Nat) (arrs : Array (Array α)) :=
    if h: i ≥ xs.size then arrs else
    iterateOmitHelper (i + 1) (arrs.push (xs.eraseIdx i))
  termination_by xs.size - i

def isSafeIncreasingDampened (xs : Array Int) : Bool :=
  let withOmissions := (iterateOmit xs).map isSafeIncreasing
  withOmissions.any id

def isSafeDecreasingDampened (xs : Array Int) : Bool :=
  let withOmissions := (iterateOmit xs).map isSafeDecreasing
  withOmissions.any id

def isSafeDampened (xs : Array Int) : Bool :=
  isSafeIncreasingDampened xs || isSafeDecreasingDampened xs

def solvePartTwo (xss : Array (Array Int)) : Int :=
  (xss.filter fun xs => isSafe xs || isSafeDampened xs).size

def main : IO Unit := do
  let input ← getInput
  let parsed := parseDay2 input
  match parsed with
  | .none => IO.Process.exit 1
  | .some xss =>
    IO.println (solvePartOne xss)
    IO.println (solvePartTwo xss)
