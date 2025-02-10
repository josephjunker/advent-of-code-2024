import Parser

open Parser Char

namespace Day2Parser

protected abbrev Parser := Parser Unit Substring Char

protected def line : Day2Parser.Parser (Array Int) :=
  sepBy space ASCII.parseInt

protected def lines : Day2Parser.Parser (Array (Array Int)) :=
  sepBy eol Day2Parser.line

end Day2Parser

def parseDay2 (s: String) : Option (Array (Array Int)) :=
  match Parser.run (Day2Parser.lines <* Parser.endOfInput) s with
  | .ok _ res => some res
  | _ => none
