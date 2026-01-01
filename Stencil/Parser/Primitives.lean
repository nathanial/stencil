/-
  Stencil.Parser.Primitives
  Low-level parsing helpers
-/
import Stencil.Parser.State

namespace Stencil.Parser

/-- Skip whitespace characters -/
def skipWhitespace : Parser Unit := do
  while true do
    match ← Parser.peek? with
    | some c =>
      if c == ' ' || c == '\t' || c == '\n' || c == '\r' then
        let _ ← Parser.next
      else
        break
    | none => break

/-- Read characters while predicate holds -/
def readWhile (pred : Char → Bool) : Parser String := do
  let mut result := ""
  while true do
    match ← Parser.peek? with
    | some c =>
      if pred c then
        let _ ← Parser.next
        result := result.push c
      else
        break
    | none => break
  return result

/-- Read at least one character matching predicate -/
def readWhile1 (pred : Char → Bool) (expected : String) : Parser String := do
  let pos ← Parser.getPosition
  let result ← readWhile pred
  if result.isEmpty then
    match ← Parser.peek? with
    | some c => throw (.unexpectedChar pos c expected)
    | none => throw (.unexpectedEnd expected)
  return result

/-- Read until a delimiter string (not consuming the delimiter) -/
def readUntil (stop : String) : Parser String := do
  let mut result := ""
  while true do
    if ← Parser.atEnd then
      break
    let ahead ← Parser.peekString stop.length
    if ahead == stop then
      break
    let c ← Parser.next
    result := result.push c
  return result

/-- Read until a delimiter string and consume it -/
def readUntilAndConsume (stop : String) : Parser String := do
  let result ← readUntil stop
  if !(← Parser.atEnd) then
    let _ ← Parser.tryString stop
  return result

/-- Check if character is alphanumeric or underscore -/
def isIdentChar (c : Char) : Bool :=
  c.isAlpha || c.isDigit || c == '_'

/-- Check if character is valid in a variable path -/
def isPathChar (c : Char) : Bool :=
  c.isAlpha || c.isDigit || c == '_' || c == '.' || c == '@'

/-- Check if character is valid in a filter name -/
def isFilterNameChar (c : Char) : Bool :=
  c.isAlpha || c.isDigit || c == '_'

/-- Try to run a parser, returning None on failure (with backtracking) -/
def tryParse {α : Type} (p : Parser α) : Parser (Option α) := do
  let s ← get
  match (ExceptT.run p).run s with
  | (Except.ok result, s') =>
    set s'
    return some result
  | (Except.error _, _) =>
    return none

/-- Parse a quoted string argument for filters -/
def parseQuotedString : Parser String := do
  let quoteChar ← Parser.next
  if quoteChar != '"' && quoteChar != '\'' then
    let pos ← Parser.getPosition
    throw (.unexpectedChar pos quoteChar "quote character")
  let mut result := ""
  while true do
    match ← Parser.peek? with
    | none => throw (.unexpectedEnd "quoted string")
    | some c =>
      let _ ← Parser.next
      if c == quoteChar then
        break
      else if c == '\\' then
        match ← Parser.peek? with
        | none => throw (.unexpectedEnd "escape sequence")
        | some escaped =>
          let _ ← Parser.next
          let actualChar := match escaped with
            | 'n' => '\n'
            | 't' => '\t'
            | 'r' => '\r'
            | _ => escaped
          result := result.push actualChar
      else
        result := result.push c
  return result

end Stencil.Parser
