/-
  Stencil.Parser.Primitives
  Low-level parsing helpers using Sift
-/
import Stencil.Parser.State

namespace Stencil.Parser

open Sift

/-- Skip whitespace characters -/
def skipWhitespace : Parser Unit :=
  skipMany (satisfy fun c => c == ' ' || c == '\t' || c == '\n' || c == '\r')

/-- Read characters while predicate holds -/
def readWhile (pred : Char → Bool) : Parser String :=
  manyChars (satisfy pred)

/-- Read at least one character matching predicate -/
def readWhile1 (pred : Char → Bool) (expected : String) : Parser String := do
  let result ← manyChars (satisfy pred)
  if result.isEmpty then
    Sift.Parser.fail s!"expected {expected}"
  pure result

/-- Read until a delimiter string (not consuming the delimiter) -/
partial def readUntil (stop : String) : Parser String := do
  let s ← Sift.Parser.get
  let startPos := s.pos
  let rec loop : Parser Unit := do
    if ← Parser.atEnd then pure ()
    else
      let ahead ← Parser.peekString stop.length
      if ahead == stop then pure ()
      else do let _ ← anyChar; loop
  loop
  let s' ← Sift.Parser.get
  pure (s.input.extract ⟨startPos⟩ ⟨s'.pos⟩)

/-- Read until a delimiter string and consume it -/
def readUntilAndConsume (stop : String) : Parser String := do
  let result ← readUntil stop
  if !(← Parser.atEnd) then
    let _ ← Parser.tryString stop
  pure result

/-- Check if character is alphanumeric or underscore -/
def isIdentChar (c : Char) : Bool :=
  c.isAlpha || c.isDigit || c == '_'

/-- Check if character is valid in a partial/template name (allows path separators) -/
def isPartialNameChar (c : Char) : Bool :=
  c.isAlpha || c.isDigit || c == '_' || c == '/' || c == '-'

/-- Check if character is valid in a variable path -/
def isPathChar (c : Char) : Bool :=
  c.isAlpha || c.isDigit || c == '_' || c == '.' || c == '@'

/-- Check if character is valid in a filter name -/
def isFilterNameChar (c : Char) : Bool :=
  c.isAlpha || c.isDigit || c == '_'

/-- Try to run a parser, returning None on failure (with backtracking) -/
def tryParse {α : Type} (p : Parser α) : Parser (Option α) :=
  Sift.optional (attempt p)

/-- Parse a quoted string argument for filters -/
partial def parseQuotedString : Parser String := do
  let quoteChar ← anyChar
  if quoteChar != '"' && quoteChar != '\'' then
    Sift.Parser.fail "expected quote character"
  let mut result := ""
  while true do
    match ← peek with
    | none => Sift.Parser.fail "unexpected end in quoted string"
    | some c =>
      let _ ← anyChar
      if c == quoteChar then
        break
      else if c == '\\' then
        match ← peek with
        | none => Sift.Parser.fail "unexpected end in escape sequence"
        | some escaped =>
          let _ ← anyChar
          let actualChar := match escaped with
            | 'n' => '\n'
            | 't' => '\t'
            | 'r' => '\r'
            | _ => escaped
          result := result.push actualChar
      else
        result := result.push c
  pure result

end Stencil.Parser
