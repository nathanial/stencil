/-
  Stencil.Parser.State
  Parser state and monad definition
-/
import Stencil.Core.Error

namespace Stencil.Parser

/-- Parser state tracking position in template input -/
structure ParserState where
  input : String
  pos : Nat := 0
  line : Nat := 1
  column : Nat := 1
  tagStack : List String := []
  trimNextLeading : Bool := false  -- For whitespace control: trim leading whitespace from next text
  deriving Repr

/-- Parser monad combining state and error handling -/
abbrev Parser := ExceptT ParseError (StateM ParserState)

namespace Parser

/-- Get current position as Position struct -/
def getPosition : Parser Position := do
  let s ← get
  return { offset := s.pos, line := s.line, column := s.column }

/-- Check if at end of input -/
def atEnd : Parser Bool := do
  let s ← get
  return s.input.atEnd ⟨s.pos⟩

/-- Peek at current character without consuming -/
def peek? : Parser (Option Char) := do
  let s ← get
  let p : String.Pos := ⟨s.pos⟩
  if s.input.atEnd p then
    return none
  else
    return some (s.input.get p)

/-- Peek at current character, error if at end -/
def peek : Parser Char := do
  match ← peek? with
  | some c => return c
  | none => throw (.unexpectedEnd "input")

/-- Consume and return current character, updating line/column -/
def next : Parser Char := do
  let s ← get
  let p : String.Pos := ⟨s.pos⟩
  if s.input.atEnd p then
    throw (.unexpectedEnd "input")
  let c := s.input.get p
  let nextP := s.input.next p  -- Correctly advances by char's byte length
  let (newLine, newCol) :=
    if c == '\n' then (s.line + 1, 1)
    else (s.line, s.column + 1)
  set { s with pos := nextP.byteIdx, line := newLine, column := newCol }
  return c

/-- Try to consume a specific character -/
def tryChar (c : Char) : Parser Bool := do
  match ← peek? with
  | some x =>
    if x == c then
      let _ ← next
      return true
    else
      return false
  | none => return false

/-- Expect and consume a specific character -/
def expect (expected : Char) : Parser Unit := do
  let pos ← getPosition
  let c ← next
  if c != expected then
    throw (.unexpectedChar pos c s!"'{expected}'")

/-- Expect and consume a specific string -/
def expectString (expected : String) : Parser Unit := do
  for c in expected.toList do
    expect c

/-- Peek ahead n characters without consuming -/
def peekString (n : Nat) : Parser String := do
  let s ← get
  let mut p : String.Pos := ⟨s.pos⟩
  for _ in [:n] do
    if s.input.atEnd p then break
    p := s.input.next p
  return s.input.extract ⟨s.pos⟩ p

/-- Try to match and consume a string -/
def tryString (expected : String) : Parser Bool := do
  let ahead ← peekString expected.length
  if ahead == expected then
    for _ in expected.toList do
      let _ ← next
    return true
  else
    return false

/-- Push a tag onto the open tag stack -/
def pushTag (tag : String) : Parser Unit := do
  modify fun s => { s with tagStack := tag :: s.tagStack }

/-- Pop a tag from the stack, returning it -/
def popTag : Parser (Option String) := do
  let s ← get
  match s.tagStack with
  | [] => return none
  | t :: rest =>
    set { s with tagStack := rest }
    return some t

/-- Peek at the current open tag -/
def currentTag : Parser (Option String) := do
  let s ← get
  return s.tagStack.head?

/-- Get the trimNextLeading flag -/
def getTrimNext : Parser Bool := do
  let s ← get
  return s.trimNextLeading

/-- Set the trimNextLeading flag -/
def setTrimNext (v : Bool) : Parser Unit := do
  modify fun s => { s with trimNextLeading := v }

/-- Run parser on input, returning result -/
def run {α : Type} (p : Parser α) (input : String) : ParseResult α :=
  let initState : ParserState := { input := input }
  let (result, _) := (ExceptT.run p).run initState
  result

/-- Run parser on input, returning result and final state -/
def runWithState {α : Type} (p : Parser α) (input : String) : ParseResult α × ParserState :=
  let initState : ParserState := { input := input }
  (ExceptT.run p).run initState

end Parser

end Stencil.Parser
