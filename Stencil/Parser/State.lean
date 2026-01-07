/-
  Stencil.Parser.State
  Parser state and monad definition using Sift parser combinators
-/
import Sift
import Stencil.Core.Position

namespace Stencil.Parser

open Stencil (Position)

/-- Stencil-specific parser state (carried as Sift user state) -/
structure StencilState where
  tagStack : List String := []
  trimNextLeading : Bool := false  -- For whitespace control: trim leading whitespace from next text
  deriving Repr

/-- Parser monad using Sift with StencilState as user state -/
abbrev Parser := Sift.Parser StencilState

namespace Parser

open Sift

/-- Get current position as Position struct -/
def getPosition : Parser Position := Sift.Parser.position

/-- Check if at end of input -/
def atEnd : Parser Bool := Sift.atEnd

/-- Peek at current character without consuming -/
def peek? : Parser (Option Char) := Sift.peek

/-- Peek at current character, error if at end -/
def peekOrFail : Parser Char := do
  match ← Sift.peek with
  | some c => pure c
  | none => Sift.Parser.fail "unexpected end of input"

/-- Consume and return current character -/
def next : Parser Char := Sift.anyChar

/-- Try to consume a specific character -/
def tryChar (c : Char) : Parser Bool := do
  match ← Sift.peek with
  | some x =>
    if x == c then
      let _ ← Sift.anyChar
      pure true
    else
      pure false
  | none => pure false

/-- Expect and consume a specific character -/
def expect (expected : Char) : Parser Unit := do
  let c ← Sift.anyChar
  if c != expected then
    Sift.Parser.fail s!"expected '{expected}', got '{c}'"

/-- Expect and consume a specific string -/
def expectString (expected : String) : Parser Unit := do
  for c in expected.toList do
    expect c

/-- Peek ahead n characters without consuming -/
def peekString (n : Nat) : Parser String := do
  let s ← Sift.Parser.get
  let input := s.input
  let pos := s.pos
  let mut endPos := pos
  let mut count := 0
  while count < n && endPos < input.length do
    endPos := endPos + 1
    count := count + 1
  pure (String.Pos.Raw.extract input ⟨pos⟩ ⟨endPos⟩)

/-- Try to match and consume a string -/
def tryString (expected : String) : Parser Bool := do
  let ahead ← peekString expected.length
  if ahead == expected then
    for _ in expected.toList do
      let _ ← Sift.anyChar
    pure true
  else
    pure false

/-- Push a tag onto the open tag stack -/
def pushTag (tag : String) : Parser Unit :=
  Sift.Parser.modifyUserState fun s => { s with tagStack := tag :: s.tagStack }

/-- Pop a tag from the stack, returning it -/
def popTag : Parser (Option String) := do
  let st ← Sift.Parser.getUserState
  match st.tagStack with
  | [] => pure none
  | t :: rest =>
    Sift.Parser.setUserState { st with tagStack := rest }
    pure (some t)

/-- Peek at the current open tag -/
def currentTag : Parser (Option String) := do
  let st ← Sift.Parser.getUserState
  pure st.tagStack.head?

/-- Get the trimNextLeading flag -/
def getTrimNext : Parser Bool := do
  let st ← Sift.Parser.getUserState
  pure st.trimNextLeading

/-- Set the trimNextLeading flag -/
def setTrimNext (v : Bool) : Parser Unit :=
  Sift.Parser.modifyUserState fun s => { s with trimNextLeading := v }

/-- Run parser on input, returning result -/
def run {α : Type} (p : Parser α) (input : String) : Except Sift.ParseError α :=
  Sift.Parser.runWith p input {}

/-- Run parser on input, returning result and final state -/
def runWithState {α : Type} (p : Parser α) (input : String) : Except Sift.ParseError α × Sift.ParseState StencilState :=
  -- Note: this returns a different result type than before
  let init : Sift.ParseState StencilState := Sift.ParseState.init input {}
  match p init with
  | .ok (a, s) => (.ok a, s)
  | .error e => (.error e, init)

end Parser

end Stencil.Parser
