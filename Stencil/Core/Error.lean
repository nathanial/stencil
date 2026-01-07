/-
  Stencil.Core.Error
  Parse and render error types with source context and suggestions
-/
import Sift
import Stencil.Core.Position
import Stencil.Core.Suggest

namespace Stencil

/-- Errors that can occur during template rendering -/
inductive RenderError where
  | unknownVariable (path : String) (pos : Option Position) (suggestion : Option String)
  | unknownPartial (name : String) (pos : Option Position)
  | unknownFilter (name : String) (pos : Option Position) (suggestion : Option String)
  | filterError (filter : String) (msg : String) (pos : Option Position)
  | typeError (context : String) (expected : String) (got : String) (pos : Option Position)
  | other (msg : String)
  deriving Repr, BEq, Inhabited

instance : ToString RenderError where
  toString
    | .unknownVariable path pos suggestion =>
      let posStr := match pos with | some p => s!" at {p}" | none => ""
      let hint := match suggestion with | some s => s!"\n  Hint: Did you mean '{s}'?" | none => ""
      s!"Render error{posStr}: unknown variable '{path}'{hint}"
    | .unknownPartial name pos =>
      let posStr := match pos with | some p => s!" at {p}" | none => ""
      s!"Render error{posStr}: unknown partial '{name}'"
    | .unknownFilter name pos suggestion =>
      let posStr := match pos with | some p => s!" at {p}" | none => ""
      let hint := match suggestion with | some s => s!"\n  Hint: Did you mean '{s}'?" | none => ""
      s!"Render error{posStr}: unknown filter '{name}'{hint}"
    | .filterError filter msg pos =>
      let posStr := match pos with | some p => s!" at {p}" | none => ""
      s!"Render error{posStr}: filter '{filter}' failed: {msg}"
    | .typeError ctx exp got pos =>
      let posStr := match pos with | some p => s!" at {p}" | none => ""
      s!"Render error{posStr} in {ctx}: expected {exp}, got {got}"
    | .other msg => s!"Render error: {msg}"

/-- Result type for parsing (uses Sift.ParseError) -/
abbrev ParseResult (α : Type) := Except Sift.ParseError α

/-- Result type for rendering -/
abbrev RenderResult (α : Type) := Except RenderError α

-- Source context extraction

/-- Get lines from a string -/
private def getLines (input : String) : Array String :=
  input.splitOn "\n" |>.toArray

/-- Extract source context around a position -/
def sourceContext (input : String) (pos : Position) (contextLines : Nat := 1) : String :=
  let lines := getLines input
  let lineIdx := pos.line - 1
  if h : lineIdx < lines.size then
    let startIdx := if lineIdx >= contextLines then lineIdx - contextLines else 0
    let endIdx := Nat.min (lineIdx + contextLines + 1) lines.size
    -- Build list of indices to process
    let indices := List.range (endIdx - startIdx) |>.map (· + startIdx)
    -- Fold over indices to build result
    let result := indices.foldl (init := "") fun acc i =>
      if h2 : i < lines.size then
        let lineNum := i + 1
        let numStr := toString lineNum
        let padding := String.mk (List.replicate (4 - numStr.length) ' ')
        let linePrefix := s!"{padding}{lineNum} | "
        let line := acc ++ linePrefix ++ lines[i] ++ "\n"
        -- Add caret indicator on the error line
        if i == lineIdx then
          let caretPad := String.mk (List.replicate (linePrefix.length + pos.column - 1) ' ')
          line ++ caretPad ++ "^\n"
        else
          line
      else
        acc
    result.dropRight 1  -- Remove trailing newline
  else
    ""

/-- Format a parse error with source context -/
def formatParseError (err : Sift.ParseError) (input : String) : String :=
  let pos := err.pos
  let ctx := sourceContext input pos
  let errMsg := toString err
  if ctx.isEmpty then errMsg
  else s!"{errMsg}\n\n{ctx}"

/-- Format a render error with source context -/
def RenderError.format (err : RenderError) (input : String) : String :=
  let getPos : Option Position := match err with
    | .unknownVariable _ pos _ => pos
    | .unknownPartial _ pos => pos
    | .unknownFilter _ pos _ => pos
    | .filterError _ _ pos => pos
    | .typeError _ _ _ pos => pos
    | .other _ => none
  match getPos with
  | some pos =>
    let ctx := sourceContext input pos
    let errMsg := toString err
    if ctx.isEmpty then errMsg
    else s!"{errMsg}\n\n{ctx}"
  | none => toString err

end Stencil
