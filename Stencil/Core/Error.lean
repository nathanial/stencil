/-
  Stencil.Core.Error
  Parse and render error types
-/
import Stencil.Core.Position

namespace Stencil

/-- Errors that can occur during template parsing -/
inductive ParseError where
  | unexpectedChar (pos : Position) (char : Char) (expected : String)
  | unexpectedEnd (context : String)
  | unmatchedTag (pos : Position) (found : String) (expected : Option String)
  | invalidTagSyntax (pos : Position) (msg : String)
  | invalidFilter (pos : Position) (name : String)
  | emptyPath (pos : Position)
  | other (pos : Position) (msg : String)
  deriving Repr, BEq

instance : ToString ParseError where
  toString
    | .unexpectedChar pos c exp => s!"Parse error at {pos}: unexpected '{c}', expected {exp}"
    | .unexpectedEnd ctx => s!"Parse error: unexpected end of input in {ctx}"
    | .unmatchedTag pos found exp =>
      let lb := "{{"
      let rb := "}}"
      match exp with
      | some e => s!"Parse error at {pos}: unmatched tag '{lb}/{found}{rb}', expected '{lb}/{e}{rb}'"
      | none => s!"Parse error at {pos}: unexpected closing tag '{lb}/{found}{rb}'"
    | .invalidTagSyntax pos msg => s!"Parse error at {pos}: {msg}"
    | .invalidFilter pos name => s!"Parse error at {pos}: invalid filter '{name}'"
    | .emptyPath pos => s!"Parse error at {pos}: empty variable path"
    | .other pos msg => s!"Parse error at {pos}: {msg}"

/-- Errors that can occur during template rendering -/
inductive RenderError where
  | unknownVariable (path : String)
  | unknownPartial (name : String)
  | unknownFilter (name : String)
  | filterError (filter : String) (msg : String)
  | typeError (context : String) (expected : String) (got : String)
  | other (msg : String)
  deriving Repr, BEq, Inhabited

instance : ToString RenderError where
  toString
    | .unknownVariable path => s!"Render error: unknown variable '{path}'"
    | .unknownPartial name => s!"Render error: unknown partial '{name}'"
    | .unknownFilter name => s!"Render error: unknown filter '{name}'"
    | .filterError filter msg => s!"Render error: filter '{filter}' failed: {msg}"
    | .typeError ctx exp got => s!"Render error in {ctx}: expected {exp}, got {got}"
    | .other msg => s!"Render error: {msg}"

/-- Result type for parsing -/
abbrev ParseResult (α : Type) := Except ParseError α

/-- Result type for rendering -/
abbrev RenderResult (α : Type) := Except RenderError α

end Stencil
