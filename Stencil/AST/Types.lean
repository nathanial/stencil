/-
  Stencil.AST.Types
  Template AST node types
-/

namespace Stencil

/-- A filter to apply to a value -/
structure Filter where
  name : String
  args : List String := []
  deriving Repr, BEq, Inhabited

/-- A variable reference with optional filters -/
structure VarRef where
  path : String
  filters : List Filter := []
  escaped : Bool := true
  deriving Repr, BEq, Inhabited

/-- Template AST node -/
inductive Node where
  /-- Raw text content -/
  | text (content : String)
  /-- Comment (ignored in output) -/
  | comment (content : String)
  /-- Variable interpolation -/
  | variable (ref : VarRef)
  /-- Conditional section (if/unless with optional else) -/
  | section (name : String) (inverted : Bool) (body : List Node) (elseBody : List Node)
  /-- Loop over array -/
  | each (name : String) (body : List Node) (elseBody : List Node)
  /-- Include a partial template -/
  | «partial» (name : String)
  deriving Repr, Inhabited

/-- A parsed template -/
structure Template where
  nodes : List Node := []
  deriving Repr, Inhabited

namespace Template

def empty : Template := { nodes := [] }

def isEmpty (t : Template) : Bool := t.nodes.isEmpty

end Template

end Stencil
