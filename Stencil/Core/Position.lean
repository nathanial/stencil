/-
  Stencil.Core.Position
  Source position tracking for error reporting
-/

namespace Stencil

/-- Position in template source -/
structure Position where
  offset : Nat
  line : Nat
  column : Nat
  deriving Repr, BEq, Inhabited

instance : ToString Position where
  toString p := s!"line {p.line}, column {p.column}"

end Stencil
