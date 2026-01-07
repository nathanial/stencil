/-
  Stencil.Core.Position
  Source position tracking for error reporting - uses Sift's SourcePos
-/
import Sift

namespace Stencil

/-- Position in template source (alias for Sift.SourcePos) -/
abbrev Position := Sift.SourcePos

end Stencil
