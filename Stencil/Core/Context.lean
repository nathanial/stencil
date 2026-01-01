/-
  Stencil.Core.Context
  Template render context
-/
import Stencil.Core.Value
import Stencil.AST.Types
import Std.Data.HashMap

namespace Stencil

/-- Loop iteration metadata -/
structure LoopMeta where
  index : Nat
  first : Bool
  last : Bool
  deriving Repr, Inhabited

/-- Registry of partial templates -/
abbrev PartialRegistry := Std.HashMap String Template

/-- Template render context -/
structure Context where
  data : Value
  partials : PartialRegistry := {}
  parent : Option Context := none
  loopMeta : Option LoopMeta := none
  deriving Inhabited

namespace Context

/-- Empty context -/
def empty : Context := { data := .null }

/-- Create context from a Value -/
def fromValue (v : Value) : Context := { data := v }

/-- Create context from key-value pairs -/
def fromPairs (pairs : List (String × Value)) : Context :=
  { data := .object pairs.toArray }

/-- Look up a variable path in the context -/
partial def lookup (ctx : Context) (path : String) : Option Value :=
  -- Handle special loop variables
  if path == "this" || path == "." then
    some ctx.data
  else if path.startsWith "@" then
    match ctx.loopMeta with
    | none => none
    | some lm =>
      match path with
      | "@index" => some (.int lm.index)
      | "@first" => some (.bool lm.first)
      | "@last" => some (.bool lm.last)
      | _ => none
  else
    -- Try current context first
    match ctx.data.getPath path with
    | some v => some v
    | none =>
      -- Try parent context
      match ctx.parent with
      | some p => p.lookup path
      | none => none

/-- Create a child context for loop iteration -/
def pushScope (ctx : Context) (data : Value) (loopInfo : LoopMeta) : Context :=
  { data := data
  , partials := ctx.partials
  , parent := some ctx
  , loopMeta := some loopInfo
  }

/-- Create a child context for section (no loop meta) -/
def pushSectionScope (ctx : Context) (data : Value) : Context :=
  { data := data
  , partials := ctx.partials
  , parent := some ctx
  , loopMeta := none
  }

/-- Register a partial template -/
def addPartial (ctx : Context) (name : String) (tmpl : Template) : Context :=
  { ctx with partials := ctx.partials.insert name tmpl }

/-- Look up a partial -/
def getPartial (ctx : Context) (name : String) : Option Template :=
  ctx.partials.get? name

end Context

end Stencil
