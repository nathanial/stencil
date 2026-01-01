/-
  Stencil.Core.Context
  Template render context
-/
import Stencil.Core.Value
import Stencil.Core.Error
import Stencil.AST.Types
import Std.Data.HashMap

namespace Stencil

/-- Loop iteration metadata -/
structure LoopMeta where
  index : Nat
  first : Bool
  last : Bool
  length : Nat := 0       -- Total length of collection
  key : Option String := none  -- Key for object iteration
  deriving Repr, Inhabited

/-- Registry of partial templates -/
abbrev PartialRegistry := Std.HashMap String Template

/-- Custom filter function type -/
abbrev CustomFilterFn := Value → List String → Option Position → RenderResult Value

/-- Registry of custom filters -/
abbrev FilterRegistry := Std.HashMap String CustomFilterFn

/-- Block definition for template inheritance -/
structure BlockDef where
  body : List Node
  parentBody : Option (List Node) := none
  deriving Inhabited

/-- Registry of block overrides -/
abbrev BlockRegistry := Std.HashMap String BlockDef

/-- Template render context -/
structure Context where
  data : Value
  partials : PartialRegistry := {}
  customFilters : FilterRegistry := {}
  blocks : BlockRegistry := {}
  currentBlock : Option String := none
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
  -- Handle special variables
  if path == "this" || path == "." then
    some ctx.data
  else if path.startsWith "@" then
    -- Check loop meta for known loop variables
    match ctx.loopMeta with
    | some lm =>
      match path with
      | "@index" => some (.int lm.index)
      | "@first" => some (.bool lm.first)
      | "@last" => some (.bool lm.last)
      | "@length" => some (.int lm.length)
      | "@key" => lm.key.map .string
      | _ => ctx.data.getPath path  -- Try data for other @ variables
    | none => ctx.data.getPath path  -- Try data for @ variables
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
  , customFilters := ctx.customFilters
  , blocks := ctx.blocks
  , currentBlock := ctx.currentBlock
  , parent := some ctx
  , loopMeta := some loopInfo
  }

/-- Create a child context for section (no loop meta) -/
def pushSectionScope (ctx : Context) (data : Value) : Context :=
  { data := data
  , partials := ctx.partials
  , customFilters := ctx.customFilters
  , blocks := ctx.blocks
  , currentBlock := ctx.currentBlock
  , parent := some ctx
  , loopMeta := none
  }

/-- Look up a block override -/
def getBlock (ctx : Context) (name : String) : Option BlockDef :=
  ctx.blocks.get? name

/-- Add a block override -/
def addBlock (ctx : Context) (name : String) (body : List Node) : Context :=
  { ctx with blocks := ctx.blocks.insert name { body := body } }

/-- Set current block being rendered (for super) -/
def withCurrentBlock (ctx : Context) (name : String) (parentBody : List Node) : Context :=
  { ctx with
    currentBlock := some name,
    blocks := ctx.blocks.insert name { body := (ctx.getBlock name).map (·.body) |>.getD [], parentBody := some parentBody }
  }

/-- Register a partial template -/
def addPartial (ctx : Context) (name : String) (tmpl : Template) : Context :=
  { ctx with partials := ctx.partials.insert name tmpl }

/-- Look up a partial -/
def getPartial (ctx : Context) (name : String) : Option Template :=
  ctx.partials.get? name

/-- Register a custom filter -/
def addFilter (ctx : Context) (name : String) (fn : CustomFilterFn) : Context :=
  { ctx with customFilters := ctx.customFilters.insert name fn }

/-- Look up a custom filter -/
def getFilter (ctx : Context) (name : String) : Option CustomFilterFn :=
  ctx.customFilters.get? name

/-- Merge new data into context (new data takes precedence) -/
def mergeData (ctx : Context) (newData : Value) : Context :=
  let mergedData := match ctx.data, newData with
    | .object existing, .object new =>
      -- Merge objects: new values override existing
      let merged := existing.foldl (init := new) fun acc (k, v) =>
        if acc.any (fun (k', _) => k' == k) then acc
        else acc.push (k, v)
      .object merged
    | _, new => new  -- Non-objects: just use new data
  { ctx with data := mergedData }

end Context

end Stencil
