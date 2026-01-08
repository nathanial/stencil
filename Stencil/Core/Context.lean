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

/-- Helper function type (for subexpressions like (eq a b)) -/
abbrev HelperFn := List Value → Option Position → RenderResult Value

/-- Registry of helper functions -/
abbrev HelperRegistry := Std.HashMap String HelperFn

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
  helpers : HelperRegistry := {}
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

/-- Look up using pre-split path parts (faster) -/
partial def lookupParts (ctx : Context) (parts : List String) (path : String) : Option Value :=
  -- Handle special variables (need original path for these)
  if path == "this" || path == "." then
    some ctx.data
  else if path.startsWith "@" then
    match ctx.loopMeta with
    | some lm =>
      match path with
      | "@index" => some (.int lm.index)
      | "@first" => some (.bool lm.first)
      | "@last" => some (.bool lm.last)
      | "@length" => some (.int lm.length)
      | "@key" => lm.key.map .string
      | _ => ctx.data.getPathParts parts
    | none => ctx.data.getPathParts parts
  else
    match ctx.data.getPathParts parts with
    | some v => some v
    | none =>
      match ctx.parent with
      | some p => p.lookupParts parts path
      | none => none

/-- Create a child context for loop iteration -/
def pushScope (ctx : Context) (data : Value) (loopInfo : LoopMeta) : Context :=
  { data := data
  , partials := ctx.partials
  , customFilters := ctx.customFilters
  , helpers := ctx.helpers
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
  , helpers := ctx.helpers
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

/-- Replace context data entirely (for partial context arguments) -/
def withData (ctx : Context) (newData : Value) : Context :=
  { ctx with data := newData }

/-- Register a helper function -/
def addHelper (ctx : Context) (name : String) (fn : HelperFn) : Context :=
  { ctx with helpers := ctx.helpers.insert name fn }

/-- Look up a helper function -/
def getHelper (ctx : Context) (name : String) : Option HelperFn :=
  ctx.helpers.get? name

/-- Traverse up N parent levels, returning the target context -/
partial def getParentContext (ctx : Context) (levels : Nat) : Option Context :=
  if levels == 0 then
    some ctx
  else
    match ctx.parent with
    | some p => p.getParentContext (levels - 1)
    | none => none

/-- Look up a path starting from a parent context N levels up -/
partial def lookupFromParent (ctx : Context) (path : String) (parentLevels : Nat) : Option Value :=
  match ctx.getParentContext parentLevels with
  | some targetCtx => targetCtx.lookup path
  | none => none

end Context

end Stencil
