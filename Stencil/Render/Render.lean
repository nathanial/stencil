/-
  Stencil.Render.Render
  Template rendering to Scribe Html
-/
import Stencil.Core.Context
import Stencil.Core.Error
import Stencil.AST.Types
import Stencil.Render.Filters
import Scribe

namespace Stencil.Render

open Scribe

-- Html needs Inhabited for partial mutual recursion to work
instance : Inhabited Html where
  default := .fragment []

/-- Render monad with context -/
abbrev RenderM := ReaderT Context (Except RenderError)

/-- Get the current context -/
def getContext : RenderM Context := read

/-- Run with a modified context -/
def withContext {α : Type} (ctx : Context) (m : RenderM α) : RenderM α :=
  fun _ => m ctx

/-- Render a variable reference to a string -/
def renderVarToString (ref : VarRef) : RenderM String := do
  let ctx ← getContext
  -- Return null for missing variables (allows default filter to work)
  let value := ctx.lookup ref.path |>.getD .null

  -- Apply filters with position for error reporting
  -- Use custom filters if any are registered
  let finalValue ← match Filters.applyFiltersWithCustom ref.filters value (some ref.pos) ctx.customFilters with
    | .ok v => pure v
    | .error e => throw e

  return finalValue.toString

/-- Render a variable node -/
def renderVariable (ref : VarRef) : RenderM Html := do
  let s ← renderVarToString ref
  if ref.escaped then
    return .text s  -- .text escapes HTML
  else
    return .raw s   -- .raw does not escape

-- Expression evaluation

/-- Evaluate an expression to a Value -/
partial def evalExpr (expr : Expr) : RenderM Value := do
  let ctx ← getContext
  match expr with
  | .var path => return ctx.lookup path |>.getD .null
  | .strLit s => return .string s
  | .intLit n => return .int n
  | .floatLit f => return .float f
  | .boolLit b => return .bool b
  | .not e =>
    let v ← evalExpr e
    return .bool (!v.isTruthy)
  | .compare op left right =>
    let lv ← evalExpr left
    let rv ← evalExpr right
    return .bool (compareValues op lv rv)
  | .logic op left right =>
    let lv ← evalExpr left
    match op with
    | .and =>
      if !lv.isTruthy then return .bool false
      let rv ← evalExpr right
      return .bool rv.isTruthy
    | .or =>
      if lv.isTruthy then return .bool true
      let rv ← evalExpr right
      return .bool rv.isTruthy
where
  /-- Compare two values -/
  compareValues (op : CompareOp) (a b : Value) : Bool :=
    match op with
    | .eq => valuesEqual a b
    | .ne => !valuesEqual a b
    | .lt => valuesLess a b
    | .le => valuesLess a b || valuesEqual a b
    | .gt => valuesLess b a
    | .ge => valuesLess b a || valuesEqual a b
  /-- Check if two values are equal -/
  valuesEqual (a b : Value) : Bool :=
    match a, b with
    | .null, .null => true
    | .bool x, .bool y => x == y
    | .int x, .int y => x == y
    | .float x, .float y => x == y
    | .int x, .float y => (Float.ofInt x) == y
    | .float x, .int y => x == (Float.ofInt y)
    | .string x, .string y => x == y
    | _, _ => a.toString == b.toString
  /-- Check if a < b -/
  valuesLess (a b : Value) : Bool :=
    match a, b with
    | .int x, .int y => x < y
    | .float x, .float y => x < y
    | .int x, .float y => (Float.ofInt x) < y
    | .float x, .int y => x < (Float.ofInt y)
    | .string x, .string y => x < y
    | _, _ => a.toString < b.toString

/-- Evaluate an expression to a boolean -/
def evalExprToBool (expr : Expr) : RenderM Bool := do
  let v ← evalExpr expr
  return v.isTruthy

-- Mutually recursive rendering functions
mutual
  /-- Render a list of nodes -/
  partial def renderNodes (nodes : List Node) : RenderM Html := do
    let htmls ← nodes.mapM renderNode
    return .fragment htmls

  /-- Render a conditional with branches -/
  partial def renderConditional (branches : List (Expr × List Node)) (elseBody : List Node) (inverted : Bool) (_pos : Position) : RenderM Html := do
    -- Try each branch in order
    for (condition, body) in branches do
      let result ← evalExprToBool condition
      let shouldRender := if inverted then !result else result
      if shouldRender then
        return ← renderNodes body
    -- No branch matched, render else body
    renderNodes elseBody

  /-- Render an each loop -/
  partial def renderEach (name : String) (body : List Node) (elseBody : List Node) (_pos : Position) : RenderM Html := do
    let ctx ← getContext
    match ctx.lookup name with
    | some (.array items) =>
      if items.isEmpty then
        renderNodes elseBody
      else
        let size := items.size
        let htmlArr ← items.mapIdxM fun idx item => do
          let loopInfo : LoopMeta := {
            index := idx
            first := idx == 0
            last := idx == size - 1
          }
          let childCtx := ctx.pushScope item loopInfo
          withContext childCtx (renderNodes body)
        return .fragment htmlArr.toList
    | _ =>
      renderNodes elseBody

  /-- Render a partial with optional parameters -/
  partial def renderPartial (name : String) (params : List (String × Expr)) (pos : Position) : RenderM Html := do
    let ctx ← getContext
    match ctx.getPartial name with
    | some tmpl =>
      if params.isEmpty then
        renderNodes tmpl.nodes
      else
        -- Evaluate params and merge into context
        let paramValues ← params.mapM fun (k, expr) => do
          let v ← evalExpr expr
          return (k, v)
        let paramData := Value.object paramValues.toArray
        let childCtx := ctx.mergeData paramData
        withContext childCtx (renderNodes tmpl.nodes)
    | none => throw (.unknownPartial name (some pos))

  /-- Render a partial block -/
  partial def renderPartialBlock (name : String) (params : List (String × Expr)) (body : List Node) (pos : Position) : RenderM Html := do
    let ctx ← getContext
    match ctx.getPartial name with
    | some tmpl =>
      -- Render the block content first
      let blockHtml ← renderNodes body
      -- Evaluate params
      let paramValues ← params.mapM fun (k, expr) => do
        let v ← evalExpr expr
        return (k, v)
      -- Add @partialBlock to context (use camelCase since - isn't valid in paths)
      let allParams := paramValues.toArray ++ #[("@partialBlock", .string blockHtml.render)]
      let paramData := Value.object allParams
      let childCtx := ctx.mergeData paramData
      withContext childCtx (renderNodes tmpl.nodes)
    | none => throw (.unknownPartial name (some pos))

  /-- Render a block (check for override) -/
  partial def renderBlock (name : String) (defaultBody : List Node) (_pos : Position) : RenderM Html := do
    let ctx ← getContext
    match ctx.getBlock name with
    | some blockDef =>
      -- Use child's block, but track parent for super
      let childCtx := ctx.withCurrentBlock name defaultBody
      withContext childCtx (renderNodes blockDef.body)
    | none =>
      -- No override, use default
      renderNodes defaultBody

  /-- Render super (parent block content) -/
  partial def renderSuper (_pos : Position) : RenderM Html := do
    let ctx ← getContext
    match ctx.currentBlock with
    | some blockName =>
      match ctx.getBlock blockName with
      | some blockDef =>
        match blockDef.parentBody with
        | some parentBody => renderNodes parentBody
        | none => return .fragment []  -- No parent body
      | none => return .fragment []
    | none => return .fragment []  -- super used outside block

  /-- Render a single node -/
  partial def renderNode (node : Node) : RenderM Html :=
    match node with
    | .text content => return .raw content  -- Template text is trusted, use raw
    | .comment _ => return .fragment []  -- Comments produce no output
    | .variable ref => renderVariable ref
    | .conditional branches elseBody inverted pos => renderConditional branches elseBody inverted pos
    | .each name body elseBody pos => renderEach name body elseBody pos
    | .«partial» name params pos => renderPartial name params pos
    | .partialBlock name params body pos => renderPartialBlock name params body pos
    | .extends _ _ => return .fragment []  -- Handled at template level
    | .block name body pos => renderBlock name body pos
    | .super pos => renderSuper pos
end

/-- Extract block definitions from nodes -/
def extractBlocks (nodes : List Node) : List (String × List Node) :=
  nodes.filterMap fun node =>
    match node with
    | .block name body _ => some (name, body)
    | _ => none

/-- Render a complete template (handling extends) -/
def renderTemplate (tmpl : Template) : RenderM Html := do
  match tmpl.nodes with
  | .extends parentName pos :: restNodes =>
    let ctx ← getContext
    match ctx.getPartial parentName with
    | some parentTmpl =>
      -- Extract all block definitions from child template
      let childBlocks := extractBlocks restNodes
      -- Add child blocks to context
      let newCtx := childBlocks.foldl (fun c (name, body) => c.addBlock name body) ctx
      withContext newCtx (renderNodes parentTmpl.nodes)
    | none => throw (.unknownPartial parentName (some pos))
  | _ => renderNodes tmpl.nodes

/-- Public API: Render template with context, producing Html -/
def render (tmpl : Template) (ctx : Context) : RenderResult Html :=
  renderTemplate tmpl ctx

/-- Public API: Render template with context, producing String -/
def renderString (tmpl : Template) (ctx : Context) : RenderResult String :=
  match render tmpl ctx with
  | .ok html => .ok (html.render)
  | .error e => .error e

end Stencil.Render
