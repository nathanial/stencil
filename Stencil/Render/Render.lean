/-
  Stencil.Render.Render
  Template rendering to Scribe Html
-/
import Stencil.Core.Context
import Stencil.Core.Error
import Stencil.AST.Types
import Stencil.Render.Filters
import Stencil.Render.Helpers
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
  -- Handle parent path traversal (../)
  let value := if ref.parentLevels > 0 then
    ctx.lookupFromParent ref.path ref.parentLevels |>.getD .null
  else
    -- Use pre-split path parts for faster lookup
    ctx.lookupParts ref.pathParts ref.path |>.getD .null

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
  | .var path parentLevels =>
    -- Handle parent path traversal (../)
    if parentLevels > 0 then
      return ctx.lookupFromParent path parentLevels |>.getD .null
    else
      return ctx.lookup path |>.getD .null
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
  | .call name args =>
    -- Helper function call: (eq a b)
    match lookupHelper ctx name with
    | some helperFn =>
      let argValues ← args.mapM evalExpr
      match helperFn argValues none with
      | .ok v => return v
      | .error e => throw e
    | none =>
      -- Unknown helper, return null (could also throw an error)
      return .null
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
  partial def renderEach (config : EachConfig) (body : List Node) (elseBody : List Node) (_pos : Position) : RenderM Html := do
    let ctx ← getContext
    match ctx.lookup config.source with
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
            length := size
          }
          -- Handle named variables: {{#each items as |item idx|}}
          let itemData := match config.itemVar, config.indexVar with
            | some itemName, some idxName =>
              -- Both named: create object with both
              Value.object #[(itemName, item), (idxName, .int idx)]
            | some itemName, none =>
              -- Just item named
              Value.object #[(itemName, item)]
            | none, _ =>
              -- Default: item becomes context
              item
          let childCtx := ctx.pushScope itemData loopInfo
          withContext childCtx (renderNodes body)
        return .fragment htmlArr.toList
    | some (.object pairs) =>
      -- Object iteration: iterate over key-value pairs
      if pairs.isEmpty then
        renderNodes elseBody
      else
        let size := pairs.size
        let htmlArr ← pairs.mapIdxM fun idx (key, value) => do
          let loopInfo : LoopMeta := {
            index := idx
            first := idx == 0
            last := idx == size - 1
            length := size
            key := some key
          }
          let itemData := match config.itemVar with
            | some itemName => Value.object #[(itemName, value)]
            | none => value
          let childCtx := ctx.pushScope itemData loopInfo
          withContext childCtx (renderNodes body)
        return .fragment htmlArr.toList
    | _ =>
      renderNodes elseBody

  /-- Render a with block -/
  partial def renderWith (path : String) (body : List Node) (elseBody : List Node) (_pos : Position) : RenderM Html := do
    let ctx ← getContext
    match ctx.lookup path with
    | some v =>
      if v.isTruthy then
        let childCtx := ctx.pushSectionScope v
        withContext childCtx (renderNodes body)
      else
        renderNodes elseBody
    | none =>
      renderNodes elseBody

  /-- Render a let block -/
  partial def renderLet (bindings : List (String × Expr)) (body : List Node) (_pos : Position) : RenderM Html := do
    let ctx ← getContext
    -- Evaluate all bindings
    let values ← bindings.mapM fun (name, expr) => do
      let v ← evalExpr expr
      return (name, v)
    let bindingsData := Value.object values.toArray
    let childCtx := ctx.mergeData bindingsData
    withContext childCtx (renderNodes body)

  /-- Render a repeat block -/
  partial def renderRepeat (countExpr : Expr) (body : List Node) (_pos : Position) : RenderM Html := do
    let ctx ← getContext
    let countVal ← evalExpr countExpr
    let count := match countVal with
      | .int n => if n > 0 then n.toNat else 0
      | _ => 0
    if count == 0 then
      return .fragment []
    else
      let htmls ← (List.range count).mapM fun idx => do
        let loopInfo : LoopMeta := {
          index := idx
          first := idx == 0
          last := idx == count - 1
          length := count
        }
        let childCtx := ctx.pushScope (.int idx) loopInfo
        withContext childCtx (renderNodes body)
      return .fragment htmls

  /-- Render a range block -/
  partial def renderRange (startExpr : Expr) (endExpr : Expr) (body : List Node) (_pos : Position) : RenderM Html := do
    let ctx ← getContext
    let startVal ← evalExpr startExpr
    let endVal ← evalExpr endExpr
    let (startN, endN) := match startVal, endVal with
      | .int s, .int e => (s, e)
      | _, _ => (0, 0)
    if startN >= endN then
      return .fragment []
    else
      let count := (endN - startN).toNat
      let htmls ← (List.range count).mapM fun (offset : Nat) => do
        let idx := startN + offset
        let loopInfo : LoopMeta := {
          index := offset
          first := offset == 0
          last := offset == count - 1
          length := count
        }
        let childCtx := ctx.pushScope (.int idx) loopInfo
        withContext childCtx (renderNodes body)
      return .fragment htmls

  /-- Render a partial with optional context and hash parameters.
      If context is provided, it replaces the current data context.
      Hash params are merged on top of the context (or current context if none). -/
  partial def renderPartial (name : String) (context : Option Expr) (params : List (String × Expr)) (pos : Position) : RenderM Html := do
    let ctx ← getContext
    match ctx.getPartial name with
    | some tmpl =>
      -- Determine base context: use provided context or keep current
      let baseCtx ← match context with
        | some contextExpr =>
          let contextVal ← evalExpr contextExpr
          pure (ctx.withData contextVal)
        | none => pure ctx
      -- If there are hash params, merge them on top
      if params.isEmpty then
        withContext baseCtx (renderNodes tmpl.nodes)
      else
        let paramValues ← params.mapM fun (k, expr) => do
          let v ← evalExpr expr
          return (k, v)
        let paramData := Value.object paramValues.toArray
        let childCtx := baseCtx.mergeData paramData
        withContext childCtx (renderNodes tmpl.nodes)
    | none => throw (.unknownPartial name (some pos))

  /-- Render a partial block with optional context -/
  partial def renderPartialBlock (name : String) (context : Option Expr) (params : List (String × Expr)) (body : List Node) (pos : Position) : RenderM Html := do
    let ctx ← getContext
    match ctx.getPartial name with
    | some tmpl =>
      -- Render the block content first (in current context)
      let blockHtml ← renderNodes body
      -- Determine base context: use provided context or keep current
      let baseCtx ← match context with
        | some contextExpr =>
          let contextVal ← evalExpr contextExpr
          pure (ctx.withData contextVal)
        | none => pure ctx
      -- Evaluate hash params
      let paramValues ← params.mapM fun (k, expr) => do
        let v ← evalExpr expr
        return (k, v)
      -- Add @partialBlock to context
      let allParams := paramValues.toArray ++ #[("@partialBlock", .string blockHtml.render)]
      let paramData := Value.object allParams
      let childCtx := baseCtx.mergeData paramData
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
    | .each config body elseBody pos => renderEach config body elseBody pos
    | .«with» path body elseBody pos => renderWith path body elseBody pos
    | .«let» bindings body pos => renderLet bindings body pos
    | .repeat count body pos => renderRepeat count body pos
    | .range start «end» body pos => renderRange start «end» body pos
    | .«partial» name context params pos => renderPartial name context params pos
    | .partialBlock name context params body pos => renderPartialBlock name context params body pos
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
