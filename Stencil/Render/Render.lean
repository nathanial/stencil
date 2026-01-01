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
  let finalValue ← match Filters.applyFilters ref.filters value (some ref.pos) with
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

-- Mutually recursive rendering functions
mutual
  /-- Render a list of nodes -/
  partial def renderNodes (nodes : List Node) : RenderM Html := do
    let htmls ← nodes.mapM renderNode
    return .fragment htmls

  /-- Render a section (if/unless) -/
  partial def renderSection (name : String) (inverted : Bool) (body : List Node) (elseBody : List Node) (_pos : Position) : RenderM Html := do
    let ctx ← getContext
    let value := ctx.lookup name |>.getD .null
    let truthy := value.isTruthy
    let condition := if inverted then !truthy else truthy

    if condition then
      renderNodes body
    else
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

  /-- Render a partial -/
  partial def renderPartial (name : String) (pos : Position) : RenderM Html := do
    let ctx ← getContext
    match ctx.getPartial name with
    | some tmpl => renderNodes tmpl.nodes
    | none => throw (.unknownPartial name (some pos))

  /-- Render a single node -/
  partial def renderNode (node : Node) : RenderM Html :=
    match node with
    | .text content => return .raw content  -- Template text is trusted, use raw
    | .comment _ => return .fragment []  -- Comments produce no output
    | .variable ref => renderVariable ref
    | .section name inverted body elseBody pos => renderSection name inverted body elseBody pos
    | .each name body elseBody pos => renderEach name body elseBody pos
    | .«partial» name pos => renderPartial name pos
end

/-- Render a complete template -/
def renderTemplate (tmpl : Template) : RenderM Html :=
  renderNodes tmpl.nodes

/-- Public API: Render template with context, producing Html -/
def render (tmpl : Template) (ctx : Context) : RenderResult Html :=
  renderTemplate tmpl ctx

/-- Public API: Render template with context, producing String -/
def renderString (tmpl : Template) (ctx : Context) : RenderResult String :=
  match render tmpl ctx with
  | .ok html => .ok (html.render)
  | .error e => .error e

end Stencil.Render
