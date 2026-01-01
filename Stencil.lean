/-
  Stencil
  Mustache/Handlebars-style template engine for Lean 4

  Templates are parsed into an AST and rendered with a context to produce
  Scribe Html values.

  Example:
  ```
  let tmpl ← Stencil.parse "Hello, {{name}}!"
  let ctx := Stencil.context [("name", .string "World")]
  let html ← Stencil.render tmpl ctx
  ```
-/
import Stencil.Core
import Stencil.AST
import Stencil.Parser
import Stencil.Render

import Scribe

namespace Stencil

open Parser Render

/-- Parse a template string -/
def parse (input : String) : ParseResult Template :=
  Parser.parse input

/-- Parse a template string, throwing on error -/
def parse! (input : String) : Template :=
  match parse input with
  | .ok tmpl => tmpl
  | .error e => panic! s!"Template parse error: {e}"

/-- Render a template with context, producing Html -/
def render (tmpl : Template) (ctx : Context) : RenderResult Scribe.Html :=
  Render.render tmpl ctx

/-- Render a template with context, producing a String -/
def renderString (tmpl : Template) (ctx : Context) : RenderResult String :=
  Render.renderString tmpl ctx

/-- Parse and render in one step -/
def compile (input : String) (ctx : Context) : Except String Scribe.Html :=
  match parse input with
  | .ok tmpl =>
    match render tmpl ctx with
    | .ok html => .ok html
    | .error e => .error (toString e)
  | .error e => .error (toString e)

/-- Create context from key-value pairs -/
def context (pairs : List (String × Value)) : Context :=
  Context.fromPairs pairs

/-- Create context from a Value -/
def contextFromValue (v : Value) : Context :=
  Context.fromValue v

/-- Register a partial template in a context -/
def withPartial (ctx : Context) (name : String) (tmpl : Template) : Context :=
  ctx.addPartial name tmpl

/-- Register a partial from a string -/
def withPartialString (ctx : Context) (name : String) (input : String) : ParseResult Context :=
  match parse input with
  | .ok tmpl => .ok (ctx.addPartial name tmpl)
  | .error e => .error e

end Stencil
