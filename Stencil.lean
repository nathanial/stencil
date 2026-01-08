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

/-- Filter function type for custom filters -/
abbrev FilterFn := Filters.FilterFn

/-- Register a custom filter in a context -/
def withFilter (ctx : Context) (name : String) (fn : FilterFn) : Context :=
  ctx.addFilter name fn

/-- Register a custom helper in a context -/
def withHelper (ctx : Context) (name : String) (fn : HelperFn) : Context :=
  ctx.addHelper name fn

/-- Format a parse error with source context for display -/
def formatParseError' (err : Sift.ParseError) (input : String) : String :=
  Stencil.formatParseError err input

/-- Format a render error with source context for display -/
def formatRenderError (err : RenderError) (input : String) : String :=
  RenderError.format err input

/-- Parse and render, returning formatted error on failure -/
def compileWithErrors (input : String) (ctx : Context) : Except String Scribe.Html :=
  match parse input with
  | .ok tmpl =>
    match render tmpl ctx with
    | .ok html => .ok html
    | .error e => .error (formatRenderError e input)
  | .error e => .error (formatParseError' e input)

/-- Template engine with caching support -/
structure Engine where
  cache : TemplateCache
  deriving Inhabited

namespace Engine

/-- Create a new engine with default cache -/
def new : Engine := ⟨TemplateCache.empty⟩

/-- Create engine with custom cache size -/
def withCacheSize (maxSize : Nat) : Engine :=
  ⟨TemplateCache.withMaxSize maxSize⟩

/-- Parse with caching - returns cached template if available -/
def parseCached (engine : Engine) (input : String) : ParseResult Template × Engine :=
  match engine.cache.get? input with
  | some tmpl => (.ok tmpl, engine)
  | none =>
    match Parser.parse input with
    | .ok tmpl => (.ok tmpl, { engine with cache := engine.cache.put input tmpl })
    | .error e => (.error e, engine)

/-- Parse with caching, throwing on error -/
def parseCached! (engine : Engine) (input : String) : Template × Engine :=
  match engine.cache.get? input with
  | some tmpl => (tmpl, engine)
  | none =>
    let tmpl := Stencil.parse! input
    (tmpl, { engine with cache := engine.cache.put input tmpl })

/-- Get current cache size -/
def cacheSize (engine : Engine) : Nat := engine.cache.size

/-- Clear the cache -/
def clearCache (engine : Engine) : Engine :=
  { engine with cache := engine.cache.clear }

end Engine

end Stencil
