/-
  Stencil.AST.Types
  Template AST node types
-/
import Stencil.Core.Position

namespace Stencil

/-- A filter to apply to a value -/
structure Filter where
  name : String
  args : List String := []
  deriving Repr, BEq, Inhabited

/-- A variable reference with optional filters -/
structure VarRef where
  path : String
  pathParts : List String := []  -- Pre-split path for fast lookup
  filters : List Filter := []
  escaped : Bool := true
  pos : Position := default
  parentLevels : Nat := 0  -- Number of ../ prefixes for parent scope access
  deriving Repr, BEq, Inhabited

/-- Comparison operators -/
inductive CompareOp where
  | eq   -- ==
  | ne   -- !=
  | lt   -- <
  | le   -- <=
  | gt   -- >
  | ge   -- >=
  deriving Repr, BEq, Inhabited

/-- Logical operators -/
inductive LogicOp where
  | and  -- &&
  | or   -- ||
  deriving Repr, BEq, Inhabited

/-- Expression for conditionals -/
inductive Expr where
  /-- Variable reference: `user.name` -/
  | var (path : String) (parentLevels : Nat := 0)
  /-- String literal: `"active"` -/
  | strLit (value : String)
  /-- Integer literal: `42` -/
  | intLit (value : Int)
  /-- Float literal: `3.14` -/
  | floatLit (value : Float)
  /-- Boolean literal: `true` or `false` -/
  | boolLit (value : Bool)
  /-- Comparison: `a > b` -/
  | compare (op : CompareOp) (left : Expr) (right : Expr)
  /-- Logical binary: `a && b` or `a || b` -/
  | logic (op : LogicOp) (left : Expr) (right : Expr)
  /-- Logical not: `!a` -/
  | not (expr : Expr)
  /-- Helper function call: `(eq a b)` -/
  | call (name : String) (args : List Expr)
  deriving Repr, Inhabited

/-- Each loop configuration -/
structure EachConfig where
  /-- Variable/path to iterate over -/
  source : String
  /-- Optional name for item variable (default: use item as context) -/
  itemVar : Option String := none
  /-- Optional name for index variable -/
  indexVar : Option String := none
  deriving Repr, Inhabited

/-- Template AST node -/
inductive Node where
  /-- Raw text content -/
  | text (content : String)
  /-- Comment (ignored in output) -/
  | comment (content : String)
  /-- Variable interpolation -/
  | variable (ref : VarRef)
  /-- Conditional section with if/else-if/else chains -/
  | conditional (branches : List (Expr × List Node)) (elseBody : List Node) (inverted : Bool) (pos : Position)
  /-- Loop over array or object: {{#each items}} or {{#each items as |item idx|}} -/
  | each (config : EachConfig) (body : List Node) (elseBody : List Node) (pos : Position)
  /-- Change context: {{#with user}}...{{/with}} -/
  | «with» (path : String) (body : List Node) (elseBody : List Node) (pos : Position)
  /-- Local variables: {{#let x=value y=other}}...{{/let}} -/
  | «let» (bindings : List (String × Expr)) (body : List Node) (pos : Position)
  /-- Repeat content: {{#repeat 5}}...{{/repeat}} -/
  | repeat (count : Expr) (body : List Node) (pos : Position)
  /-- Numeric range: {{#range 1 10}}...{{/range}} -/
  | range (start : Expr) («end» : Expr) (body : List Node) (pos : Position)
  /-- Include a partial template with optional context and hash parameters -/
  | «partial» (name : String) (context : Option Expr) (params : List (String × Expr)) (pos : Position)
  /-- Partial block: {{#> layout}}content{{/layout}} -/
  | partialBlock (name : String) (context : Option Expr) (params : List (String × Expr)) (body : List Node) (pos : Position)
  /-- Template inheritance: {{#extends "base"}} -/
  | extends (name : String) (pos : Position)
  /-- Named block: {{#block "content"}}default{{/block}} -/
  | block (name : String) (body : List Node) (pos : Position)
  /-- Call parent block content: {{#super}} -/
  | super (pos : Position)
  deriving Repr, Inhabited

/-- A condition with its body for if/else-if chains -/
abbrev CondBranch := Expr × List Node

/-- A parsed template -/
structure Template where
  nodes : List Node := []
  deriving Repr, Inhabited

namespace Template

def empty : Template := { nodes := [] }

def isEmpty (t : Template) : Bool := t.nodes.isEmpty

end Template

end Stencil
