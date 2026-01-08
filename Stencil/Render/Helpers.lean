/-
  Stencil.Render.Helpers
  Built-in helper functions for subexpressions like (eq a b)
-/
import Stencil.Core.Value
import Stencil.Core.Error
import Stencil.Core.Context
import Stencil.Core.Position

namespace Stencil

/-- Compare two floats for ordering -/
private def compareFloats (x y : Float) : Ordering :=
  if x < y then .lt
  else if x > y then .gt
  else .eq

/-- Compare two values for ordering -/
private def compareValues (a b : Value) : Option Ordering :=
  match a, b with
  | .int x, .int y => some (compare x y)
  | .float x, .float y => some (compareFloats x y)
  | .int x, .float y => some (compareFloats (Float.ofInt x) y)
  | .float x, .int y => some (compareFloats x (Float.ofInt y))
  | .string x, .string y => some (compare x y)
  | .bool x, .bool y => some (compare x y)
  | _, _ => none

/-- Check equality between two values -/
private def valuesEqual (a b : Value) : Bool :=
  match a, b with
  | .null, .null => true
  | .bool x, .bool y => x == y
  | .int x, .int y => x == y
  | .float x, .float y => x == y
  | .int x, .float y => (Float.ofInt x) == y
  | .float x, .int y => x == (Float.ofInt y)
  | .string x, .string y => x == y
  | _, _ => false

/-- Helper: eq - equality check -/
def helperEq : HelperFn := fun args _pos =>
  match args with
  | [a, b] => .ok (.bool (valuesEqual a b))
  | _ => .ok .null

/-- Helper: ne - not equal check -/
def helperNe : HelperFn := fun args _pos =>
  match args with
  | [a, b] => .ok (.bool (!valuesEqual a b))
  | _ => .ok .null

/-- Helper: lt - less than -/
def helperLt : HelperFn := fun args _pos =>
  match args with
  | [a, b] =>
    match compareValues a b with
    | some .lt => .ok (.bool true)
    | some _ => .ok (.bool false)
    | none => .ok .null
  | _ => .ok .null

/-- Helper: gt - greater than -/
def helperGt : HelperFn := fun args _pos =>
  match args with
  | [a, b] =>
    match compareValues a b with
    | some .gt => .ok (.bool true)
    | some _ => .ok (.bool false)
    | none => .ok .null
  | _ => .ok .null

/-- Helper: le - less than or equal -/
def helperLe : HelperFn := fun args _pos =>
  match args with
  | [a, b] =>
    match compareValues a b with
    | some .lt | some .eq => .ok (.bool true)
    | some _ => .ok (.bool false)
    | none => .ok .null
  | _ => .ok .null

/-- Helper: ge - greater than or equal -/
def helperGe : HelperFn := fun args _pos =>
  match args with
  | [a, b] =>
    match compareValues a b with
    | some .gt | some .eq => .ok (.bool true)
    | some _ => .ok (.bool false)
    | none => .ok .null
  | _ => .ok .null

/-- Helper: and - logical AND -/
def helperAnd : HelperFn := fun args _pos =>
  match args with
  | [a, b] => .ok (.bool (a.isTruthy && b.isTruthy))
  | _ => .ok .null

/-- Helper: or - logical OR -/
def helperOr : HelperFn := fun args _pos =>
  match args with
  | [a, b] => .ok (.bool (a.isTruthy || b.isTruthy))
  | _ => .ok .null

/-- Helper: not - logical NOT -/
def helperNot : HelperFn := fun args _pos =>
  match args with
  | [a] => .ok (.bool (!a.isTruthy))
  | _ => .ok .null

/-- Registry of built-in helpers -/
def builtinHelpers : HelperRegistry :=
  ({} : HelperRegistry)
    |>.insert "eq" helperEq
    |>.insert "ne" helperNe
    |>.insert "lt" helperLt
    |>.insert "gt" helperGt
    |>.insert "le" helperLe
    |>.insert "ge" helperGe
    |>.insert "and" helperAnd
    |>.insert "or" helperOr
    |>.insert "not" helperNot

/-- Look up a helper (custom first, then builtin) -/
def lookupHelper (ctx : Context) (name : String) : Option HelperFn :=
  match ctx.getHelper name with
  | some fn => some fn
  | none => builtinHelpers.get? name

end Stencil
