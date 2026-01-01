/-
  Stencil.Core.Value
  Dynamic value type for template context
-/

namespace Stencil

/-- Dynamic value type for template data -/
inductive Value where
  | null
  | bool (val : Bool)
  | int (val : Int)
  | float (val : Float)
  | string (val : String)
  | array (val : Array Value)
  | object (val : Array (String × Value))
  deriving Repr, Inhabited

namespace Value

/-- Get type name for error messages -/
def typeName : Value → String
  | .null => "Null"
  | .bool _ => "Bool"
  | .int _ => "Int"
  | .float _ => "Float"
  | .string _ => "String"
  | .array _ => "Array"
  | .object _ => "Object"

/-- Check if value is truthy (for conditionals) -/
def isTruthy : Value → Bool
  | .null => false
  | .bool b => b
  | .int n => n != 0
  | .float f => f != 0.0
  | .string s => !s.isEmpty
  | .array a => !a.isEmpty
  | .object _ => true

/-- Check if value is falsy -/
def isFalsy (v : Value) : Bool := !v.isTruthy

/-- Lookup a key in an object -/
def get? (v : Value) (key : String) : Option Value :=
  match v with
  | .object entries => entries.find? (fun (k, _) => k == key) |>.map Prod.snd
  | _ => none

/-- Parse a dot-separated path into components -/
private def parsePath (path : String) : List String :=
  path.splitOn "." |>.filter (!·.isEmpty)

/-- Navigate a path like "user.profile.name" -/
def getPath (v : Value) (path : String) : Option Value :=
  let parts := parsePath path
  parts.foldlM (fun acc key => acc.get? key) v

/-- Navigate using pre-split path parts (faster) -/
def getPathParts (v : Value) (parts : List String) : Option Value :=
  parts.foldlM (fun acc key => acc.get? key) v

/-- Convert value to string for output -/
partial def toString : Value → String
  | .null => ""
  | .bool true => "true"
  | .bool false => "false"
  | .int n => ToString.toString n
  | .float f => ToString.toString f
  | .string s => s
  | .array items =>
    let strs := items.toList.map toString
    "[" ++ ", ".intercalate strs ++ "]"
  | .object entries =>
    let pairs := entries.toList.map fun (k, v) => s!"\"{k}\": {toString v}"
    "{" ++ ", ".intercalate pairs ++ "}"

instance : ToString Value where
  toString := Value.toString

/-- Convert value to JSON string -/
partial def toJson : Value → String
  | .null => "null"
  | .bool true => "true"
  | .bool false => "false"
  | .int n => ToString.toString n
  | .float f => ToString.toString f
  | .string s => "\"" ++ escapeJsonString s ++ "\""
  | .array items =>
    let strs := items.toList.map toJson
    "[" ++ ", ".intercalate strs ++ "]"
  | .object entries =>
    let pairs := entries.toList.map fun (k, v) => s!"\"{escapeJsonString k}\": {toJson v}"
    "{" ++ ", ".intercalate pairs ++ "}"
where
  escapeJsonString (s : String) : String :=
    String.ofList (s.toList.map escapeChar)
  escapeChar : Char → Char
    | '"' => '"'  -- Would need proper escaping in real impl
    | c => c

/-- Get array length or string length -/
def length : Value → Option Nat
  | .string s => some s.length
  | .array a => some a.size
  | _ => none

/-- Get first element of array -/
def first? : Value → Option Value
  | .array a => if h : 0 < a.size then some a[0] else none
  | _ => none

/-- Get last element of array -/
def last? : Value → Option Value
  | .array a => a.back?
  | _ => none

/-- Reverse array or string -/
def reverse : Value → Option Value
  | .array a => some (.array a.reverse)
  | .string s => some (.string (String.ofList s.toList.reverse))
  | _ => none

end Value

/-- Typeclass for converting Lean values to template Values -/
class ToValue (α : Type) where
  toValue : α → Value

instance : ToValue String where toValue := .string
instance : ToValue Int where toValue := .int
instance : ToValue Nat where toValue n := .int n
instance : ToValue Float where toValue := .float
instance : ToValue Bool where toValue := .bool

instance {α : Type} [ToValue α] : ToValue (Option α) where
  toValue
    | some x => ToValue.toValue x
    | none => .null

instance {α : Type} [ToValue α] : ToValue (Array α) where
  toValue arr := .array (arr.map ToValue.toValue)

instance {α : Type} [ToValue α] : ToValue (List α) where
  toValue lst := .array (lst.toArray.map ToValue.toValue)

/-- Helper to build object values -/
def obj (pairs : List (String × Value)) : Value :=
  .object pairs.toArray

end Stencil
