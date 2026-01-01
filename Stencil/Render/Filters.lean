/-
  Stencil.Render.Filters
  Built-in filter functions
-/
import Stencil.Core.Value
import Stencil.Core.Error
import Stencil.Core.Suggest
import Stencil.AST.Types
import Std.Data.HashMap

namespace Stencil.Filters

/-- Filter function signature (no position needed - type errors are clear from context) -/
abbrev FilterFn := Value → List String → Option Position → RenderResult Value

/-- uppercase filter -/
def uppercase : FilterFn := fun v _ pos =>
  match v with
  | .string s => .ok (.string s.toUpper)
  | other => .error (.typeError "uppercase" "String" other.typeName pos)

/-- lowercase filter -/
def lowercase : FilterFn := fun v _ pos =>
  match v with
  | .string s => .ok (.string s.toLower)
  | other => .error (.typeError "lowercase" "String" other.typeName pos)

/-- trim filter -/
def trim : FilterFn := fun v _ pos =>
  match v with
  | .string s => .ok (.string s.trim)
  | other => .error (.typeError "trim" "String" other.typeName pos)

/-- length filter -/
def length : FilterFn := fun v _ pos =>
  match v with
  | .string s => .ok (.int s.length)
  | .array a => .ok (.int a.size)
  | other => .error (.typeError "length" "String or Array" other.typeName pos)

/-- default filter - returns default value if input is falsy -/
def default : FilterFn := fun v args _ =>
  if v.isTruthy then .ok v
  else .ok (.string (args.head?.getD ""))

/-- join filter - join array elements with separator -/
def join : FilterFn := fun v args pos =>
  match v with
  | .array items =>
    let sep := args.head?.getD ", "
    let strs := items.toList.map Value.toString
    .ok (.string (sep.intercalate strs))
  | other => .error (.typeError "join" "Array" other.typeName pos)

/-- first filter - get first element of array -/
def first : FilterFn := fun v _ pos =>
  match v with
  | .array items =>
    if h : 0 < items.size then .ok items[0]
    else .ok .null
  | other => .error (.typeError "first" "Array" other.typeName pos)

/-- last filter - get last element of array -/
def last : FilterFn := fun v _ pos =>
  match v with
  | .array items => .ok (items.back?.getD .null)
  | other => .error (.typeError "last" "Array" other.typeName pos)

/-- reverse filter - reverse array or string -/
def reverse : FilterFn := fun v _ pos =>
  match v with
  | .array items => .ok (.array items.reverse)
  | .string s => .ok (.string (String.ofList s.toList.reverse))
  | other => .error (.typeError "reverse" "String or Array" other.typeName pos)

/-- json filter - encode value as JSON -/
def json : FilterFn := fun v _ _ =>
  .ok (.string (v.toJson))

/-- capitalize filter - capitalize first character -/
def capitalize : FilterFn := fun v _ pos =>
  match v with
  | .string s =>
    match s.toList with
    | [] => .ok (.string "")
    | c :: cs => .ok (.string (String.ofList (c.toUpper :: cs)))
  | other => .error (.typeError "capitalize" "String" other.typeName pos)

/-- Built-in filter registry -/
def builtinFilters : Std.HashMap String FilterFn :=
  ({} : Std.HashMap String FilterFn)
  |>.insert "uppercase" uppercase
  |>.insert "lowercase" lowercase
  |>.insert "trim" trim
  |>.insert "length" length
  |>.insert "default" default
  |>.insert "join" join
  |>.insert "first" first
  |>.insert "last" last
  |>.insert "reverse" reverse
  |>.insert "json" json
  |>.insert "capitalize" capitalize

/-- Look up a filter by name -/
def getFilter (name : String) : Option FilterFn :=
  builtinFilters[name]?

/-- Apply a filter to a value -/
def applyFilter (filter : Filter) (v : Value) (pos : Option Position) : RenderResult Value := do
  match getFilter filter.name with
  | some fn => fn v filter.args pos
  | none =>
    let suggestion := suggestFilter filter.name
    .error (.unknownFilter filter.name pos suggestion)

/-- Apply a chain of filters -/
def applyFilters (filters : List Filter) (v : Value) (pos : Option Position) : RenderResult Value :=
  filters.foldlM (fun acc f => applyFilter f acc pos) v

end Stencil.Filters
