/-
  Stencil.Render.Filters
  Built-in filter functions
-/
import Stencil.Core.Value
import Stencil.Core.Error
import Stencil.AST.Types
import Std.Data.HashMap

namespace Stencil.Filters

/-- Filter function signature -/
abbrev FilterFn := Value → List String → RenderResult Value

/-- uppercase filter -/
def uppercase : FilterFn := fun v _ =>
  match v with
  | .string s => .ok (.string s.toUpper)
  | other => .error (.typeError "uppercase" "String" other.typeName)

/-- lowercase filter -/
def lowercase : FilterFn := fun v _ =>
  match v with
  | .string s => .ok (.string s.toLower)
  | other => .error (.typeError "lowercase" "String" other.typeName)

/-- trim filter -/
def trim : FilterFn := fun v _ =>
  match v with
  | .string s => .ok (.string s.trim)
  | other => .error (.typeError "trim" "String" other.typeName)

/-- length filter -/
def length : FilterFn := fun v _ =>
  match v with
  | .string s => .ok (.int s.length)
  | .array a => .ok (.int a.size)
  | other => .error (.typeError "length" "String or Array" other.typeName)

/-- default filter - returns default value if input is falsy -/
def default : FilterFn := fun v args =>
  if v.isTruthy then .ok v
  else .ok (.string (args.head?.getD ""))

/-- join filter - join array elements with separator -/
def join : FilterFn := fun v args =>
  match v with
  | .array items =>
    let sep := args.head?.getD ", "
    let strs := items.toList.map Value.toString
    .ok (.string (sep.intercalate strs))
  | other => .error (.typeError "join" "Array" other.typeName)

/-- first filter - get first element of array -/
def first : FilterFn := fun v _ =>
  match v with
  | .array items =>
    if h : 0 < items.size then .ok items[0]
    else .ok .null
  | other => .error (.typeError "first" "Array" other.typeName)

/-- last filter - get last element of array -/
def last : FilterFn := fun v _ =>
  match v with
  | .array items => .ok (items.back?.getD .null)
  | other => .error (.typeError "last" "Array" other.typeName)

/-- reverse filter - reverse array or string -/
def reverse : FilterFn := fun v _ =>
  match v with
  | .array items => .ok (.array items.reverse)
  | .string s => .ok (.string (String.ofList s.toList.reverse))
  | other => .error (.typeError "reverse" "String or Array" other.typeName)

/-- json filter - encode value as JSON -/
def json : FilterFn := fun v _ =>
  .ok (.string (v.toJson))

/-- capitalize filter - capitalize first character -/
def capitalize : FilterFn := fun v _ =>
  match v with
  | .string s =>
    match s.toList with
    | [] => .ok (.string "")
    | c :: cs => .ok (.string (String.ofList (c.toUpper :: cs)))
  | other => .error (.typeError "capitalize" "String" other.typeName)

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
def applyFilter (filter : Filter) (v : Value) : RenderResult Value := do
  match getFilter filter.name with
  | some fn => fn v filter.args
  | none => .error (.unknownFilter filter.name)

/-- Apply a chain of filters -/
def applyFilters (filters : List Filter) (v : Value) : RenderResult Value :=
  filters.foldlM (fun acc f => applyFilter f acc) v

end Stencil.Filters
