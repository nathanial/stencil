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

/-- slice filter - extract substring or array slice -/
def slice : FilterFn := fun v args pos =>
  let start := args.head?.bind (·.toNat?) |>.getD 0
  let len := args.tail.head?.bind (·.toNat?) |>.getD 0
  match v with
  | .string s =>
    let chars := s.toList
    let sliced := chars.drop start |>.take len
    .ok (.string (String.ofList sliced))
  | .array items =>
    let sliced := items.toList.drop start |>.take len
    .ok (.array sliced.toArray)
  | other => .error (.typeError "slice" "String or Array" other.typeName pos)

/-- Helper to get a property from an object value -/
private def getProp (v : Value) (key : String) : Option Value :=
  match v with
  | .object fields => fields.find? (fun (k, _) => k == key) |>.map (fun (_, v) => v)
  | _ => none

/-- sort filter - sort array (optionally by property) -/
def sort : FilterFn := fun v args pos =>
  match v with
  | .array items =>
    let prop := args.head?
    let sorted := match prop with
    | none =>
      -- Sort by string representation
      items.toList.toArray.qsort (fun a b => a.toString < b.toString)
    | some key =>
      -- Sort by property
      items.toList.toArray.qsort (fun a b =>
        let va := getProp a key
        let vb := getProp b key
        (va.map Value.toString |>.getD "") < (vb.map Value.toString |>.getD ""))
    .ok (.array sorted)
  | other => .error (.typeError "sort" "Array" other.typeName pos)

/-- uniq filter - remove duplicates from array -/
def uniq : FilterFn := fun v _ pos =>
  match v with
  | .array items =>
    let unique := items.toList.foldl (fun acc item =>
      if acc.any (fun x => x.toString == item.toString) then acc else acc ++ [item]
    ) []
    .ok (.array unique.toArray)
  | other => .error (.typeError "uniq" "Array" other.typeName pos)

/-- map filter - extract property from array of objects -/
def map : FilterFn := fun v args pos =>
  match args.head? with
  | none => .error (.filterError "map" "requires a property name argument" pos)
  | some prop =>
    match v with
    | .array items =>
      let mapped := items.map fun item => getProp item prop |>.getD .null
      .ok (.array mapped)
    | other => .error (.typeError "map" "Array" other.typeName pos)

/-- where filter - filter array by property value -/
def where_ : FilterFn := fun v args pos =>
  match args.head?, args.tail.head? with
  | some prop, valueArg =>
    match v with
    | .array items =>
      let target := valueArg.getD "true"
      let filtered := items.filter fun item =>
        match getProp item prop with
        | some (Value.bool b) => if target == "true" then b else !b
        | some (Value.string s) => s == target
        | some val => val.toString == target
        | none => false
      .ok (.array filtered)
    | other => .error (.typeError "where" "Array" other.typeName pos)
  | none, _ => .error (.filterError "where" "requires a property name argument" pos)

/-- truncate filter - truncate string with ellipsis -/
def truncate : FilterFn := fun v args pos =>
  match v with
  | .string s =>
    let maxLen := args.head?.bind (·.toNat?) |>.getD 50
    let ellipsis := args.tail.head?.getD "..."
    if s.length <= maxLen then .ok (.string s)
    else .ok (.string ((s.toList.take maxLen |> String.ofList) ++ ellipsis))
  | other => .error (.typeError "truncate" "String" other.typeName pos)

/-- replace filter - replace substring -/
def replace : FilterFn := fun v args pos =>
  match v with
  | .string s =>
    match args.head?, args.tail.head? with
    | some old, some new => .ok (.string (s.replace old new))
    | some old, none => .ok (.string (s.replace old ""))
    | none, _ => .error (.filterError "replace" "requires at least one argument" pos)
  | other => .error (.typeError "replace" "String" other.typeName pos)

/-- split filter - split string into array -/
def split : FilterFn := fun v args pos =>
  match v with
  | .string s =>
    let sep := args.head?.getD ","
    let parts := s.splitOn sep
    .ok (.array (parts.map Value.string |>.toArray))
  | other => .error (.typeError "split" "String" other.typeName pos)

/-- number filter - format number with decimal places -/
def number : FilterFn := fun v args pos =>
  let decimals := args.head?.bind (·.toNat?) |>.getD 0
  match v with
  | .int n =>
    if decimals == 0 then .ok (.string (toString n))
    else .ok (.string (toString n ++ "." ++ String.ofList (List.replicate decimals '0')))
  | .float f =>
    -- Simple decimal formatting
    let str := toString f
    let parts := str.splitOn "."
    match parts with
    | [whole] =>
      if decimals == 0 then .ok (.string whole)
      else .ok (.string (whole ++ "." ++ String.ofList (List.replicate decimals '0')))
    | [whole, frac] =>
      if decimals == 0 then .ok (.string whole)
      else
        let paddedFrac := (frac.toList ++ List.replicate decimals '0').take decimals
        .ok (.string (whole ++ "." ++ String.ofList paddedFrac))
    | _ => .ok (.string str)
  | other => .error (.typeError "number" "Int or Float" other.typeName pos)

/-- pluralize filter - return singular or plural form -/
def pluralize : FilterFn := fun v args pos =>
  let singular := args.head?.getD ""
  let plural := args.tail.head?.getD (singular ++ "s")
  match v with
  | .int n => .ok (.string (if n == 1 then singular else plural))
  | .array items => .ok (.string (if items.size == 1 then singular else plural))
  | other => .error (.typeError "pluralize" "Int or Array" other.typeName pos)

/-- escape_js filter - escape for JavaScript strings -/
def escape_js : FilterFn := fun v _ pos =>
  match v with
  | .string s =>
    let escaped := s.toList.foldl (fun acc c =>
      acc ++ match c with
      | '\\' => "\\\\"
      | '"' => "\\\""
      | '\'' => "\\'"
      | '\n' => "\\n"
      | '\r' => "\\r"
      | '\t' => "\\t"
      | c => String.ofList [c]
    ) ""
    .ok (.string escaped)
  | other => .error (.typeError "escape_js" "String" other.typeName pos)

/-- escape_uri filter - URI encoding -/
def escape_uri : FilterFn := fun v _ pos =>
  match v with
  | .string s =>
    let escaped := s.toList.foldl (fun acc c =>
      if c.isAlphanum || c == '-' || c == '_' || c == '.' || c == '~' then
        acc.push c
      else
        -- Encode as %XX
        let n := c.toNat
        let hex := "0123456789ABCDEF"
        let h1 := hex.get! ⟨n / 16⟩
        let h2 := hex.get! ⟨n % 16⟩
        acc ++ "%" ++ String.ofList [h1, h2]
    ) ""
    .ok (.string escaped)
  | other => .error (.typeError "escape_uri" "String" other.typeName pos)

/-- abs filter - absolute value -/
def abs : FilterFn := fun v _ pos =>
  match v with
  | .int n => .ok (.int n.natAbs)
  | .float f => .ok (.float (if f < 0 then -f else f))
  | other => .error (.typeError "abs" "Int or Float" other.typeName pos)

/-- keys filter - get object keys as array -/
def keys : FilterFn := fun v _ pos =>
  match v with
  | .object fields => .ok (.array (fields.map (fun (k, _) => Value.string k)))
  | other => .error (.typeError "keys" "Object" other.typeName pos)

/-- values filter - get object values as array -/
def values : FilterFn := fun v _ pos =>
  match v with
  | .object fields => .ok (.array (fields.map (fun (_, v) => v)))
  | other => .error (.typeError "values" "Object" other.typeName pos)

/-- Built-in filter registry -/
def builtinFilters : Std.HashMap String FilterFn :=
  ({} : Std.HashMap String FilterFn)
  -- String filters
  |>.insert "uppercase" uppercase
  |>.insert "lowercase" lowercase
  |>.insert "capitalize" capitalize
  |>.insert "trim" trim
  |>.insert "truncate" truncate
  |>.insert "replace" replace
  |>.insert "split" split
  -- Array/collection filters
  |>.insert "length" length
  |>.insert "first" first
  |>.insert "last" last
  |>.insert "reverse" reverse
  |>.insert "join" join
  |>.insert "slice" slice
  |>.insert "sort" sort
  |>.insert "uniq" uniq
  |>.insert "map" map
  |>.insert "where" where_
  -- Object filters
  |>.insert "keys" keys
  |>.insert "values" values
  -- Numeric filters
  |>.insert "number" number
  |>.insert "abs" abs
  |>.insert "pluralize" pluralize
  -- Utility filters
  |>.insert "default" default
  |>.insert "json" json
  -- Escape filters
  |>.insert "escape_js" escape_js
  |>.insert "escape_uri" escape_uri

/-- Look up a filter by name in builtins -/
def getFilter (name : String) : Option FilterFn :=
  builtinFilters[name]?

/-- Apply a filter to a value (builtin only) -/
def applyFilter (filter : Filter) (v : Value) (pos : Option Position) : RenderResult Value := do
  match getFilter filter.name with
  | some fn => fn v filter.args pos
  | none =>
    let suggestion := suggestFilter filter.name
    .error (.unknownFilter filter.name pos suggestion)

/-- Apply a chain of filters (builtin only) -/
def applyFilters (filters : List Filter) (v : Value) (pos : Option Position) : RenderResult Value :=
  filters.foldlM (fun acc f => applyFilter f acc pos) v

/-- Apply a filter with custom filter support -/
def applyFilterWithCustom (filter : Filter) (v : Value) (pos : Option Position)
    (customFilters : Std.HashMap String FilterFn) : RenderResult Value := do
  -- Try custom filters first
  match customFilters[filter.name]? with
  | some fn => fn v filter.args pos
  | none =>
    -- Fall back to builtin filters
    match getFilter filter.name with
    | some fn => fn v filter.args pos
    | none =>
      let suggestion := suggestFilter filter.name
      .error (.unknownFilter filter.name pos suggestion)

/-- Apply a chain of filters with custom filter support -/
def applyFiltersWithCustom (filters : List Filter) (v : Value) (pos : Option Position)
    (customFilters : Std.HashMap String FilterFn) : RenderResult Value :=
  filters.foldlM (fun acc f => applyFilterWithCustom f acc pos customFilters) v

end Stencil.Filters
