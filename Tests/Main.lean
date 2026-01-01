/-
  Stencil Tests
-/
import Crucible
import Stencil

namespace Stencil.Tests

open Crucible
open Stencil

/-- Check if a string contains a substring -/
def contains (haystack : String) (needle : String) : Bool :=
  (haystack.splitOn needle).length != 1

testSuite "Stencil Tests"

-- Parser Tests

test "Parse simple text" := do
  let tmpl ← shouldBeOk (parse "Hello, World!") "parsing text"
  tmpl.nodes.length ≡ 1

test "Parse variable" := do
  let tmpl ← shouldBeOk (parse "Hello, {{name}}!") "parsing variable"
  tmpl.nodes.length ≡ 3

test "Parse triple brace (raw)" := do
  let tmpl ← shouldBeOk (parse "{{{html}}}") "parsing raw"
  tmpl.nodes.length ≡ 1

test "Parse comment" := do
  let tmpl ← shouldBeOk (parse "{{! this is a comment }}") "parsing comment"
  tmpl.nodes.length ≡ 1

test "Parse if section" := do
  let tmpl ← shouldBeOk (parse "{{#if show}}visible{{/if}}") "parsing if"
  tmpl.nodes.length ≡ 1

test "Parse if-else section" := do
  let tmpl ← shouldBeOk (parse "{{#if show}}yes{{else}}no{{/if}}") "parsing if-else"
  tmpl.nodes.length ≡ 1

test "Parse each loop" := do
  let tmpl ← shouldBeOk (parse "{{#each items}}{{this}}{{/each}}") "parsing each"
  tmpl.nodes.length ≡ 1

test "Parse partial" := do
  let tmpl ← shouldBeOk (parse "{{> header}}") "parsing partial"
  tmpl.nodes.length ≡ 1

test "Parse filter" := do
  let tmpl ← shouldBeOk (parse "{{name | uppercase}}") "parsing filter"
  tmpl.nodes.length ≡ 1

test "Parse multiple filters" := do
  let tmpl ← shouldBeOk (parse "{{name | trim | uppercase}}") "parsing filters"
  tmpl.nodes.length ≡ 1

-- Value Tests

test "Value isTruthy - string" := do
  let v : Value := .string "hello"
  ensure v.isTruthy "non-empty string should be truthy"

test "Value isTruthy - empty string" := do
  let v : Value := .string ""
  ensure (!v.isTruthy) "empty string should be falsy"

test "Value isTruthy - null" := do
  let v : Value := .null
  ensure (!v.isTruthy) "null should be falsy"

test "Value getPath" := do
  let v : Value := .object #[
    ("user", .object #[
      ("name", .string "Alice")
    ])
  ]
  match v.getPath "user.name" with
  | some (.string "Alice") => pure ()
  | _ => throw <| IO.userError "Expected to find user.name = Alice"

-- Render Tests

test "Render simple text" := do
  let tmpl ← shouldBeOk (parse "Hello, World!") "parsing"
  let result ← shouldBeOk (render tmpl Context.empty) "rendering"
  result.render ≡ "Hello, World!"

test "Render variable" := do
  let tmpl ← shouldBeOk (parse "Hello, {{name}}!") "parsing"
  let ctx := context [("name", .string "Alice")]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "Hello, Alice!"

test "Render escapes HTML" := do
  let tmpl ← shouldBeOk (parse "{{content}}") "parsing"
  let ctx := context [("content", .string "<script>")]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "&lt;script&gt;"

test "Render raw does not escape" := do
  let tmpl ← shouldBeOk (parse "{{{content}}}") "parsing"
  let ctx := context [("content", .string "<b>bold</b>")]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "<b>bold</b>"

test "Render if - truthy" := do
  let tmpl ← shouldBeOk (parse "{{#if show}}visible{{/if}}") "parsing"
  let ctx := context [("show", .bool true)]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "visible"

test "Render if - falsy" := do
  let tmpl ← shouldBeOk (parse "{{#if show}}visible{{/if}}") "parsing"
  let ctx := context [("show", .bool false)]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ ""

test "Render if-else - truthy" := do
  let tmpl ← shouldBeOk (parse "{{#if show}}yes{{else}}no{{/if}}") "parsing"
  let ctx := context [("show", .bool true)]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "yes"

test "Render if-else - falsy" := do
  let tmpl ← shouldBeOk (parse "{{#if show}}yes{{else}}no{{/if}}") "parsing"
  let ctx := context [("show", .bool false)]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "no"

test "Render unless - truthy" := do
  let tmpl ← shouldBeOk (parse "{{#unless hide}}visible{{/unless}}") "parsing"
  let ctx := context [("hide", .bool true)]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ ""

test "Render unless - falsy" := do
  let tmpl ← shouldBeOk (parse "{{#unless hide}}visible{{/unless}}") "parsing"
  let ctx := context [("hide", .bool false)]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "visible"

test "Render each loop" := do
  let tmpl ← shouldBeOk (parse "{{#each items}}{{this}} {{/each}}") "parsing"
  let ctx := context [("items", .array #[.string "a", .string "b", .string "c"])]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "a b c "

test "Render each with @index" := do
  let tmpl ← shouldBeOk (parse "{{#each items}}{{@index}}:{{this}} {{/each}}") "parsing"
  let ctx := context [("items", .array #[.string "a", .string "b"])]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "0:a 1:b "

test "Render nested path" := do
  let tmpl ← shouldBeOk (parse "{{user.name}}") "parsing"
  let ctx := context [("user", .object #[("name", .string "Bob")])]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "Bob"

-- Filter Tests

test "Filter uppercase" := do
  let tmpl ← shouldBeOk (parse "{{name | uppercase}}") "parsing"
  let ctx := context [("name", .string "hello")]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "HELLO"

test "Filter lowercase" := do
  let tmpl ← shouldBeOk (parse "{{name | lowercase}}") "parsing"
  let ctx := context [("name", .string "HELLO")]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "hello"

test "Filter trim" := do
  let tmpl ← shouldBeOk (parse "{{text | trim}}") "parsing"
  let ctx := context [("text", .string "  hello  ")]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "hello"

test "Filter length - string" := do
  let tmpl ← shouldBeOk (parse "{{text | length}}") "parsing"
  let ctx := context [("text", .string "hello")]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "5"

test "Filter length - array" := do
  let tmpl ← shouldBeOk (parse "{{items | length}}") "parsing"
  let ctx := context [("items", .array #[.int 1, .int 2, .int 3])]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "3"

test "Filter default - has value" := do
  let tmpl ← shouldBeOk (parse "{{name | default \"Anonymous\"}}") "parsing"
  let ctx := context [("name", .string "Alice")]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "Alice"

test "Filter default - null" := do
  let tmpl ← shouldBeOk (parse "{{name | default \"Anonymous\"}}") "parsing"
  let ctx := context []
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "Anonymous"

test "Filter chain" := do
  let tmpl ← shouldBeOk (parse "{{text | trim | uppercase}}") "parsing"
  let ctx := context [("text", .string "  hello  ")]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "HELLO"

-- Partial Tests

test "Render partial" := do
  let headerTmpl ← shouldBeOk (parse "<header>{{title}}</header>") "parsing header"
  let mainTmpl ← shouldBeOk (parse "{{> header}}<main>content</main>") "parsing main"
  let ctx := context [("title", .string "My Page")]
    |>.addPartial "header" headerTmpl
  let result ← shouldBeOk (render mainTmpl ctx) "rendering"
  result.render ≡ "<header>My Page</header><main>content</main>"

-- Error Message Tests

test "Levenshtein distance - identical" := do
  let dist := levenshtein "hello" "hello"
  dist ≡ 0

test "Levenshtein distance - one change" := do
  let dist := levenshtein "hello" "hallo"
  dist ≡ 1

test "Levenshtein distance - two changes" := do
  let dist := levenshtein "hello" "hxllo"
  dist ≡ 1

test "Levenshtein distance - empty string" := do
  let dist := levenshtein "" "hello"
  dist ≡ 5

test "Filter suggestion - typo" := do
  let suggestion := suggestFilter "upprcase"
  match suggestion with
  | some "uppercase" => pure ()
  | _ => throw <| IO.userError "Expected suggestion 'uppercase'"

test "Filter suggestion - no match" := do
  let suggestion := suggestFilter "foobar123"
  match suggestion with
  | none => pure ()
  | some s => throw <| IO.userError s!"Unexpected suggestion: {s}"

test "Unknown filter error has position" := do
  let tmpl ← shouldBeOk (parse "{{name | upprcase}}") "parsing"
  let ctx := context [("name", .string "test")]
  match render tmpl ctx with
  | .error (.unknownFilter "upprcase" (some pos) (some "uppercase")) =>
    ensure (pos.line == 1) "position should be line 1"
  | .error e => throw <| IO.userError s!"Wrong error type: {e}"
  | .ok _ => throw <| IO.userError "Expected error for unknown filter"

test "Unknown partial error has position" := do
  let tmpl ← shouldBeOk (parse "test{{> missing}}done") "parsing"
  match render tmpl Context.empty with
  | .error (.unknownPartial "missing" (some pos)) =>
    ensure (pos.line == 1) "position should be line 1"
  | .error e => throw <| IO.userError s!"Wrong error type: {e}"
  | .ok _ => throw <| IO.userError "Expected error for unknown partial"

test "Source context formatting" := do
  let input := "line1\nline2\n{{name | badfilter}}\nline4"
  let pos : Position := { offset := 12, line := 3, column := 10 }
  let ctx := sourceContext input pos
  ensure (contains ctx "line2") "should show line before"
  ensure (contains ctx "badfilter") "should show error line"
  ensure (contains ctx "line4") "should show line after"
  ensure (contains ctx "^") "should show caret"

test "ParseError format includes source" := do
  let input := "Hello {{#if}}"
  match parse input with
  | .error e =>
    let formatted := formatParseError e input
    ensure (contains formatted "#if") "should show context"
  | .ok _ => throw <| IO.userError "Expected parse error"

test "Type error includes position" := do
  let tmpl ← shouldBeOk (parse "{{items | uppercase}}") "parsing"
  let ctx := context [("items", .array #[.int 1, .int 2])]
  match render tmpl ctx with
  | .error (.typeError "uppercase" "String" "Array" (some _)) => pure ()
  | .error e => throw <| IO.userError s!"Wrong error type: {e}"
  | .ok _ => throw <| IO.userError "Expected type error"

-- New Filter Tests

test "Filter slice - string" := do
  let tmpl ← shouldBeOk (parse "{{text | slice \"1\" \"3\"}}") "parsing"
  let ctx := context [("text", .string "hello")]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "ell"

test "Filter slice - array" := do
  let tmpl ← shouldBeOk (parse "{{items | slice \"1\" \"2\" | join \",\"}}") "parsing"
  let ctx := context [("items", .array #[.string "a", .string "b", .string "c", .string "d"])]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "b,c"

test "Filter sort - simple" := do
  let tmpl ← shouldBeOk (parse "{{items | sort | join \",\"}}") "parsing"
  let ctx := context [("items", .array #[.string "c", .string "a", .string "b"])]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "a,b,c"

test "Filter uniq" := do
  let tmpl ← shouldBeOk (parse "{{items | uniq | join \",\"}}") "parsing"
  let ctx := context [("items", .array #[.string "a", .string "b", .string "a", .string "c"])]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "a,b,c"

test "Filter map" := do
  let tmpl ← shouldBeOk (parse "{{users | map \"name\" | join \", \"}}") "parsing"
  let ctx := context [("users", .array #[
    .object #[("name", .string "Alice")],
    .object #[("name", .string "Bob")]
  ])]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "Alice, Bob"

test "Filter where" := do
  let tmpl ← shouldBeOk (parse "{{users | where \"active\" | length}}") "parsing"
  let ctx := context [("users", .array #[
    .object #[("name", .string "Alice"), ("active", .bool true)],
    .object #[("name", .string "Bob"), ("active", .bool false)],
    .object #[("name", .string "Carol"), ("active", .bool true)]
  ])]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "2"

test "Filter truncate" := do
  let tmpl ← shouldBeOk (parse "{{text | truncate \"10\"}}") "parsing"
  let ctx := context [("text", .string "Hello, this is a long text")]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "Hello, thi..."

test "Filter truncate with custom ellipsis" := do
  let tmpl ← shouldBeOk (parse "{{text | truncate \"5\" \"---\"}}") "parsing"
  let ctx := context [("text", .string "Hello World")]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "Hello---"

test "Filter replace" := do
  let tmpl ← shouldBeOk (parse "{{text | replace \"world\" \"Lean\"}}") "parsing"
  let ctx := context [("text", .string "Hello world!")]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "Hello Lean!"

test "Filter split" := do
  let tmpl ← shouldBeOk (parse "{{csv | split \",\" | length}}") "parsing"
  let ctx := context [("csv", .string "a,b,c,d")]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "4"

test "Filter number" := do
  let tmpl ← shouldBeOk (parse "{{price | number \"2\"}}") "parsing"
  let ctx := context [("price", .int 42)]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "42.00"

test "Filter pluralize - singular" := do
  let tmpl ← shouldBeOk (parse "{{count}} {{count | pluralize \"item\" \"items\"}}") "parsing"
  let ctx := context [("count", .int 1)]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "1 item"

test "Filter pluralize - plural" := do
  let tmpl ← shouldBeOk (parse "{{count}} {{count | pluralize \"item\" \"items\"}}") "parsing"
  let ctx := context [("count", .int 5)]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "5 items"

test "Filter abs" := do
  let tmpl ← shouldBeOk (parse "{{num | abs}}") "parsing"
  let ctx := context [("num", .int (-42))]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "42"

test "Filter keys" := do
  let tmpl ← shouldBeOk (parse "{{obj | keys | join \",\"}}") "parsing"
  let ctx := context [("obj", .object #[("a", .int 1), ("b", .int 2)])]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "a,b"

test "Filter values" := do
  let tmpl ← shouldBeOk (parse "{{obj | values | join \",\"}}") "parsing"
  let ctx := context [("obj", .object #[("a", .int 1), ("b", .int 2)])]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "1,2"

test "Filter escape_js" := do
  let tmpl ← shouldBeOk (parse "{{{text | escape_js}}}") "parsing"  -- Use raw output
  let ctx := context [("text", .string "say \"hi\"")]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  -- After JS escaping, "say \"hi\"" becomes "say \\\"hi\\\""
  result.render ≡ "say \\\"hi\\\""

test "Filter escape_uri" := do
  let tmpl ← shouldBeOk (parse "{{text | escape_uri}}") "parsing"
  let ctx := context [("text", .string "hello world!")]
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "hello%20world%21"

test "Custom filter registration" := do
  -- Define a custom filter that doubles a number
  let doubleFilter : FilterFn := fun v _ pos =>
    match v with
    | .int n => .ok (.int (n * 2))
    | other => .error (.typeError "double" "Int" other.typeName pos)

  let tmpl ← shouldBeOk (parse "{{num | double}}") "parsing"
  let ctx := context [("num", .int 21)]
    |> (fun c => withFilter c "double" doubleFilter)
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "42"

test "Custom filter overrides builtin" := do
  -- Define a custom uppercase that adds exclamation
  let customUpper : FilterFn := fun v _ pos =>
    match v with
    | .string s => .ok (.string (s.toUpper ++ "!"))
    | other => .error (.typeError "uppercase" "String" other.typeName pos)

  let tmpl ← shouldBeOk (parse "{{text | uppercase}}") "parsing"
  let ctx := context [("text", .string "hello")]
    |> (fun c => withFilter c "uppercase" customUpper)
  let result ← shouldBeOk (render tmpl ctx) "rendering"
  result.render ≡ "HELLO!"

#generate_tests

end Stencil.Tests

def main : IO UInt32 := do
  IO.println "╔════════════════════════════════════════╗"
  IO.println "║        Stencil Test Suite              ║"
  IO.println "╚════════════════════════════════════════╝"
  IO.println ""

  let result ← runAllSuites

  IO.println ""
  if result == 0 then
    IO.println "✓ All tests passed!"
  else
    IO.println "✗ Some tests failed"

  return result
