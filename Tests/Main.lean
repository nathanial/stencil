/-
  Stencil Tests
-/
import Crucible
import Stencil

namespace Stencil.Tests

open Crucible
open Stencil

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
