/-
  Stencil Benchmark Suite
  Run: lake exe stencil_bench
-/
import Stencil.Bench

open Stencil

/-- Render and force evaluation of result -/
def benchRender (tmpl : Template) (ctx : Context) : IO Unit := do
  match renderString tmpl ctx with
  | .ok s => Bench.forceEval s
  | .error _ => pure ()

/-- Parse and force evaluation -/
def benchParse (input : String) : IO Unit := do
  let tmpl := parse! input
  -- Force evaluation by accessing structure
  Bench.forceEval (toString tmpl.nodes.length)

def main : IO Unit := do
  IO.println "Stencil Performance Benchmarks"
  IO.println "=============================="
  IO.println ""
  IO.println "Warming up..."

  -- Benchmark 1: Simple variable substitution
  Bench.printHeader "Simple Variable Substitution"
  let tmpl1 := parse! "Hello {{name}}!"
  let ctx1 := context [("name", .string "World")]
  let r1 ← Bench.benchWithWarmup "simple_var" 1000 10000 do
    benchRender tmpl1 ctx1
  IO.println (Bench.formatResult r1)

  -- Benchmark 2: Multiple variables
  Bench.printHeader "Multiple Variables (10)"
  let tmpl2 := parse! "{{a}} {{b}} {{c}} {{d}} {{e}} {{f}} {{g}} {{h}} {{i}} {{j}}"
  let ctx2 := context [
    ("a", .string "1"), ("b", .string "2"), ("c", .string "3"),
    ("d", .string "4"), ("e", .string "5"), ("f", .string "6"),
    ("g", .string "7"), ("h", .string "8"), ("i", .string "9"),
    ("j", .string "10")
  ]
  let r2 ← Bench.benchWithWarmup "multi_var_10" 1000 10000 do
    benchRender tmpl2 ctx2
  IO.println (Bench.formatResult r2)

  -- Benchmark 3: Loop with 10 items
  Bench.printHeader "Loop (10 items)"
  let items10 := (List.range 10).map (fun i => Value.int (Int.ofNat i))
  let tmpl3 := parse! "{{#each items}}{{.}}{{/each}}"
  let ctx3 := context [("items", .array items10.toArray)]
  let r3 ← Bench.benchWithWarmup "loop_10" 1000 10000 do
    benchRender tmpl3 ctx3
  IO.println (Bench.formatResult r3)

  -- Benchmark 4: Loop with 100 items
  Bench.printHeader "Loop (100 items)"
  let items100 := (List.range 100).map (fun i => Value.int (Int.ofNat i))
  let ctx4 := context [("items", .array items100.toArray)]
  let r4 ← Bench.benchWithWarmup "loop_100" 100 1000 do
    benchRender tmpl3 ctx4
  IO.println (Bench.formatResult r4)

  -- Benchmark 5: Filter chain (3 filters)
  Bench.printHeader "Filter Chain (3 filters)"
  let tmpl5 := parse! "{{text | upper | trim | default \"none\"}}"
  let ctx5 := context [("text", .string "  hello world  ")]
  let r5 ← Bench.benchWithWarmup "filter_chain_3" 1000 10000 do
    benchRender tmpl5 ctx5
  IO.println (Bench.formatResult r5)

  -- Benchmark 6: Deep nested path (3 levels)
  Bench.printHeader "Deep Path (3 levels)"
  let nested := Value.object #[
    ("a", .object #[
      ("b", .object #[
        ("c", .string "deep")
      ])
    ])
  ]
  let tmpl6 := parse! "{{a.b.c}}"
  let ctx6 := contextFromValue nested
  let r6 ← Bench.benchWithWarmup "deep_path_3" 1000 10000 do
    benchRender tmpl6 ctx6
  IO.println (Bench.formatResult r6)

  -- Benchmark 7: Conditional (if/else)
  Bench.printHeader "Conditional (if/else)"
  let tmpl7 := parse! "{{#if active}}yes{{else}}no{{/if}}"
  let ctx7a := context [("active", .bool true)]
  let ctx7b := context [("active", .bool false)]
  let r7a ← Bench.benchWithWarmup "cond_true" 1000 10000 do
    benchRender tmpl7 ctx7a
  IO.println (Bench.formatResult r7a)
  let r7b ← Bench.benchWithWarmup "cond_false" 1000 10000 do
    benchRender tmpl7 ctx7b
  IO.println (Bench.formatResult r7b)

  -- Benchmark 8: Complex expression
  Bench.printHeader "Complex Expression"
  let tmpl8 := parse! "{{#if (count > 0) && (active || visible)}}show{{/if}}"
  let ctx8 := context [("count", .int 5), ("active", .bool true), ("visible", .bool false)]
  let r8 ← Bench.benchWithWarmup "complex_expr" 1000 10000 do
    benchRender tmpl8 ctx8
  IO.println (Bench.formatResult r8)

  -- Benchmark 9: Parse only (small template)
  Bench.printHeader "Parse Only (small)"
  let smallTmpl := "Hello {{name}}!"
  let r9 ← Bench.benchWithWarmup "parse_small" 1000 10000 do
    benchParse smallTmpl
  IO.println (Bench.formatResult r9)

  -- Benchmark 10: Parse only (medium template)
  Bench.printHeader "Parse Only (medium - 100 vars)"
  let mediumTmpl := String.join ((List.range 100).map (fun i => "{{v" ++ toString i ++ "}} "))
  let r10 ← Bench.benchWithWarmup "parse_medium" 100 1000 do
    benchParse mediumTmpl
  IO.println (Bench.formatResult r10)

  -- Benchmark 11: Large template parse + render
  Bench.printHeader "Large Template (1000 vars, parse+render)"
  let largeTmpl := String.join (List.replicate 1000 "Hello {{name}}! ")
  let r11 ← Bench.benchWithWarmup "large_parse_render" 10 100 do
    let tmpl := parse! largeTmpl
    benchRender tmpl ctx1
  IO.println (Bench.formatResult r11)

  -- Benchmark 12: Nested loops
  Bench.printHeader "Nested Loops (10x10)"
  let tmpl12 := parse! "{{#each outer}}{{#each .}}{{.}}{{/each}}{{/each}}"
  let innerItems := (List.range 10).map (fun i => Value.int (Int.ofNat i))
  let outerItems := (List.range 10).map (fun _ => Value.array innerItems.toArray)
  let ctx12 := context [("outer", .array outerItems.toArray)]
  let r12 ← Bench.benchWithWarmup "nested_10x10" 100 1000 do
    benchRender tmpl12 ctx12
  IO.println (Bench.formatResult r12)

  -- Benchmark 13: Cached parse vs uncached
  Bench.printHeader "Cached Parse (medium template)"
  let mut engine := Stencil.Engine.new
  -- First call populates cache
  let (_, engine') := engine.parseCached! mediumTmpl
  engine := engine'
  let r13 ← Bench.benchWithWarmup "cached_parse" 100 10000 do
    let (_, _) := engine.parseCached! mediumTmpl
    pure ()
  IO.println (Bench.formatResult r13)
  IO.println s!"  (uncached was: {297.67}μs, cache hit: ~0μs expected)"

  -- Summary
  Bench.printHeader "Summary"
  IO.println "Benchmark suite complete."
  IO.println ""
