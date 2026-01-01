/-
  Stencil.Bench
  Benchmark infrastructure for measuring template performance
-/
import Stencil

namespace Stencil.Bench

/-- Result of a single benchmark run -/
structure BenchResult where
  name : String
  iterations : Nat
  totalNs : Nat
  avgNs : Float
  deriving Repr

/-- Format a benchmark result for display -/
def formatResult (r : BenchResult) : String :=
  let avgUs := r.avgNs / 1000.0
  let totalMs := r.totalNs.toFloat / 1_000_000.0
  s!"{r.name}: {avgUs.toString}μs avg ({r.iterations} iters, {totalMs.toString}ms total)"

/-- Dummy variable to prevent optimization -/
@[noinline]
unsafe def blackholeImpl : IO.Ref String := unsafeBaseIO (IO.mkRef "")
@[implemented_by blackholeImpl]
opaque blackhole : IO.Ref String

/-- Force evaluation of a string result -/
def forceEval (s : String) : IO Unit := do
  blackhole.set s

/-- Run an action n times and measure total time -/
def bench (name : String) (iterations : Nat) (action : IO Unit) : IO BenchResult := do
  let start ← IO.monoNanosNow
  for _ in [:iterations] do
    action
  let stop ← IO.monoNanosNow
  let total := stop - start
  return {
    name
    iterations
    totalNs := total
    avgNs := total.toFloat / iterations.toFloat
  }

/-- Run an action n times with warmup iterations -/
def benchWithWarmup (name : String) (warmup : Nat) (iterations : Nat) (action : IO Unit) : IO BenchResult := do
  -- Warmup phase (not measured)
  for _ in [:warmup] do
    action
  -- Measured phase
  bench name iterations action

/-- Print a separator line -/
def printSeparator : IO Unit :=
  IO.println (String.mk (List.replicate 60 '-'))

/-- Print benchmark header -/
def printHeader (title : String) : IO Unit := do
  IO.println ""
  printSeparator
  IO.println s!"  {title}"
  printSeparator

end Stencil.Bench
