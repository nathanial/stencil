/-
  Stencil.Core.Suggest
  Fuzzy matching and suggestion helpers for error messages
-/

namespace Stencil

/-- Simple Levenshtein edit distance using Wagner-Fischer algorithm -/
def levenshtein (s1 s2 : String) : Nat :=
  let a := s1.toList.toArray
  let b := s2.toList.toArray
  let m := a.size
  let n := b.size
  if m == 0 then n
  else if n == 0 then m
  else
    -- Create initial row [0, 1, 2, ..., n]
    let row0 : Array Nat := Array.range (n + 1)
    -- Process each character of s1
    let finalRow := a.foldl (init := (row0, 0)) fun (prevRow, i) c1 =>
      let newRow := b.foldl (init := (#[i + 1], 0)) fun (row, j) c2 =>
        let cost := if c1 == c2 then 0 else 1
        let deleteCost := prevRow[j + 1]! + 1
        let insertCost := row[j]! + 1
        let replaceCost := prevRow[j]! + cost
        let minCost := Nat.min deleteCost (Nat.min insertCost replaceCost)
        (row.push minCost, j + 1)
      (newRow.1, i + 1)
    finalRow.1[n]!

/-- Find similar strings from a list of candidates -/
def findSimilar (name : String) (candidates : List String) (maxDistance : Nat := 2) : List String :=
  candidates.filterMap fun candidate =>
    let dist := levenshtein name.toLower candidate.toLower
    if dist > 0 && dist <= maxDistance then some candidate else none

/-- Find the best match from candidates (lowest edit distance) -/
def findBestMatch (name : String) (candidates : List String) (maxDistance : Nat := 2) : Option String :=
  let similar := candidates.filterMap fun candidate =>
    let dist := levenshtein name.toLower candidate.toLower
    if dist > 0 && dist <= maxDistance then some (candidate, dist) else none
  -- Find minimum by folding
  match similar with
  | [] => none
  | first :: rest =>
    let best := rest.foldl (fun (bestName, bestDist) (name, dist) =>
      if dist < bestDist then (name, dist) else (bestName, bestDist)
    ) first
    some best.1

/-- Known filter names for suggestions -/
def builtinFilterNames : List String :=
  -- String filters
  ["uppercase", "lowercase", "capitalize", "trim", "truncate", "replace", "split",
   -- Array/collection filters
   "length", "first", "last", "reverse", "join", "slice", "sort", "uniq", "map", "where",
   -- Object filters
   "keys", "values",
   -- Numeric filters
   "number", "abs", "pluralize",
   -- Utility filters
   "default", "json",
   -- Escape filters
   "escape_js", "escape_uri"]

/-- Suggest a similar filter name -/
def suggestFilter (name : String) : Option String :=
  findBestMatch name builtinFilterNames

end Stencil
