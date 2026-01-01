# Stencil Performance Benchmarks

Run benchmarks with: `lake exe stencil_bench`

## Current Results (v0.7.0)

Measured on Apple Silicon (M-series), Lean 4.26.0.

| Benchmark | Avg Time | Iterations | vs Baseline |
|-----------|----------|------------|-------------|
| simple_var | 0.46μs | 10,000 | -12% |
| multi_var_10 | 3.12μs | 10,000 | -9% |
| loop_10 | 2.44μs | 10,000 | - |
| loop_100 | 22.23μs | 1,000 | -5% |
| filter_chain_3 | 26.70μs | 10,000 | - |
| deep_path_3 | 0.42μs | 10,000 | **-23%** |
| cond_true | 0.39μs | 10,000 | - |
| cond_false | 0.37μs | 10,000 | - |
| complex_expr | 0.67μs | 10,000 | - |
| parse_small | 3.71μs | 10,000 | - |
| parse_medium | 308μs | 1,000 | - |
| large_parse_render | 341μs | 100 | **-16%** |
| nested_10x10 | 22.28μs | 1,000 | -3% |
| **cached_parse** | **0.01μs** | 10,000 | **30,000x faster** |

## Optimizations Applied (v0.7.0)

1. **Precompiled paths** - Paths like `"a.b.c"` split at parse time, not render
   - Deep paths 23% faster

2. **String extraction** - Parser uses `String.extract` instead of char-by-char
   - Changed O(n²) to O(n) for text parsing

3. **Template caching** - `Engine.parseCached!` caches by content hash
   - 30,000x faster for repeated parses (0.01μs vs 308μs)

## Baseline Results (v0.6.0)

| Benchmark | Avg Time | Notes |
|-----------|----------|-------|
| simple_var | 0.52μs | Single `{{name}}` |
| multi_var_10 | 3.44μs | 10 variables |
| deep_path_3 | 0.52μs | `{{a.b.c}}` |
| parse_medium | 297.68μs | Parse 100 variables |
| large_parse_render | 404.45μs | Parse+render 1000 vars |

## Observations

### Performance Characteristics

1. **Rendering is fast** - Simple variable render is ~0.5μs
2. **Loops scale linearly** - 10 items: 2.4μs, 100 items: 22.2μs
3. **Deep paths are efficient** - 0.42μs (23% faster with precompiled paths)
4. **Conditionals are fast** - ~0.4μs regardless of branch
5. **Caching is essential** - 30,000x speedup for repeated templates

### Remaining Bottlenecks

1. **Filter chains** - 26.70μs for 3 filters (still expensive)
2. **Initial parse** - ~308μs for complex templates (use caching!)

### Future Optimization Opportunities

1. **Filter composition** - Pre-compose filter chains at parse time
2. **String builder in Html.render** - Buffer-based output
3. **Lazy partial loading** - Defer parsing until first use

## Test Cases

| Test | Template | Context |
|------|----------|---------|
| simple_var | `Hello {{name}}!` | `{name: "World"}` |
| multi_var_10 | `{{a}} {{b}} ... {{j}}` | 10 string values |
| loop_10/100 | `{{#each items}}{{.}}{{/each}}` | Array of ints |
| filter_chain_3 | `{{text \| upper \| trim \| default "none"}}` | String value |
| deep_path_3 | `{{a.b.c}}` | Nested object |
| cond_* | `{{#if active}}yes{{else}}no{{/if}}` | Boolean |
| complex_expr | `{{#if (count > 0) && (active \|\| visible)}}show{{/if}}` | Multiple values |
| parse_* | Various sizes | N/A (parse only) |
| nested_10x10 | `{{#each outer}}{{#each .}}{{.}}{{/each}}{{/each}}` | 10x10 nested arrays |
