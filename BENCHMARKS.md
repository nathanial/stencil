# Stencil Performance Benchmarks

Run benchmarks with: `lake exe stencil_bench`

## Baseline Results (v0.6.0)

Measured on Apple Silicon (M-series), Lean 4.26.0.

| Benchmark | Avg Time | Iterations | Notes |
|-----------|----------|------------|-------|
| simple_var | 0.52μs | 10,000 | Single `{{name}}` |
| multi_var_10 | 3.44μs | 10,000 | 10 variables |
| loop_10 | 2.40μs | 10,000 | `{{#each}}` with 10 items |
| loop_100 | 23.49μs | 1,000 | `{{#each}}` with 100 items |
| filter_chain_3 | 26.88μs | 10,000 | 3 chained filters |
| deep_path_3 | 0.52μs | 10,000 | `{{a.b.c}}` |
| cond_true | 0.38μs | 10,000 | `{{#if}}` true branch |
| cond_false | 0.37μs | 10,000 | `{{#if}}` false branch |
| complex_expr | 0.64μs | 10,000 | `{{#if (a > 0) && (b \|\| c)}}` |
| parse_small | 3.64μs | 10,000 | Parse `Hello {{name}}!` |
| parse_medium | 297.68μs | 1,000 | Parse 100 variables |
| large_parse_render | 404.45μs | 100 | Parse+render 1000 vars |
| nested_10x10 | 23.07μs | 1,000 | Nested loops 10x10 |

## Observations

### Performance Characteristics

1. **Rendering is fast** - Simple variable render is ~0.5μs
2. **Loops scale linearly** - 10 items: 2.4μs, 100 items: 23.5μs
3. **Deep paths are efficient** - Same cost as simple variable
4. **Conditionals are fast** - ~0.4μs regardless of branch

### Bottlenecks Identified

1. **Parsing is slow** - 297μs for 100 vars suggests O(n²) string ops
2. **Filter chains are expensive** - 26.88μs for 3 filters (52x simple var)

### Optimization Priorities

Based on benchmark data:

1. **Precompiled paths** - Split `"a.b.c"` at parse time, not render time
2. **Filter composition** - Pre-compose filter chains to reduce lookups
3. **Template caching** - Cache parsed AST by content hash
4. **String builder** - Use buffer for Html.render

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
