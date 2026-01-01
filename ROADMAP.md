# Stencil Roadmap

Future improvements and features for the Stencil template engine.

## v0.1.0 - Enhanced Filters & Expressions ✅

### Additional Filters
- [x] `slice` - Extract substring or array slice: `{{items | slice "0" "3"}}`
- [x] `sort` - Sort array: `{{items | sort}}` or `{{items | sort "name"}}`
- [x] `uniq` - Remove duplicates from array
- [x] `map` - Extract property from array of objects: `{{users | map "name"}}`
- [x] `where` - Filter array by property: `{{users | where "active" "true"}}`
- [ ] `date` - Format dates: `{{timestamp | date "%Y-%m-%d"}}` (future)
- [x] `number` - Format numbers: `{{price | number "2"}}`
- [x] `pluralize` - Pluralization: `{{count | pluralize "item" "items"}}`
- [x] `truncate` - Truncate with ellipsis: `{{text | truncate "100"}}`
- [x] `replace` - String replacement: `{{text | replace "old" "new"}}`
- [x] `split` - Split string to array: `{{csv | split ","}}`
- [x] `escape_js` - Escape for JavaScript strings
- [x] `escape_uri` - URI encoding
- [x] `abs` - Absolute value (bonus)
- [x] `keys` - Get object keys as array (bonus)
- [x] `values` - Get object values as array (bonus)

### Custom Filter Registration
- [x] API for registering custom filters at runtime (`withFilter`)
- [x] Filter validation and error handling
- [ ] Filter documentation/introspection (future)

## v0.2.0 - Conditional Expressions

### Expression Support
- [ ] Comparison operators in conditions: `{{#if count > 0}}`
- [ ] Logical operators: `{{#if active && visible}}`
- [ ] Negation: `{{#if !hidden}}`
- [ ] Equality checks: `{{#if status == "active"}}`
- [ ] Numeric comparisons: `{{#if age >= 18}}`

### Enhanced Conditionals
- [ ] `{{else if condition}}` chains
- [ ] `{{#switch}}` / `{{#case}}` blocks
- [ ] Inline conditionals: `{{if condition then "yes" else "no"}}`

## v0.3.0 - Whitespace Control

### Whitespace Trimming
- [ ] `{{~` and `~}}` for whitespace trimming (Handlebars-style)
- [ ] `{{-` and `-}}` alternative syntax
- [ ] Standalone tag detection (remove surrounding newlines)
- [ ] Configuration option for default whitespace handling

## v0.4.0 - Block Helpers

### Custom Block Helpers
- [ ] `{{#with user}}...{{/with}}` - Change context
- [ ] `{{#let x=value}}...{{/let}}` - Local variables
- [ ] `{{#repeat 5}}...{{/repeat}}` - Repeat content
- [ ] `{{#range 1 10}}...{{/range}}` - Numeric iteration
- [ ] Custom block helper registration API

### Enhanced Each
- [ ] `{{#each items as |item index|}}` - Named iteration variables
- [ ] `{{@key}}` for object iteration
- [ ] `{{@length}}` for array length
- [ ] `{{else}}` for empty arrays (already supported)

## v0.5.0 - Template Composition

### Template Inheritance
- [ ] `{{#extends "base"}}` - Extend parent template
- [ ] `{{#block "content"}}...{{/block}}` - Define blocks
- [ ] `{{#super}}` - Call parent block

### Partial Enhancements
- [ ] Partial parameters: `{{> card title="Hello"}}`
- [ ] Dynamic partials: `{{> (lookup templates type)}}`
- [ ] Inline partials: `{{#*inline "myPartial"}}...{{/inline}}`
- [ ] Partial blocks: `{{#> layout}}content{{/layout}}`

## v0.6.0 - Error Handling & Debugging ✅

### Better Error Messages
- [x] Include line/column in all error messages
- [x] Source snippets in error output
- [x] Suggestions for common mistakes (Levenshtein distance)
- [ ] Stack traces for nested template errors (future)

### Debugging Support
- [ ] `{{log variable}}` - Debug output (future)
- [ ] `{{debug}}` - Dump current context (future)
- [ ] Template source maps (future)
- [ ] Render timing/profiling (future)

## v0.7.0 - Performance

### Compilation
- [ ] Pre-compiled template representation
- [ ] Template caching
- [ ] Lazy partial loading

### Optimization
- [ ] String builder for render output
- [ ] Avoid intermediate allocations
- [ ] Benchmark suite

## v0.8.0 - Integration

### Framework Integration
- [ ] Loom middleware for automatic template rendering
- [ ] Template discovery from filesystem
- [ ] Hot reloading for development

### Tooling
- [ ] Syntax highlighting definitions (VSCode, etc.)
- [ ] Template linter
- [ ] CLI tool for template validation

## Future Considerations

### Potential Features
- [ ] Async rendering for IO-based data
- [ ] Streaming output for large templates
- [ ] Template security sandboxing
- [ ] i18n/localization helpers
- [ ] Markdown filter integration

### Compatibility
- [ ] Mustache spec compliance tests
- [ ] Handlebars compatibility mode
- [ ] Migration guide from other engines

## Non-Goals

Things intentionally not planned:
- **Logic-heavy templates**: Keep templates declarative, complex logic belongs in Lean
- **JavaScript execution**: No embedded scripting
- **Template compilation to Lean**: Focus on runtime interpretation

## Contributing

Contributions welcome! Priority areas:
1. Additional filters (most impactful, lowest risk)
2. Whitespace control (frequently requested)
3. Better error messages (improves developer experience)
