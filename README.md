# Stencil

Mustache/Handlebars-style template engine for Lean 4, outputting Scribe Html.

## Features

- **Mustache-compatible syntax**: `{{variable}}`, `{{#if}}`, `{{#each}}`, `{{> partial}}`
- **HTML escaping by default**: Variables are escaped, use `{{{raw}}}` for unescaped
- **Filters**: Transform values with `{{name | uppercase | trim}}`
- **Scribe integration**: Outputs `Scribe.Html` for web framework compatibility
- **Type-safe**: Fully typed AST and render context

## Installation

Add to your `lakefile.lean`:

```lean
require stencil from git "https://github.com/nathanial/stencil" @ "v0.0.1"
```

## Usage

```lean
import Stencil
import Scribe

open Stencil

def main : IO Unit := do
  -- Parse template
  let tmpl ← parse "Hello, {{name | uppercase}}!" |>.toIO (fun e => IO.userError s!"{e}")

  -- Create context
  let ctx := context [("name", .string "world")]

  -- Render to Html
  let html ← render tmpl ctx |>.toIO (fun e => IO.userError s!"{e}")

  IO.println html.render  -- "Hello, WORLD!"
```

## Syntax Reference

| Syntax | Example | Description |
|--------|---------|-------------|
| Variable | `{{name}}` | Escaped output |
| Raw | `{{{html}}}` or `{{& html}}` | Unescaped output |
| Dot notation | `{{user.profile.name}}` | Nested access |
| Filter | `{{name \| uppercase}}` | Transform value |
| Comment | `{{! comment }}` | Ignored in output |
| If | `{{#if x}}...{{/if}}` | Conditional |
| Unless | `{{#unless x}}...{{/unless}}` | Inverted conditional |
| Else | `{{#if x}}...{{else}}...{{/if}}` | Alternative branch |
| Each | `{{#each items}}...{{/each}}` | Loop iteration |
| Loop vars | `{{this}}`, `{{@index}}`, `{{@first}}`, `{{@last}}` | Loop context |
| Partial | `{{> header}}` | Include template |

## Built-in Filters

| Filter | Example | Description |
|--------|---------|-------------|
| `uppercase` | `{{name \| uppercase}}` | Convert to uppercase |
| `lowercase` | `{{name \| lowercase}}` | Convert to lowercase |
| `trim` | `{{text \| trim}}` | Trim whitespace |
| `length` | `{{items \| length}}` | Array/string length |
| `default` | `{{name \| default "N/A"}}` | Fallback value |
| `join` | `{{items \| join ", "}}` | Join array |
| `first` | `{{items \| first}}` | First element |
| `last` | `{{items \| last}}` | Last element |
| `reverse` | `{{items \| reverse}}` | Reverse array/string |
| `json` | `{{data \| json}}` | JSON encode |
| `capitalize` | `{{name \| capitalize}}` | Capitalize first char |

## Value Types

```lean
inductive Value where
  | null
  | bool (val : Bool)
  | int (val : Int)
  | float (val : Float)
  | string (val : String)
  | array (val : Array Value)
  | object (val : Array (String × Value))
```

## Partials

```lean
let headerTmpl ← parse "<header>{{title}}</header>"
let mainTmpl ← parse "{{> header}}<main>{{content}}</main>"

let ctx := context [("title", .string "My Page"), ("content", .string "Hello")]
  |>.addPartial "header" headerTmpl

let html ← render mainTmpl ctx
```

## Dependencies

- [scribe](https://github.com/nathanial/scribe) - HTML builder
- [crucible](https://github.com/nathanial/crucible) - Test framework

## License

MIT
