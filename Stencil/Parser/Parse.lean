/-
  Stencil.Parser.Parse
  Template parsing using Sift parser combinators
-/
import Stencil.Parser.Primitives
import Stencil.AST.Types

namespace Stencil.Parser

open Sift

/-- Parse filter arguments (quoted strings after filter name) -/
def parseFilterArgs : Parser (List String) := do
  let mut args := []
  skipWhitespace
  while true do
    match ← peek with
    | some '"' | some '\'' =>
      let arg ← parseQuotedString
      args := args ++ [arg]
      skipWhitespace
    | _ => break
  return args

/-- Parse a single filter: `| filtername "arg"` -/
def parseFilter : Parser Filter := do
  skipWhitespace
  let name ← readWhile1 isFilterNameChar "filter name"
  let args ← parseFilterArgs
  return { name, args }

/-- Parse filter chain: `| filter1 | filter2 "arg"` -/
def parseFilters : Parser (List Filter) := do
  let mut filters := []
  skipWhitespace
  while ← Parser.tryChar '|' do
    skipWhitespace
    let filter ← parseFilter
    filters := filters ++ [filter]
    skipWhitespace
  return filters

/-- Count and consume leading ../ segments for parent context access -/
def parseParentPrefix : Parser Nat := do
  let mut levels : Nat := 0
  while true do
    -- Check for "../"
    let ahead ← Parser.peekString 3
    if ahead.startsWith "../" then
      let _ ← Parser.tryString "../"
      levels := levels + 1
    else
      break
  return levels

/-- Parse a variable path with optional filters -/
def parseVarRef (escaped : Bool) (pos : Position) : Parser VarRef := do
  skipWhitespace
  -- Check for parent path prefix: ../
  let parentLevels ← parseParentPrefix
  let path ← readWhile1 isPathChar "variable path"
  -- Pre-split path for faster lookup at render time
  let pathParts := path.splitOn "." |>.filter (!·.isEmpty)
  let filters ← parseFilters
  skipWhitespace
  return { path, pathParts, filters, escaped, pos, parentLevels }

/-- Check for opening delimiter and detect type -/
def checkOpenDelim : Parser Bool := do
  let ahead ← Parser.peekString 2
  return ahead == "{{"

/-- Check if character is a trim marker (~ or -) -/
def isTrimMarker (c : Char) : Bool := c == '~' || c == '-'

/-- Try to consume a trim marker at start of tag -/
def tryTrimStart : Parser Bool := do
  match ← peek with
  | some c => if isTrimMarker c then let _ ← anyChar; return true else return false
  | none => return false

/-- Try to consume a trim marker before closing delimiter -/
def tryTrimEnd : Parser Bool := do
  match ← peek with
  | some c => if isTrimMarker c then let _ ← anyChar; return true else return false
  | none => return false

/-- Parse a comment: `{{! ... }}` -/
def parseComment : Parser Node := do
  let content ← readUntil "}}"
  let _ ← Parser.tryString "}}"
  return .comment content.trim

/-- Parse a closing tag: `{{/name}}` (with optional trim markers) -/
def parseCloseTag : Parser String := do
  skipWhitespace
  let name ← readWhile1 isIdentChar "tag name"
  skipWhitespace
  -- Check for trim marker before }}
  let trimAfter ← tryTrimEnd
  let _ ← Parser.tryString "}}"
  if trimAfter then Parser.setTrimNext true
  return name

/-- Parse text content until a tag or end -/
partial def parseText : Parser (Option Node) := do
  -- Track start position for efficient substring extraction
  let s ← Sift.Parser.get
  let startPos := s.pos
  while true do
    if ← Parser.atEnd then break
    if ← checkOpenDelim then break
    let _ ← anyChar
  let s' ← Sift.Parser.get
  let endPos := s'.pos
  if startPos == endPos then
    return none
  -- Extract substring directly (O(n) instead of O(n²))
  let content := s.input.extract ⟨startPos⟩ ⟨endPos⟩
  return some (.text content)

-- Expression parsing for conditionals

/-- Check if character is valid for a number -/
private def isNumberChar (c : Char) : Bool := c.isDigit || c == '.' || c == '-'

/-- Parse a float from string (manual since String.toFloat? doesn't exist) -/
private def parseFloatString (s : String) : Float :=
  let negative := s.startsWith "-"
  let s' := if negative then s.drop 1 else s
  let (intStr, fracStr) := match s'.splitOn "." with
    | [i, f] => (i, f)
    | [i] => (i, "")
    | _ => ("0", "0")
  let intVal := intStr.toNat!.toFloat
  let fracVal := if fracStr.isEmpty then 0.0
                 else fracStr.toNat!.toFloat / Float.pow 10.0 fracStr.length.toFloat
  let result := intVal + fracVal
  if negative then -result else result

/-- Parse an integer or float literal -/
private def parseNumber : Parser Expr := do
  let numStr ← readWhile1 isNumberChar "number"
  if numStr.any (· == '.') then
    return .floatLit (parseFloatString numStr)
  else
    match numStr.toInt? with
    | some n => return .intLit n
    | none =>
      Sift.Parser.fail s!"invalid number: {numStr}"

/-- Parse a string literal -/
private def parseStringLit : Parser Expr := do
  let s ← parseQuotedString
  return .strLit s

/-- Try to parse a comparison operator -/
private def parseCompareOp : Parser (Option CompareOp) := do
  skipWhitespace
  let ahead ← Parser.peekString 2
  if ahead == "==" then
    let _ ← Parser.tryString "=="
    return some .eq
  else if ahead == "!=" then
    let _ ← Parser.tryString "!="
    return some .ne
  else if ahead == "<=" then
    let _ ← Parser.tryString "<="
    return some .le
  else if ahead == ">=" then
    let _ ← Parser.tryString ">="
    return some .ge
  else
    match ahead.get? ⟨0⟩ with
    | some '<' =>
      let _ ← anyChar
      return some .lt
    | some '>' =>
      let _ ← anyChar
      return some .gt
    | _ => return none

-- Expression parsing is mutually recursive
mutual
  /-- Parse helper call arguments (expressions separated by whitespace until ')') -/
  partial def parseHelperArgs : Parser (List Expr) := do
    let mut args : List Expr := []
    while true do
      skipWhitespace
      match ← peek with
      | some ')' => break
      | some _ =>
        let arg ← parseAtom
        args := args ++ [arg]
      | none => break
    return args

  /-- Parse an atom (variable, literal, parenthesized expression, or helper call) -/
  partial def parseAtom : Parser Expr := do
    skipWhitespace
    match ← peek with
    | some '"' | some '\'' =>
      parseStringLit
    | some c =>
      if c.isDigit || c == '-' then
        parseNumber
      else if c == '(' then
        let _ ← anyChar  -- consume '('
        skipWhitespace
        -- Check if this is a helper call: (helperName arg1 arg2)
        -- or a grouping expression: (expr)
        -- Strategy: try to read an identifier, then see what follows
        let savedState ← Sift.Parser.get
        match ← peek with
        | some ch =>
          if isIdentChar ch then
            -- Could be a helper name or start of an expression
            let ident ← readWhile isIdentChar
            skipWhitespace
            match ← peek with
            | some ')' =>
              -- Just (identifier), treat as grouping of a variable
              let _ ← anyChar -- consume ')'
              return .var ident 0
            | some op =>
              if op == '=' || op == '!' || op == '<' || op == '>' || op == '&' || op == '|' then
                -- It's an expression like (x == y), restore and parse as grouping
                Sift.Parser.set savedState
                let _ ← anyChar  -- consume '(' again
                let expr ← parseOr
                skipWhitespace
                match ← peek with
                | some ')' => let _ ← anyChar
                | _ => Sift.Parser.fail "expected ')'"
                return expr
              else
                -- It's a helper call: (helperName arg1 arg2 ...)
                let args ← parseHelperArgs
                skipWhitespace
                match ← peek with
                | some ')' => let _ ← anyChar
                | _ => Sift.Parser.fail "expected ')'"
                return .call ident args
            | none => Sift.Parser.fail "unexpected end of input in expression"
          else
            -- Starts with non-identifier, parse as grouping expression
            let expr ← parseOr
            skipWhitespace
            match ← peek with
            | some ')' => let _ ← anyChar
            | _ => Sift.Parser.fail "expected ')'"
            return expr
        | none => Sift.Parser.fail "unexpected end of input in expression"
      else if c == '!' then
        let _ ← anyChar
        let inner ← parseAtom
        return .not inner
      else if c == '.' then
        -- Could be parent path: ../
        let parentLevels ← parseParentPrefix
        if parentLevels > 0 then
          -- Parse the rest of the path
          let name ← readWhile isPathChar
          if name.isEmpty then
            -- Just ".." with no following path, use "this" as default
            return .var "this" parentLevels
          else
            return .var name parentLevels
        else
          -- Just a dot, not a parent path
          Sift.Parser.fail s!"unexpected '.', expected expression"
      else
        -- Variable or keyword
        let name ← readWhile isPathChar
        if name.isEmpty then
          Sift.Parser.fail s!"unexpected '{c}', expected expression"
        match name with
        | "true" => return .boolLit true
        | "false" => return .boolLit false
        | _ => return .var name 0
    | none => Sift.Parser.fail "unexpected end of input in expression"

  /-- Parse comparison expression: `a == b`, `a > b`, etc. -/
  partial def parseComparison : Parser Expr := do
    let left ← parseAtom
    match ← parseCompareOp with
    | some op =>
      let right ← parseAtom
      return .compare op left right
    | none => return left

  /-- Parse logical AND expression: `a && b` -/
  partial def parseAnd : Parser Expr := do
    let mut left ← parseComparison
    while true do
      skipWhitespace
      let ahead ← Parser.peekString 2
      if ahead == "&&" then
        let _ ← Parser.tryString "&&"
        let right ← parseComparison
        left := .logic .and left right
      else
        break
    return left

  /-- Parse logical OR expression: `a || b` -/
  partial def parseOr : Parser Expr := do
    let mut left ← parseAnd
    while true do
      skipWhitespace
      let ahead ← Parser.peekString 2
      if ahead == "||" then
        let _ ← Parser.tryString "||"
        let right ← parseAnd
        left := .logic .or left right
      else
        break
    return left
end

/-- Parse a complete expression -/
def parseExpr : Parser Expr := parseOr

/-- Parse condition until closing `}}` -/
def parseCondition : Parser Expr := do
  skipWhitespace
  let expr ← parseExpr
  skipWhitespace
  return expr

/-- Parse partial hash parameters: `key=value key2="string"` -/
def parsePartialHashParams : Parser (List (String × Expr)) := do
  let mut params : List (String × Expr) := []
  while true do
    skipWhitespace
    match ← peek with
    | some '}' => break
    | some c =>
      if isIdentChar c then
        -- Save state to backtrack if needed
        let savedState ← Sift.Parser.get
        let key ← readWhile1 isIdentChar "parameter name"
        skipWhitespace
        if ← Parser.tryChar '=' then
          skipWhitespace
          let value ← parseAtom
          params := params ++ [(key, value)]
        else
          -- Not a hash param (no =), backtrack and stop
          Sift.Parser.set savedState
          break
      else
        break
    | none => break
  return params

/-- Parse partial context and hash params.
    Syntax: `{{> name context hash...}}` where context is optional.
    Context is a single expression (path/literal) that becomes the partial's context.
    Hash params are key=value pairs that get merged into the context.
    Example: `{{> partial user}}` - context is user
    Example: `{{> partial user name=user.name}}` - context is user, name is merged
    Example: `{{> partial name=value}}` - no context change, name is merged -/
def parsePartialArgs : Parser (Option Expr × List (String × Expr)) := do
  skipWhitespace
  match ← peek with
  | some '}' => return (none, [])
  | some c =>
    if isIdentChar c then
      -- Could be a context variable/path or a hash param key
      let savedState ← Sift.Parser.get
      let ident ← readWhile1 isIdentChar "identifier"
      skipWhitespace
      if ← Parser.tryChar '=' then
        -- It's a hash param, backtrack and parse as hash
        Sift.Parser.set savedState
        let hash ← parsePartialHashParams
        return (none, hash)
      else
        -- Check for path continuation (dots)
        let mut fullPath := ident
        while (← peek) == some '.' do
          let _ ← anyChar
          let segment ← readWhile1 isIdentChar "path segment"
          fullPath := fullPath ++ "." ++ segment
        skipWhitespace
        -- This is a context expression, now parse remaining hash params
        let contextExpr := Expr.var fullPath 0
        let hash ← parsePartialHashParams
        return (some contextExpr, hash)
    else if c == '"' || c == '\'' then
      -- String literal as context
      let str ← parseQuotedString
      skipWhitespace
      let hash ← parsePartialHashParams
      return (some (.strLit str), hash)
    else if c.isDigit then
      -- Number literal as context
      let numExpr ← parseNumber
      skipWhitespace
      let hash ← parsePartialHashParams
      return (some numExpr, hash)
    else
      return (none, [])
  | none => return (none, [])

/-- Parse a partial with optional context and parameters: `{{> name context key=value}}` -/
def parsePartial (pos : Position) : Parser Node := do
  skipWhitespace
  let name ← readWhile1 isPartialNameChar "partial name"
  let (context, params) ← parsePartialArgs
  skipWhitespace
  let _ ← Parser.tryString "}}"
  return .«partial» name context params pos

/-- Trim trailing whitespace from the last text node in a list -/
private def trimLastNodeRight (nodes : List Node) : List Node :=
  match nodes.reverse with
  | .text s :: rest => (.text s.trimRight :: rest).reverse
  | _ => nodes

-- Mutually recursive parsing functions
mutual
  /-- Parse a conditional (if/unless) with optional else-if chains -/
  partial def parseConditional (startPos : Position) (inverted : Bool) (tagName : String) (_trimBefore : Bool) : Parser Node := do
    -- Parse the initial condition
    let condition ← parseCondition
    skipWhitespace
    let trimAfter ← tryTrimEnd
    let _ ← Parser.tryString "}}"
    if trimAfter then Parser.setTrimNext true

    -- Push tag for matching (use the actual tag name: "if" or "unless")
    Parser.pushTag tagName

    -- Collect all branches (if + else-if chains)
    let mut branches : List (Expr × List Node) := []
    let mut currentCond := condition
    let mut done := false
    let mut elseBody : List Node := []

    while !done do
      -- Parse body until {{else}}, {{else if}}, or {{/tagName}}
      let (body, foundTag) ← parseNodes ["else", tagName]
      branches := branches ++ [(currentCond, body)]

      match foundTag with
      | some "else" =>
        -- Check if it's {{else if ...}} or just {{else}}
        -- We need to peek ahead to see if there's an "if"
        let s ← Sift.Parser.get
        skipWhitespace
        let maybeIf ← readWhile Char.isAlpha
        if maybeIf == "if" then
          -- It's an else-if, parse the new condition
          skipWhitespace
          currentCond ← parseCondition
          let _ ← Parser.tryString "}}"
        else
          -- It's just else, restore state and parse else body
          Sift.Parser.set s
          let (elsePart, _) ← parseNodes [tagName]
          elseBody := elsePart
          done := true
      | _ =>
        -- Hit closing tag or end
        done := true

    -- Pop the tag
    let _ ← Parser.popTag

    return .conditional branches elseBody inverted startPos

  /-- Parse a section opening: `{{#if condition}}` or `{{#each items}}` etc -/
  partial def parseSection (startPos : Position) (trimBefore : Bool) : Parser Node := do
    skipWhitespace
    let blockType ← readWhile1 Char.isAlpha "block type"
    skipWhitespace

    match blockType with
    | "if" => parseConditional startPos false "if" trimBefore
    | "unless" => parseConditional startPos true "unless" trimBefore
    | "extends" =>
      -- Template inheritance: {{#extends "base"}}
      skipWhitespace
      let name ← if ← Parser.tryChar '"' then
        let s ← readUntil "\""
        let _ ← Parser.tryChar '"'
        pure s
      else if ← Parser.tryChar '\'' then
        let s ← readUntil "'"
        let _ ← Parser.tryChar '\''
        pure s
      else
        readWhile1 isIdentChar "template name"
      skipWhitespace
      let trimAfter ← tryTrimEnd
      let _ ← Parser.tryString "}}"
      if trimAfter then Parser.setTrimNext true
      return .extends name startPos
    | "block" =>
      -- Named block: {{#block "content"}}...{{/block}}
      skipWhitespace
      let name ← if ← Parser.tryChar '"' then
        let s ← readUntil "\""
        let _ ← Parser.tryChar '"'
        pure s
      else if ← Parser.tryChar '\'' then
        let s ← readUntil "'"
        let _ ← Parser.tryChar '\''
        pure s
      else
        readWhile1 isIdentChar "block name"
      skipWhitespace
      let trimAfter ← tryTrimEnd
      let _ ← Parser.tryString "}}"
      if trimAfter then Parser.setTrimNext true
      Parser.pushTag "block"
      let (body, _) ← parseNodes ["block"]
      let _ ← Parser.popTag
      return .block name body startPos
    | "super" =>
      -- Call parent block: {{#super}}
      let trimAfter ← tryTrimEnd
      let _ ← Parser.tryString "}}"
      if trimAfter then Parser.setTrimNext true
      return .super startPos
    | "each" =>
      -- Parse: {{#each items}} or {{#each items as |item idx|}}
      skipWhitespace
      let source ← readWhile1 isPathChar "variable path"
      skipWhitespace

      -- Check for "as |item idx|" syntax
      let (itemVar, indexVar) ← if ← Parser.tryString "as" then
        skipWhitespace
        if ← Parser.tryChar '|' then
          skipWhitespace
          let item ← readWhile1 isIdentChar "item variable"
          skipWhitespace
          let idx ← do
            match ← peek with
            | some c =>
              if isIdentChar c then
                let i ← readWhile1 isIdentChar "index variable"
                pure (some i)
              else
                pure none
            | none => pure none
          skipWhitespace
          let _ ← Parser.tryChar '|'
          pure (some item, idx)
        else
          pure (none, none)
      else
        pure (none, none)

      skipWhitespace
      let trimAfter ← tryTrimEnd
      let _ ← Parser.tryString "}}"
      if trimAfter then Parser.setTrimNext true

      let config : EachConfig := { source, itemVar, indexVar }

      Parser.pushTag "each"
      let (body, foundTag) ← parseNodes ["else", "each"]

      let elseBody ← match foundTag with
        | some "else" =>
          let (elsePart, _) ← parseNodes ["each"]
          pure elsePart
        | _ => pure []

      let _ ← Parser.popTag
      return .each config body elseBody startPos

    | "with" =>
      -- Parse: {{#with user}}...{{/with}}
      skipWhitespace
      let path ← readWhile1 isPathChar "variable path"
      skipWhitespace
      let trimAfter ← tryTrimEnd
      let _ ← Parser.tryString "}}"
      if trimAfter then Parser.setTrimNext true

      Parser.pushTag "with"
      let (body, foundTag) ← parseNodes ["else", "with"]

      let elseBody ← match foundTag with
        | some "else" =>
          let (elsePart, _) ← parseNodes ["with"]
          pure elsePart
        | _ => pure []

      let _ ← Parser.popTag
      return .«with» path body elseBody startPos

    | "let" =>
      -- Parse: {{#let x=value y=other}}...{{/let}}
      let bindings ← parsePartialHashParams  -- key=value pairs only
      skipWhitespace
      let trimAfter ← tryTrimEnd
      let _ ← Parser.tryString "}}"
      if trimAfter then Parser.setTrimNext true

      Parser.pushTag "let"
      let (body, _) ← parseNodes ["let"]
      let _ ← Parser.popTag
      return .«let» bindings body startPos

    | "repeat" =>
      -- Parse: {{#repeat 5}}...{{/repeat}}
      skipWhitespace
      let count ← parseAtom
      skipWhitespace
      let trimAfter ← tryTrimEnd
      let _ ← Parser.tryString "}}"
      if trimAfter then Parser.setTrimNext true

      Parser.pushTag "repeat"
      let (body, _) ← parseNodes ["repeat"]
      let _ ← Parser.popTag
      return .repeat count body startPos

    | "range" =>
      -- Parse: {{#range 1 10}}...{{/range}}
      skipWhitespace
      let startExpr ← parseAtom
      skipWhitespace
      let endExpr ← parseAtom
      skipWhitespace
      let trimAfter ← tryTrimEnd
      let _ ← Parser.tryString "}}"
      if trimAfter then Parser.setTrimNext true

      Parser.pushTag "range"
      let (body, _) ← parseNodes ["range"]
      let _ ← Parser.popTag
      return .range startExpr endExpr body startPos
    | other =>
      -- Unknown blocks treated as simple conditionals on variable truthiness
      let arg ← readUntil "}}"
      -- Check for trim marker at end of arg (before }})
      let trimAfter := arg.endsWith "~" || arg.endsWith "-"
      let argClean := if trimAfter then arg.dropRight 1 else arg
      let _ ← Parser.tryString "}}"
      if trimAfter then Parser.setTrimNext true
      let argTrimmed := argClean.trim

      if argTrimmed.isEmpty then
        let lb := "{{"
        let rb := "}}"
        Sift.Parser.fail s!"{lb}#{other}{rb} requires an argument"

      Parser.pushTag other
      let (body, foundTag) ← parseNodes ["else", other]

      let elseBody ← match foundTag with
        | some "else" =>
          let (elsePart, _) ← parseNodes [other]
          pure elsePart
        | _ => pure []

      let _ ← Parser.popTag
      -- Treat as simple variable truthiness check
      return .conditional [(.var argTrimmed 0, body)] elseBody false startPos

  /-- Parse a single tag (after detecting `{{`), returns (Node, trimBefore) -/
  partial def parseTag : Parser (Node × Bool) := do
    let pos ← Parser.getPosition

    -- Check for triple brace first: {{{var}}}
    if ← Parser.tryString "{{{" then
      let trimBefore ← tryTrimStart
      skipWhitespace
      let ref ← parseVarRef false pos
      skipWhitespace
      let trimAfter ← tryTrimEnd
      if !(← Parser.tryString "}}}") then
        Sift.Parser.fail "expected closing }}}"
      if trimAfter then Parser.setTrimNext true
      return (.variable ref, trimBefore)

    -- Consume the opening {{
    let _ ← Parser.tryString "{{"

    -- Check for trim marker after {{
    let trimBefore ← tryTrimStart
    skipWhitespace

    -- Peek at next char to determine tag type
    match ← peek with
    | some '!' =>
      -- Comment: {{! ... }}
      let _ ← anyChar
      let content ← readUntil "}}"
      -- Check for trim marker before }}
      let trimAfter := content.endsWith "~" || content.endsWith "-"
      let _ ← Parser.tryString "}}"
      if trimAfter then Parser.setTrimNext true
      let trimmedContent := if trimAfter then content.dropRight 1 |>.trimRight else content
      return (.comment trimmedContent.trim, trimBefore)

    | some '#' =>
      -- Section open - check for partial block {{#>}}
      let _ ← anyChar
      if ← Parser.tryChar '>' then
        -- Partial block: {{#> name context key=value}}...{{/name}}
        skipWhitespace
        let name ← readWhile1 isPartialNameChar "partial name"
        let (context, params) ← parsePartialArgs
        skipWhitespace
        let trimAfter ← tryTrimEnd
        let _ ← Parser.tryString "}}"
        if trimAfter then Parser.setTrimNext true
        Parser.pushTag name
        let (body, _) ← parseNodes [name]
        let _ ← Parser.popTag
        return (.partialBlock name context params body pos, trimBefore)
      else
        let node ← parseSection pos trimBefore
        return (node, trimBefore)

    | some '/' =>
      -- Close tag - this is an error at top level
      let _ ← anyChar
      let name ← parseCloseTag
      Sift.Parser.fail ("unexpected closing tag '{{/" ++ name ++ "}}'")


    | some '>' =>
      -- Partial
      let _ ← anyChar
      skipWhitespace
      let name ← readWhile1 isPartialNameChar "partial name"
      let (context, params) ← parsePartialArgs
      skipWhitespace
      let trimAfter ← tryTrimEnd
      let _ ← Parser.tryString "}}"
      if trimAfter then Parser.setTrimNext true
      return (.«partial» name context params pos, trimBefore)

    | some '&' =>
      -- Unescaped variable (alternative syntax)
      let _ ← anyChar
      skipWhitespace
      let ref ← parseVarRef false pos
      skipWhitespace
      let trimAfter ← tryTrimEnd
      if !(← Parser.tryString "}}") then
        Sift.Parser.fail "expected closing }}"
      if trimAfter then Parser.setTrimNext true
      return (.variable ref, trimBefore)

    | some _ =>
      -- Variable
      let ref ← parseVarRef true pos
      skipWhitespace
      let trimAfter ← tryTrimEnd
      if !(← Parser.tryString "}}") then
        Sift.Parser.fail "expected closing }}"
      if trimAfter then Parser.setTrimNext true
      return (.variable ref, trimBefore)

    | none =>
      Sift.Parser.fail "unexpected end of input in tag"

  /-- Parse nodes until we hit a stop tag or end of input -/
  partial def parseNodes (stopTags : List String) : Parser (List Node × Option String) := do
    let mut nodes : List Node := []
    let mut foundTag : Option String := none

    while true do
      if ← Parser.atEnd then break

      -- Check for closing or else tag
      let ahead ← Parser.peekString 3
      if ahead.startsWith "{{/" || ahead.startsWith "{{e" || ahead.startsWith "{{~" || ahead.startsWith "{{-" then
        -- Save position for potential backtrack
        let s ← Sift.Parser.get

        if ahead.startsWith "{{/" then
          let _ ← Parser.tryString "{{/"
          -- Check for trim marker after {{/
          let trimBefore ← tryTrimStart
          if trimBefore then
            nodes := trimLastNodeRight nodes
          let name ← parseCloseTag
          if stopTags.contains name then
            foundTag := some name
            break
          else
            -- Not our closing tag - error
            let expected := stopTags.head?
            match expected with
            | some exp => Sift.Parser.fail ("unmatched tag '{{/" ++ name ++ "}}', expected '{{/" ++ exp ++ "}}'")
            | none => Sift.Parser.fail ("unexpected closing tag '{{/" ++ name ++ "}}'")


        else if ← Parser.tryString "{{" then
          -- Check for trim marker that might precede / or else
          let trimBefore ← tryTrimStart
          skipWhitespace

          -- Check for close tag: {{~/ or {{-/
          if ← Parser.tryChar '/' then
            if trimBefore then
              nodes := trimLastNodeRight nodes
            -- Also check for another trim marker after /
            let _ ← tryTrimStart
            let name ← parseCloseTag
            if stopTags.contains name then
              foundTag := some name
              break
            else
              let expected := stopTags.head?
              match expected with
              | some exp => Sift.Parser.fail ("unmatched tag '{{/" ++ name ++ "}}', expected '{{/" ++ exp ++ "}}'")
              | none => Sift.Parser.fail ("unexpected closing tag '{{/" ++ name ++ "}}'")


          else if ← Parser.tryString "else" then
            if trimBefore then
              nodes := trimLastNodeRight nodes
            -- Check if it's {{else}} or {{else if ...}}
            skipWhitespace
            match ← peek with
            | some c =>
              if c == '~' || c == '-' then
                -- Trim marker before }}
                let _ ← anyChar
                skipWhitespace
              if c == '}' || c == '~' || c == '-' then
                -- Check for trim marker
                let trimAfter ← tryTrimEnd
                -- It's {{else}}, consume the closing }}
                let _ ← Parser.tryString "}}"
                if trimAfter then Parser.setTrimNext true
                if stopTags.contains "else" then
                  foundTag := some "else"
                  break
                else
                  -- else is not expected here, restore and treat as text
                  Sift.Parser.set s
              else
                -- It's {{else if ...}} or {{else something}}, report as "else"
                if stopTags.contains "else" then
                  foundTag := some "else"
                  break
                else
                  -- else is not expected here, restore and treat as text
                  Sift.Parser.set s
            | none =>
              Sift.Parser.set s
          else
            -- Not an else or close tag, restore
            Sift.Parser.set s

      -- Try to parse text first
      match ← parseText with
      | some (.text content) =>
        -- Check if we need to trim leading whitespace
        let shouldTrim ← Parser.getTrimNext
        let trimmedContent := if shouldTrim then content.trimLeft else content
        if shouldTrim then Parser.setTrimNext false
        if !trimmedContent.isEmpty then
          nodes := nodes ++ [.text trimmedContent]
      | some other =>
        nodes := nodes ++ [other]
      | none =>
        -- No text, try tag
        if ← checkOpenDelim then
          let (tagNode, trimBefore) ← parseTag
          if trimBefore then
            nodes := trimLastNodeRight nodes
          nodes := nodes ++ [tagNode]
        else if ← Parser.atEnd then
          break
        else
          -- This shouldn't happen, but consume a char to avoid infinite loop
          let c ← anyChar
          nodes := nodes ++ [.text (String.ofList [c])]

    return (nodes, foundTag)
end

/-- Parse a complete template -/
def parseTemplate : Parser Template := do
  let (nodes, foundTag) ← parseNodes []
  -- Check for unexpected closing tag
  match foundTag with
  | some tag =>
    Sift.Parser.fail ("unexpected closing tag '{{/" ++ tag ++ "}}'")

  | none => pure ()
  return { nodes }

/-- Public API: Parse a template string -/
def parse (input : String) : Except Sift.ParseError Template :=
  Parser.run parseTemplate input

end Stencil.Parser
