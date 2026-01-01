/-
  Stencil.Parser.Parse
  Template parsing
-/
import Stencil.Parser.Primitives
import Stencil.AST.Types

namespace Stencil.Parser

/-- Parse filter arguments (quoted strings after filter name) -/
def parseFilterArgs : Parser (List String) := do
  let mut args := []
  skipWhitespace
  while true do
    match ← Parser.peek? with
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

/-- Parse a variable path with optional filters -/
def parseVarRef (escaped : Bool) (pos : Position) : Parser VarRef := do
  skipWhitespace
  let path ← readWhile1 isPathChar "variable path"
  let filters ← parseFilters
  skipWhitespace
  return { path, filters, escaped, pos }

/-- Check for opening delimiter and detect type -/
def checkOpenDelim : Parser Bool := do
  let ahead ← Parser.peekString 2
  return ahead == "{{"

/-- Parse a comment: `{{! ... }}` -/
def parseComment : Parser Node := do
  let content ← readUntil "}}"
  let _ ← Parser.tryString "}}"
  return .comment content.trim

/-- Parse a closing tag: `{{/name}}` -/
def parseCloseTag : Parser String := do
  skipWhitespace
  let name ← readWhile1 isIdentChar "tag name"
  skipWhitespace
  let _ ← Parser.tryString "}}"
  return name

/-- Parse text content until a tag or end -/
partial def parseText : Parser (Option Node) := do
  let mut content := ""
  while true do
    if ← Parser.atEnd then break
    if ← checkOpenDelim then break
    let c ← Parser.next
    content := content.push c
  if content.isEmpty then
    return none
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
      let pos ← Parser.getPosition
      throw (ParseError.other pos s!"invalid number: {numStr}")

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
      let _ ← Parser.next
      return some .lt
    | some '>' =>
      let _ ← Parser.next
      return some .gt
    | _ => return none

-- Expression parsing is mutually recursive
mutual
  /-- Parse an atom (variable, literal, or parenthesized expression) -/
  partial def parseAtom : Parser Expr := do
    skipWhitespace
    match ← Parser.peek? with
    | some '"' | some '\'' =>
      parseStringLit
    | some c =>
      if c.isDigit || c == '-' then
        parseNumber
      else if c == '(' then
        let _ ← Parser.next  -- consume '('
        let expr ← parseOr
        skipWhitespace
        match ← Parser.peek? with
        | some ')' => let _ ← Parser.next
        | _ =>
          let pos ← Parser.getPosition
          throw (ParseError.other pos "expected ')'")
        return expr
      else if c == '!' then
        let _ ← Parser.next
        let inner ← parseAtom
        return .not inner
      else
        -- Variable or keyword
        let name ← readWhile isPathChar
        if name.isEmpty then
          let pos ← Parser.getPosition
          throw (.unexpectedChar pos c "expression")
        match name with
        | "true" => return .boolLit true
        | "false" => return .boolLit false
        | _ => return .var name
    | none => throw (.unexpectedEnd "expression")

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

/-- Parse partial parameters: `key=value key2="string"` -/
def parsePartialParams : Parser (List (String × Expr)) := do
  let mut params : List (String × Expr) := []
  while true do
    skipWhitespace
    match ← Parser.peek? with
    | some '}' => break
    | some c =>
      if isIdentChar c then
        let key ← readWhile1 isIdentChar "parameter name"
        skipWhitespace
        if ← Parser.tryChar '=' then
          skipWhitespace
          let value ← parseAtom
          params := params ++ [(key, value)]
        else
          -- Just a variable name without =, treat as key=key
          params := params ++ [(key, .var key)]
      else
        break
    | none => break
  return params

/-- Parse a partial with parameters: `{{> name key=value}}` -/
def parsePartial (pos : Position) : Parser Node := do
  skipWhitespace
  let name ← readWhile1 isIdentChar "partial name"
  let params ← parsePartialParams
  skipWhitespace
  let _ ← Parser.tryString "}}"
  return .«partial» name params pos

-- Mutually recursive parsing functions
mutual
  /-- Parse a conditional (if/unless) with optional else-if chains -/
  partial def parseConditional (startPos : Position) (inverted : Bool) (tagName : String) : Parser Node := do
    -- Parse the initial condition
    let condition ← parseCondition
    let _ ← Parser.tryString "}}"

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
        let s ← get
        skipWhitespace
        let maybeIf ← readWhile Char.isAlpha
        if maybeIf == "if" then
          -- It's an else-if, parse the new condition
          skipWhitespace
          currentCond ← parseCondition
          let _ ← Parser.tryString "}}"
        else
          -- It's just else, restore state and parse else body
          set s
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
  partial def parseSection (startPos : Position) : Parser Node := do
    skipWhitespace
    let blockType ← readWhile1 Char.isAlpha "block type"
    skipWhitespace

    match blockType with
    | "if" => parseConditional startPos false "if"
    | "unless" => parseConditional startPos true "unless"
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
      let _ ← Parser.tryString "}}"
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
      let _ ← Parser.tryString "}}"
      Parser.pushTag "block"
      let (body, _) ← parseNodes ["block"]
      let _ ← Parser.popTag
      return .block name body startPos
    | "super" =>
      -- Call parent block: {{#super}}
      let _ ← Parser.tryString "}}"
      return .super startPos
    | "each" =>
      let arg ← readUntil "}}"
      let _ ← Parser.tryString "}}"
      let argTrimmed := arg.trim

      if argTrimmed.isEmpty then
        let lb := "{{"
        let rb := "}}"
        throw (.invalidTagSyntax startPos s!"{lb}#each{rb} requires an argument")

      Parser.pushTag "each"
      let (body, foundTag) ← parseNodes ["else", "each"]

      let elseBody ← match foundTag with
        | some "else" =>
          let (elsePart, _) ← parseNodes ["each"]
          pure elsePart
        | _ => pure []

      let _ ← Parser.popTag
      return .each argTrimmed body elseBody startPos
    | other =>
      -- Unknown blocks treated as simple conditionals on variable truthiness
      let arg ← readUntil "}}"
      let _ ← Parser.tryString "}}"
      let argTrimmed := arg.trim

      if argTrimmed.isEmpty then
        let lb := "{{"
        let rb := "}}"
        throw (.invalidTagSyntax startPos s!"{lb}#{other}{rb} requires an argument")

      Parser.pushTag other
      let (body, foundTag) ← parseNodes ["else", other]

      let elseBody ← match foundTag with
        | some "else" =>
          let (elsePart, _) ← parseNodes [other]
          pure elsePart
        | _ => pure []

      let _ ← Parser.popTag
      -- Treat as simple variable truthiness check
      return .conditional [(.var argTrimmed, body)] elseBody false startPos

  /-- Parse a single tag (after detecting `{{`) -/
  partial def parseTag : Parser Node := do
    let pos ← Parser.getPosition

    -- Check for triple brace first
    if ← Parser.tryString "{{{" then
      let ref ← parseVarRef false pos
      if !(← Parser.tryString "}}}") then
        throw (.invalidTagSyntax pos "expected closing }}}")
      return .variable ref

    -- Consume the opening {{
    let _ ← Parser.tryString "{{"

    -- Peek at next char to determine tag type
    match ← Parser.peek? with
    | some '!' =>
      -- Comment
      let _ ← Parser.next
      parseComment

    | some '#' =>
      -- Section open - check for partial block {{#>}}
      let _ ← Parser.next
      if ← Parser.tryChar '>' then
        -- Partial block: {{#> name}}...{{/name}}
        skipWhitespace
        let name ← readWhile1 isIdentChar "partial name"
        let params ← parsePartialParams
        skipWhitespace
        let _ ← Parser.tryString "}}"
        Parser.pushTag name
        let (body, _) ← parseNodes [name]
        let _ ← Parser.popTag
        return .partialBlock name params body pos
      else
        parseSection pos

    | some '/' =>
      -- Close tag - this is an error at top level
      let _ ← Parser.next
      let name ← parseCloseTag
      throw (.unmatchedTag pos name none)

    | some '>' =>
      -- Partial
      let _ ← Parser.next
      parsePartial pos

    | some '&' =>
      -- Unescaped variable (alternative syntax)
      let _ ← Parser.next
      skipWhitespace
      let ref ← parseVarRef false pos
      if !(← Parser.tryString "}}") then
        throw (.invalidTagSyntax pos "expected closing }}")
      return .variable ref

    | some _ =>
      -- Variable
      let ref ← parseVarRef true pos
      if !(← Parser.tryString "}}") then
        throw (.invalidTagSyntax pos "expected closing }}")
      return .variable ref

    | none =>
      throw (.unexpectedEnd "tag")

  /-- Parse nodes until we hit a stop tag or end of input -/
  partial def parseNodes (stopTags : List String) : Parser (List Node × Option String) := do
    let mut nodes : List Node := []
    let mut foundTag : Option String := none

    while true do
      if ← Parser.atEnd then break

      -- Check for closing or else tag
      let ahead ← Parser.peekString 3
      if ahead.startsWith "{{/" || ahead.startsWith "{{e" then
        -- Save position for potential backtrack
        let s ← get

        if ahead.startsWith "{{/" then
          let _ ← Parser.tryString "{{/"
          let name ← parseCloseTag
          if stopTags.contains name then
            foundTag := some name
            break
          else
            -- Not our closing tag - error
            let pos ← Parser.getPosition
            let expected := stopTags.head?
            throw (.unmatchedTag pos name expected)

        else if ← Parser.tryString "{{else" then
          -- Check if it's {{else}} or {{else if ...}}
          skipWhitespace
          match ← Parser.peek? with
          | some '}' =>
            -- It's {{else}}, consume the closing }}
            let _ ← Parser.tryString "}}"
            if stopTags.contains "else" then
              foundTag := some "else"
              break
            else
              -- else is not expected here, restore and treat as text
              set s
          | some _ =>
            -- It's {{else if ...}} or {{else something}}, report as "else"
            if stopTags.contains "else" then
              foundTag := some "else"
              break
            else
              -- else is not expected here, restore and treat as text
              set s
          | none =>
            set s

      -- Try to parse text first
      match ← parseText with
      | some textNode =>
        nodes := nodes ++ [textNode]
      | none =>
        -- No text, try tag
        if ← checkOpenDelim then
          let tagNode ← parseTag
          nodes := nodes ++ [tagNode]
        else if ← Parser.atEnd then
          break
        else
          -- This shouldn't happen, but consume a char to avoid infinite loop
          let c ← Parser.next
          nodes := nodes ++ [.text (String.ofList [c])]

    return (nodes, foundTag)
end

/-- Parse a complete template -/
def parseTemplate : Parser Template := do
  let (nodes, foundTag) ← parseNodes []
  -- Check for unexpected closing tag
  match foundTag with
  | some tag =>
    let pos ← Parser.getPosition
    throw (.unmatchedTag pos tag none)
  | none => pure ()
  return { nodes }

/-- Public API: Parse a template string -/
def parse (input : String) : ParseResult Template :=
  Parser.run parseTemplate input

end Stencil.Parser
