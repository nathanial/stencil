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

/-- Parse a partial: `{{> name }}` -/
def parsePartial (pos : Position) : Parser Node := do
  skipWhitespace
  let name ← readWhile1 isIdentChar "partial name"
  skipWhitespace
  let _ ← Parser.tryString "}}"
  return .«partial» name pos

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

-- Mutually recursive parsing functions
mutual
  /-- Parse a section opening: `{{#if condition}}` or `{{#each items}}` etc -/
  partial def parseSection (startPos : Position) : Parser Node := do
    skipWhitespace
    let blockType ← readWhile1 Char.isAlpha "block type"
    skipWhitespace
    let arg ← readUntil "}}"
    let _ ← Parser.tryString "}}"
    let argTrimmed := arg.trim

    -- Validate we have an argument
    if argTrimmed.isEmpty then
      let lb := "{{"
      let rb := "}}"
      throw (.invalidTagSyntax startPos s!"{lb}#{blockType}{rb} requires an argument")

    -- Push tag for matching
    Parser.pushTag blockType

    -- Parse body until {{else}} or {{/blockType}}
    let (body, foundTag) ← parseNodes ["else", blockType]

    let elseBody ← match foundTag with
      | some "else" =>
        -- Continue parsing for the else body
        let (elsePart, _) ← parseNodes [blockType]
        pure elsePart
      | _ => pure []

    -- Pop the tag
    let _ ← Parser.popTag

    -- Dispatch based on block type
    match blockType with
    | "if" => return .section argTrimmed false body elseBody startPos
    | "unless" => return .section argTrimmed true body elseBody startPos
    | "each" => return .each argTrimmed body elseBody startPos
    | other =>
      -- Treat unknown blocks as sections
      return .section other false body elseBody startPos

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
      -- Section open
      let _ ← Parser.next
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

        else if ← Parser.tryString "{{else}}" then
          if stopTags.contains "else" then
            foundTag := some "else"
            break
          else
            -- else is not expected here, restore and treat as text
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
