# tree-sitter-bosatsu

In-repo Tree-sitter grammar for Bosatsu.

## Semantic Coverage (Current)

The current grammar is intentionally token/category oriented. It can reliably
identify Bosatsu semantic markers as `keyword`, `identifier`,
`type_identifier`, `operator`, and `punctuation` tokens inside
`simple_statement` lines.

Coverage checklist:

- Top-level bindings (`name = expr`): detectable as a statement with an
  `identifier` and `=` punctuation.
- Function definitions (`def`): `keyword` token `def` plus following name token
  (`identifier`, `backticked_identifier`, or operator declaration form).
- Type definitions (`enum`, `struct`): `keyword` tokens `enum` and `struct`.
- Imports/exports (`from`, `import`, `export`): `keyword` tokens are present.
- Pattern/control constructs (`match`, `case`, `recur`, `loop`): all are
  recognized as `keyword` tokens.
- Conditionals (`if`, `elif`, `else`): all are recognized as `keyword` tokens.
- Operators:
  - symbolic operators are tokenized as `operator`
  - operator declarations use `keyword` token `operator`

These forms are exercised across real `.bosatsu` fixtures in `test_workspace`
(for example: `List.bosatsu`, `Quicksort.bosatsu`, `Char.bosatsu`,
`IntTest.bosatsu`, and `BinInt.bosatsu`).

## Current Limitations

- This is not yet a fully structured Bosatsu AST grammar:
  there are no dedicated nodes like `def_statement`, `enum_declaration`,
  `match_expression`, or `if_expression`.
- Block relationships are shallow:
  indentation is tracked via external scanner tokens, but statements are still
  represented primarily as line-level token streams.
- Semantic relationships are not encoded:
  the grammar does not currently attach fields like function-name/args/body or
  match-scrutinee/case-branches.
- `queries/locals.scm` is intentionally minimal today and does not yet model
  Bosatsu definition/reference scoping rules in detail.
- Because the grammar is permissive, some structurally invalid Bosatsu may still
  parse as tokenized statements and require downstream validation.

## Local development

1. Build/update parser artifacts:

   ```bash
   cd syntax/tree-sitter-bosatsu
   tree-sitter generate
   tree-sitter test
   ```

2. Neovim setup (`nvim-treesitter`):

   ```lua
   dofile(vim.fn.getcwd() .. "/syntax/tree-sitter-bosatsu/nvim/bosatsu.lua")
   ```

3. Install/update in Neovim:

   ```vim
   :TSInstall bosatsu
   :TSUpdate bosatsu
   ```
