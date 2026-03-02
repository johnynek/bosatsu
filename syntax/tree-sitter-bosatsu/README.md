# tree-sitter-bosatsu

In-repo Tree-sitter grammar for Bosatsu.

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
