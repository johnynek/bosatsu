-- Add this from your Neovim config to use the in-repo Bosatsu parser.
local parser_config = require("nvim-treesitter.parsers").get_parser_configs()

parser_config.bosatsu = {
  install_info = {
    url = vim.fn.getcwd(),
    files = { "src/parser.c", "src/scanner.c" },
    location = "syntax/tree-sitter-bosatsu",
    generate_requires_npm = false,
    requires_generate_from_grammar = false,
  },
  filetype = "bosatsu",
}

vim.filetype.add({
  extension = {
    bosatsu = "bosatsu",
  },
})
