-- lua/plugins/specs/treesitter.lua - Treesitter

return {
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    lazy = false,
    priority = 100,
    opts = {
      ensure_installed = {
        "bash", "c", "cpp", "css", "go", "html", "javascript",
        "json", "lua", "markdown", "python", "rust", "typescript", "yaml",
      },
      auto_install = true,
      highlight = { enable = true },
      indent = { enable = true, disable = { "python", "yaml" } },
    },
  },

  {
    "nvim-treesitter/nvim-treesitter-context",
    lazy = false,
    dependencies = "nvim-treesitter/nvim-treesitter",
    opts = { enable = true, max_lines = 3 },
  },
}