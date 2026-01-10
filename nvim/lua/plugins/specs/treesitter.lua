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
        "fortran", "julia", "zig",
      },
      auto_install = true,
      highlight = { enable = true },
      indent = { enable = true, disable = { "python", "yaml" } },
      incremental_selection = {
        enable = true,
        keymaps = {
          init_selection = "<C-space>",
          node_incremental = "<C-space>",
          scope_incremental = false,
          node_decremental = "<bs>",
        },
      },
    },
  },

  {
    "nvim-treesitter/nvim-treesitter-context",
    lazy = false,
    dependencies = "nvim-treesitter/nvim-treesitter",
    opts = { enable = true, max_lines = 3 },
  },
}