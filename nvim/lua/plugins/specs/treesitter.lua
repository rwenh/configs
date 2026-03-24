-- lua/plugins/specs/treesitter.lua - Treesitter

return {
  {
    "nvim-treesitter/nvim-treesitter",
    build     = ":TSUpdate",
    lazy      = false,
    priority  = 100,
    -- FIX #12: nvim-treesitter uses require("nvim-treesitter.configs").setup(),
    -- not the plugin root. Lazy's `opts` shorthand passes to setup() on the
    -- root module, which is wrong here. Using an explicit config function
    -- ensures the correct setup target is called, especially post-rewrite.
    config = function(_, opts)
      require("nvim-treesitter").setup(opts)
    end,
    opts = {
      ensure_installed = {
        "bash", "c", "cpp", "css", "go", "html", "javascript",
        "json", "lua", "markdown", "python", "rust", "typescript", "yaml",
        "fortran", "julia", "zig",
        -- New languages
        "ruby",
        "elixir", "heex", "eex",
        "kotlin",
        "vhdl",
        -- COBOL: no stable treesitter parser yet
      },
      auto_install = true,
      highlight    = { enable = true },
      indent       = { enable = true, disable = { "python", "yaml" } },
      incremental_selection = {
        enable = true,
        keymaps = {
          init_selection    = "<C-space>",
          node_incremental  = "<C-space>",
          scope_incremental = false,
          node_decremental  = "<bs>",
        },
      },
    },
  },

  {
    "nvim-treesitter/nvim-treesitter-context",
    -- FIX #11: Changed lazy=false → BufReadPost. treesitter-context only
    -- activates in buffers with treesitter attached — loading it at startup
    -- adds unnecessary startup cost. BufReadPost matches when treesitter
    -- itself first activates on a buffer.
    event        = "BufReadPost",
    dependencies = "nvim-treesitter/nvim-treesitter",
    opts         = { enable = true, max_lines = 3 },
  },
}
