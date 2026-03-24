-- lua/plugins/specs/lang/elixir.lua - Elixir development (SAFE KEYMAPS)

return {
  -- Elixir LSP and tools
  {
    "elixir-tools/elixir-tools.nvim",
    ft = { "elixir", "eex", "heex", "surface" },
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      local elixir    = require("elixir")
      local elixirls  = require("elixir.elixirls")

      elixir.setup({
        nextls = { enable = false },
        elixirls = {
          -- FIX #5: Disabled elixirls here — lsp.lua already configures and
          -- enables elixirls via vim.lsp.config/enable (Neovim 0.11+ API).
          -- Having both active attaches two ElixirLS clients per buffer,
          -- producing duplicate diagnostics, completions, and hover results.
          -- elixir-tools is kept for mix task integration and future NextLS.
          enable = false,
        },
      })
    end,
  },

  -- Mix integration
  {
    "mattreduce/vim-mix",
    ft = "elixir",
  },

  -- Phoenix framework support
  {
    "mhanberg/output-panel.nvim",
    ft = { "elixir", "heex" },
    config = function()
      require("output_panel").setup()
    end,
  },

  -- NOTE (Fix #6 & #7): neotest-elixir standalone spec removed.
  -- test.lua already registers neotest-elixir as a neotest adapter dependency
  -- and the global <leader>'* bindings (test nearest/file/all/summary) cover
  -- Elixir buffers. Keeping a duplicate spec here caused double adapter
  -- registration and redundant which-key entries for the same actions.

