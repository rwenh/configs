-- lua/plugins/specs/lang/ruby.lua - Ruby development
-- LSP (solargraph) is configured in lsp.lua.

return {
  -- Test runner (vim-test)
  {
    "vim-test/vim-test",
    ft   = "ruby",
    keys = {
      { "<leader>rbn", "<cmd>TestNearest<cr>", desc = "Ruby Test Nearest", ft = "ruby" },
      { "<leader>rbf", "<cmd>TestFile<cr>",    desc = "Ruby Test File",    ft = "ruby" },
      { "<leader>rbs", "<cmd>TestSuite<cr>",   desc = "Ruby Test Suite",   ft = "ruby" },
      { "<leader>rbl", "<cmd>TestLast<cr>",    desc = "Ruby Test Last",    ft = "ruby" },
      { "<leader>rbv", "<cmd>TestVisit<cr>",   desc = "Ruby Test Visit",   ft = "ruby" },
    },
    init = function()
      vim.g["test#strategy"] = "toggleterm"
    end,
  },

  -- NOTE (Fix #4): neotest-rspec spec removed — test.lua already registers
  -- it centrally with the same rspec_cmd config. Inserting it here too caused
  -- double adapter registration and duplicate test results.
  -- NOTE (Fix #5): vim.tbl_flatten was also used here (deprecated in 0.11,
  -- no-op on flat lists) — moot since the spec is removed.

  -- Rails support
  { "tpope/vim-rails", ft = "ruby" },

  -- Auto-close `end` blocks
  {
    "RRethy/nvim-treesitter-endwise",
    ft           = "ruby",
    dependencies = "nvim-treesitter/nvim-treesitter",
  },

  -- NOTE (Fix #6): Ruby DAP spec removed for two reasons:
  -- 1. optional=true config functions don't run — the adapter was silently
  --    never registered, making Ruby debugging non-functional from this spec.
  -- 2. dap.lua already registers dap.adapters.ruby and dap.configurations.ruby
  --    directly. This was a redundant duplicate of that configuration.

  -- NOTE (Fix #7): Conform rubocop spec removed — lsp.lua already registers
  -- ruby = { "rubocop" } in formatters_by_ft. Redundant duplicate.
}
