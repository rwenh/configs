-- lua/plugins/specs/lang/ruby.lua - Ruby development (SAFE KEYMAPS)
-- Note: LSP (solargraph) is configured in lsp.lua, no need to duplicate here

return {
  -- Ruby testing with RSpec/Minitest (using 'rb' prefix for Ruby)
  {
    "vim-test/vim-test",
    ft = "ruby",
    keys = {
      { "<leader>rbn", "<cmd>TestNearest<cr>", desc = "Ruby Test Nearest", ft = "ruby" },
      { "<leader>rbf", "<cmd>TestFile<cr>", desc = "Ruby Test File", ft = "ruby" },
      { "<leader>rbs", "<cmd>TestSuite<cr>", desc = "Ruby Test Suite", ft = "ruby" },
      { "<leader>rbl", "<cmd>TestLast<cr>", desc = "Ruby Test Last", ft = "ruby" },
      { "<leader>rbv", "<cmd>TestVisit<cr>", desc = "Ruby Test Visit", ft = "ruby" },
    },
  },

  -- Rails support
  {
    "tpope/vim-rails",
    ft = "ruby",
  },

  -- Endwise - auto add 'end' keyword
  {
    "RRethy/nvim-treesitter-endwise",
    ft = "ruby",
    dependencies = "nvim-treesitter/nvim-treesitter",
  },

  -- Ruby debug adapter
  {
    "suketa/nvim-dap-ruby",
    ft = "ruby",
    dependencies = "mfussenegger/nvim-dap",
    config = function()
      require("dap-ruby").setup()
    end,
  },
}
