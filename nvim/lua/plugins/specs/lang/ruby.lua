-- lua/plugins/specs/lang/ruby.lua - Ruby development

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

  -- Rails support
  { "tpope/vim-rails", ft = "ruby" },

  -- Auto-close `end` blocks
  {
    "RRethy/nvim-treesitter-endwise",
    ft           = "ruby",
    dependencies = "nvim-treesitter/nvim-treesitter",
  },
}
