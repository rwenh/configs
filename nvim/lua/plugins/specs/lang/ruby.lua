-- lua/plugins/specs/lang/rust.lua - Ruby development

return {
  -- Ruby LSP and formatting
  {
    "neovim/nvim-lspconfig",
    ft = "ruby",
    config = function()
      local lspconfig = require("lspconfig")
      local capabilities = require("cmp_nvim_lsp").default_capabilities()
      
      lspconfig.solargraph.setup({
        capabilities = capabilities,
        settings = {
          solargraph = {
            diagnostics = true,
            completion = true,
            hover = true,
            formatting = true,
          },
        },
      })
    end,
  },

  -- Ruby testing with RSpec/Minitest
  {
    "vim-test/vim-test",
    ft = "ruby",
    keys = {
      { "<leader>tn", "<cmd>TestNearest<cr>", desc = "Test Nearest", ft = "ruby" },
      { "<leader>tf", "<cmd>TestFile<cr>", desc = "Test File", ft = "ruby" },
      { "<leader>ts", "<cmd>TestSuite<cr>", desc = "Test Suite", ft = "ruby" },
      { "<leader>tl", "<cmd>TestLast<cr>", desc = "Test Last", ft = "ruby" },
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