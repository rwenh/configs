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

  -- Neotest adapter for RSpec
  {
    "nvim-neotest/neotest",
    optional = true,
    dependencies = { "olimorris/neotest-rspec" },
    opts = function(_, opts)
      opts.adapters = opts.adapters or {}
      table.insert(opts.adapters, require("neotest-rspec")({
        rspec_cmd = function()
          return vim.tbl_flatten({
            "bundle", "exec", "rspec",
          })
        end,
      }))
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

  -- DAP via rdbg (replaces unmaintained nvim-dap-ruby)
  {
    "mfussenegger/nvim-dap",
    optional = true,
    ft       = "ruby",
    config   = function()
      local dap = require("dap")

      dap.adapters.ruby = function(callback, config)
        callback({
          type = "server",
          host = "127.0.0.1",
          port = "${port}",
          executable = {
            command = "bundle",
            args    = {
              "exec", "rdbg",
              "--open", "--port", "${port}",
              "-c", "--",
              config.command or "ruby",
              config.program,
            },
          },
        })
      end

      dap.configurations.ruby = {
        {
          type    = "ruby",
          name    = "Debug current file",
          request = "attach",
          localfs = true,
          program = "${file}",
          command = "ruby",
        },
        {
          type    = "ruby",
          name    = "Debug RSpec (nearest)",
          request = "attach",
          localfs = true,
          program = "${file}",
          command = "bundle exec rspec",
        },
      }
    end,
  },

  -- Conform: rubocop
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = {
      formatters_by_ft = {
        ruby = { "rubocop" },
      },
    },
  },
}
