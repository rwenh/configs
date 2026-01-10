-- lua/plugins/specs/lang/elixir.lua - Elixir development

return {
  -- Elixir LSP and tools
  {
    "elixir-tools/elixir-tools.nvim",
    ft = { "elixir", "eex", "heex", "surface" },
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      local elixir = require("elixir")
      local elixirls = require("elixir.elixirls")

      elixir.setup({
        nextls = { enable = false },
        elixirls = {
          enable = true,
          settings = elixirls.settings({
            dialyzerEnabled = true,
            enableTestLenses = true,
          }),
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

  -- Elixir debug adapter
  {
    "mfussenegger/nvim-dap",
    optional = true,
    opts = function()
      local dap = require("dap")
      dap.adapters.mix_task = {
        type = "executable",
        command = vim.fn.exepath("elixir-ls-debugger"),
        args = {},
      }
      dap.configurations.elixir = {
        {
          type = "mix_task",
          name = "mix test",
          task = "test",
          taskArgs = { "--trace" },
          request = "launch",
          startApps = true,
          projectDir = "${workspaceFolder}",
          requireFiles = {
            "test/**/test_helper.exs",
            "test/**/*_test.exs",
          },
        },
      }
    end,
  },

  -- Testing
  {
    "jfpedroza/neotest-elixir",
    ft = "elixir",
    dependencies = "nvim-neotest/neotest",
  },
}