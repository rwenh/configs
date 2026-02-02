-- lua/plugins/specs/lang/elixir.lua - Elixir development (SAFE KEYMAPS)

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

  -- Testing (using 'ex' prefix for Elixir)
  {
    "jfpedroza/neotest-elixir",
    ft = "elixir",
    dependencies = "nvim-neotest/neotest",
    keys = {
      { "<leader>exn", function() require("neotest").run.run() end, desc = "Elixir Test Nearest", ft = "elixir" },
      { "<leader>exf", function() require("neotest").run.run(vim.fn.expand("%")) end, desc = "Elixir Test File", ft = "elixir" },
      { "<leader>exa", function() require("neotest").run.run(vim.uv.cwd()) end, desc = "Elixir Test All", ft = "elixir" },
      { "<leader>exs", function() require("neotest").summary.toggle() end, desc = "Elixir Test Summary", ft = "elixir" },
    },
  },
}
