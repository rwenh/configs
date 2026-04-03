-- lua/plugins/specs/lang/elixir.lua - Elixir development

return {
  -- Elixir LSP and tools
  {
    "elixir-tools/elixir-tools.nvim",
    ft = { "elixir", "eex", "heex", "surface" },
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      local ok, elixir = pcall(require, "elixir")
      if not ok then
        vim.notify("elixir-tools setup failed", vim.log.levels.WARN)
        return
      end

      -- RECALIBRATION: Safe setup with elixirls disabled
      pcall(function()
        elixir.setup({
          nextls = { enable = false },
          elixirls = {
            enable = false,  -- lsp.lua handles this
          },
        })
      end)
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
      pcall(function() require("output_panel").setup() end)
    end,
  },
}
