-- lua/plugins/specs/lang/elixir.lua - Elixir development
--
-- FIX (v2.2.3):
--   • mhanberg/output-panel.nvim removed — plugin is archived/unmaintained;
--     setup() produced warnings on newer Neovim builds. Replaced with a
--     lightweight Phoenix log tail via toggleterm (more useful in practice).
--   • <leader>ex* keymaps added — the group was declared in which-key's spec
--     but no keymaps existed for it. At minimum: mix test, mix format,
--     Phoenix server toggle, and IEx session.

return {
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

      pcall(function()
        elixir.setup({
          nextls   = { enable = false },
          elixirls = { enable = false },  -- lsp.lua owns elixirls
        })
      end)
    end,
  },

  {
    "mattreduce/vim-mix",
    ft = "elixir",
  },

  -- Elixir-specific keymaps via toggleterm
  {
    "akinsho/toggleterm.nvim",
    optional = true,
    keys = {
      {
        "<leader>ext",
        function()
          local ok, term = pcall(require, "toggleterm.terminal")
          if not ok then return end
          term.Terminal:new({
            cmd = "mix test",
            direction = "float", close_on_exit = false,
          }):toggle()
        end,
        desc = "Elixir mix test",
        ft   = "elixir",
      },
      {
        "<leader>exf",
        function()
          local ok, term = pcall(require, "toggleterm.terminal")
          if not ok then return end
          term.Terminal:new({
            cmd = "mix format",
            direction = "float", close_on_exit = false,
          }):toggle()
        end,
        desc = "Elixir mix format",
        ft   = "elixir",
      },
      {
        "<leader>exp",
        function()
          local ok, term = pcall(require, "toggleterm.terminal")
          if not ok then return end
          term.Terminal:new({
            cmd = "mix phx.server",
            direction = "float", close_on_exit = false,
          }):toggle()
        end,
        desc = "Elixir Phoenix server",
        ft   = "elixir",
      },
      {
        "<leader>exi",
        function()
          local ok, term = pcall(require, "toggleterm.terminal")
          if not ok then return end
          term.Terminal:new({
            cmd = "iex -S mix",
            direction = "float", close_on_exit = false,
          }):toggle()
        end,
        desc = "Elixir IEx session",
        ft   = "elixir",
      },
    },
  },
}
