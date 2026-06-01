-- lua/plugins/specs/lang/elixir.lua — Elixir development
--
-- Test mechanism guide:
--   <leader>ext   → mix test (this file, project root)
--   <leader>'n    → neotest-elixir (nearest ExUnit test)
--   <leader>'t    → runner.lua (mix test from project root)
--   Use neotest for interactive TDD; <leader>ext for quick full runs.
--

local shared = require("plugins.specs.lang.shared")

return {
  -- ── elixir-tools ─────────────────────────────────────────────────────────
  {
    "elixir-tools/elixir-tools.nvim",
    ft           = { "elixir", "eex", "heex", "surface" },
    dependencies = { "nvim-lua/plenary.nvim" },

    cond = function()
      return vim.g.elixir_use_nextls == true
    end,

    config = function()
      local ok, elixir = pcall(require, "elixir")
      if not ok then
        vim.notify("elixir-tools setup failed", vim.log.levels.WARN)
        return
      end
      local ok_elixir = pcall(function()
        elixir.setup({
          elixirls = { enable = false },
          nextls   = { enable = true },
        })
      end)
      if not ok_elixir then
        vim.notify("[elixir-tools] setup failed — check elixir-tools.nvim install",
          vim.log.levels.WARN)
      end
    end,
  },

  -- ── Keymaps ────────────────────────────────────────────────────────────────

  {
    "akinsho/toggleterm.nvim",
    keys = {
      {
        "<leader>ext",
        function() require("core.util.term").float_at_root("mix test") end,
        desc = "Elixir mix test",
        ft   = "elixir",
      },
      {
        "<leader>exf",
        function() require("core.util.term").float_at_root("mix format") end,
        desc = "Elixir mix format",
        ft   = "elixir",
      },
      {
        "<leader>exp",
        function()
          require("core.util.term").float_at_root(
            "mix phx.server",
            { close_on_exit = false }
          )
        end,
        desc = "Elixir Phoenix server",
        ft   = "elixir",
      },
      {
        "<leader>exi",
        function() require("core.util.term").float_at_root("iex -S mix") end,
        desc = "Elixir IEx session",
        ft   = "elixir",
      },
    },
  },

  -- ── Treesitter ─────────────────────────────────────────────────────────────

  shared.treesitter({ "elixir", "heex", "eex" }),

}
