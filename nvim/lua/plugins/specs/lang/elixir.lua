-- lua/plugins/specs/lang/elixir.lua — Elixir development
--
-- LSP:    elixirls via lsp.lua (canonical)
-- Format: mix format via lsp.lua conform
-- Lint:   none configured (credo is a popular linter — see note below)
-- DAP:    elixir-ls debugger via dap.lua
-- Test:   neotest-elixir via test.lua; <leader>ext here; runner.lua
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
      pcall(function()
        elixir.setup({
          -- elixir-ls is managed by lsp.lua; disable it here to avoid
          -- double-attachment when elixir-tools is active.
          elixirls = { enable = false },
          nextls   = { enable = true },
        })
      end)
    end,
  },

  -- ── Keymaps ────────────────────────────────────────────────────────────────
  -- These keymaps use toggleterm directly and do not depend on elixir-tools,
  -- so they are available in the default setup regardless of the nextls flag.

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
