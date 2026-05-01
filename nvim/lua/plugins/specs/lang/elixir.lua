-- lua/plugins/specs/lang/elixir.lua — Elixir development
--
-- LSP:    elixirls via lsp.lua (canonical; elixir-tools disabled below)
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
-- NOTE: credo linting is not configured.
--       To add it: install credo (mix escript.install hex credo), then add
--       an nvim-lint optional spec here:
--         lint.linters_by_ft.elixir = { "credo" }
--

return {
  -- ── elixir-tools ─────────────────────────────────────────────────────────
  {
    "elixir-tools/elixir-tools.nvim",
    ft           = { "elixir", "eex", "heex", "surface" },
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      local ok, elixir = pcall(require, "elixir")
      if not ok then
        vim.notify("elixir-tools setup failed", vim.log.levels.WARN)
        return
      end
      pcall(function()
        elixir.setup({
          nextls   = { enable = vim.g.elixir_use_nextls == true },
          elixirls = { enable = false },
        })
      end)
    end,
  },

  -- ── Keymaps ────────────────────────────────────────────────────────────────

  {
    "akinsho/toggleterm.nvim",
    optional = true,
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

  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "elixir", "heex", "eex" })
      end
    end,
  },

  -- NOTE: elixirls LSP is in lsp.lua servers table.
  -- mix format conform is in lsp.lua formatters_by_ft.
  -- elixir-ls DAP debugger is in dap.lua.
  -- neotest-elixir adapter is in test.lua.
}
