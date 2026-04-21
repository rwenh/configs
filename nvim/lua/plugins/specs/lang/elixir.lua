-- lua/plugins/specs/lang/elixir.lua - Elixir development
--
-- FIX (v2.2.3): output-panel.nvim removed; <leader>ex* keymaps added.
--
-- OPT (v2.3.13):
--   • Toggleterm keymaps table-driven; 4 × boilerplate replaced by a loop.
--   • core.util.term.float() used for all launches.

return {
  -- ── elixir-tools ─────────────────────────────────────────────────────
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
          nextls   = { enable = false },
          elixirls = { enable = false },  -- lsp.lua owns elixirls
        })
      end)
    end,
  },

  { "mattreduce/vim-mix", ft = "elixir" },

  -- ── Keymaps ───────────────────────────────────────────────────────────
  {
    "akinsho/toggleterm.nvim",
    optional = true,
    keys = (function()
      local cmds = {
        { "<leader>ext", "mix test",      "Elixir mix test"      },
        { "<leader>exf", "mix format",    "Elixir mix format"    },
        { "<leader>exp", "mix phx.server","Elixir Phoenix server" },
        { "<leader>exi", "iex -S mix",    "Elixir IEx session"   },
      }
      local keys = {}
      for _, c in ipairs(cmds) do
        local lhs, cmd, desc = c[1], c[2], c[3]
        table.insert(keys, {
          lhs,
          function() require("core.util.term").float(cmd) end,
          desc = desc,
          ft   = "elixir",
        })
      end
      return keys
    end)(),
  },
}
