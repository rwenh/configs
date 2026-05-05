-- lua/plugins/specs/lang/javascript.lua — JavaScript development
--
-- LSP:    typescript-tools (typescript.lua) covers JS too
-- Format: prettier via lsp.lua conform (javascript + javascriptreact)
-- Lint:   eslint_d via lsp.lua nvim-lint (guarded)
-- DAP:    pwa-node via dap.lua
-- Test:   neotest-jest/vitest via test.lua; runner.lua
--
-- NOTE: package-info.nvim makes npm registry network requests on show().
--       It requires internet access and npm registry availability.
--

local shared = require("plugins.specs.lang.shared")

return {
  -- ── Package.json dependency management ─────────────────────────────────────

  {
    "vuki656/package-info.nvim",
    dependencies = "MunifTanjim/nui.nvim",
    event = "BufRead package.json",
    cond  = function()
      return not vim.fn.expand("%:p"):find("node_modules", 1, true)
    end,
    opts  = { colors = { up_to_date = "#3C4048", outdated = "#d19a66" } },
    config = function(_, opts)
      pcall(function() require("package-info").setup(opts) end)
    end,
    keys = {
      {
        "<leader>jps",
        function() pcall(function() require("package-info").show() end) end,
        desc = "Show package versions (requires network)",
      },
      {
        "<leader>jpu",
        function() pcall(function() require("package-info").update() end) end,
        desc = "Update package",
      },
      {
        "<leader>jpd",
        function() pcall(function() require("package-info").delete() end) end,
        desc = "Delete package",
      },
      {
        "<leader>jpi",
        function() pcall(function() require("package-info").install() end) end,
        desc = "Install package",
      },
      {
        "<leader>jpc",
        function() pcall(function() require("package-info").change_version() end) end,
        desc = "Change version",
      },
    },
  },

  -- ── Conform: javascriptreact ───────────────────────────────────────────────

  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters_by_ft = opts.formatters_by_ft or {}
      opts.formatters_by_ft.javascriptreact = { "prettier" }
    end,
  },

  -- ── nvim-lint: eslint_d ────────────────────────────────────────────────────

  {
    "mfussenegger/nvim-lint",
    optional = true,
    init = function()
      if vim.fn.executable("eslint_d") ~= 1 then return end
    end,
  },

  -- ── Treesitter ─────────────────────────────────────────────────────────────

  shared.treesitter({ "javascript", "jsdoc" }),

}
