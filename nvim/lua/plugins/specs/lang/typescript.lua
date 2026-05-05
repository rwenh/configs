-- lua/plugins/specs/lang/typescript.lua — TypeScript development
--
-- LSP:    typescript-tools (this file — covers JS too; see javascript.lua)
-- Format: prettier via lsp.lua conform (typescript + typescriptreact)
-- Lint:   eslint_d via lsp.lua nvim-lint (guarded)
-- DAP:    pwa-node via dap.lua
-- Test:   neotest-jest/vitest via test.lua; runner.lua
--
--

local shared = require("plugins.specs.lang.shared")

return {
  -- ── typescript-tools ───────────────────────────────────────────────────────
  -- Covers both TypeScript AND JavaScript (ft includes all four variants).
  -- typescript-tools manages its own tsserver instance; do NOT add tsserver
  -- to lsp.lua's servers table — two tsserver processes would conflict.
  {
    "pmizio/typescript-tools.nvim",
    ft           = shared.JS_TS_FT,
    dependencies = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
    opts = {
      settings = {
        tsserver_file_preferences = {
          importModuleSpecifierPreference         = vim.g.ts_import_preference
                                                    or "non-relative",
          includeInlayParameterNameHints          = "literals",
          includeInlayVariableTypeHints           = false,
          includeInlayFunctionLikeReturnTypeHints = true,
        },
      },
    },
    config = function(_, opts)
      pcall(function() require("typescript-tools").setup(opts) end)
    end,
    keys = {
      { "<leader>tso", "<cmd>TSToolsOrganizeImports<cr>",
        desc = "TS Organize Imports",    ft = shared.TS_FT },
      { "<leader>tsi", "<cmd>TSToolsAddMissingImports<cr>",
        desc = "TS Add Missing Imports", ft = shared.TS_FT },
      { "<leader>tsr", "<cmd>TSToolsRemoveUnusedImports<cr>",
        desc = "TS Remove Unused",       ft = shared.TS_FT },
      { "<leader>tsf", "<cmd>TSToolsFixAll<cr>",
        desc = "TS Fix All",             ft = shared.TS_FT },
      { "<leader>tsd", "<cmd>TSToolsGoToSourceDefinition<cr>",
        desc = "TS Go To Source Def (not .d.ts)", ft = shared.TS_FT },
    },
  },

  -- ── Conform: typescriptreact ───────────────────────────────────────────────

  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters_by_ft = opts.formatters_by_ft or {}
      opts.formatters_by_ft.typescriptreact = { "prettier" }
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

  -- ── Neogen: JSDoc/TSDoc ────────────────────────────────────────────────────

  {
    "danymat/neogen",
    optional = true,
    ft       = { "typescript", "typescriptreact" },
    opts = {
      languages = {
        typescript = { template = { annotation_convention = "tsdoc" } },
      },
    },
  },

  shared.treesitter({ "typescript", "tsx" }),
}
