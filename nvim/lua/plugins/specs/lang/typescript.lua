-- lua/plugins/specs/lang/typescript.lua
--
-- OPT (v2.3.13):
--   • nvim-lint spec: BufReadPost + once=true autocmd wrapper removed.
--     Direct init() assignment is sufficient (see css.lua note).

return {
  -- ── TypeScript LSP extras ─────────────────────────────────────────────
  {
    "pmizio/typescript-tools.nvim",
    ft           = { "typescript", "typescriptreact", "javascript", "javascriptreact" },
    dependencies = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
    opts = {
      settings = {
        tsserver_file_preferences = {
          importModuleSpecifierPreference         = "non-relative",
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
      { "<leader>tso", "<cmd>TSToolsOrganizeImports<cr>",      desc = "TS Organize Imports",   ft = { "typescript", "typescriptreact" } },
      { "<leader>tsi", "<cmd>TSToolsAddMissingImports<cr>",    desc = "TS Add Missing Imports", ft = { "typescript", "typescriptreact" } },
      { "<leader>tsr", "<cmd>TSToolsRemoveUnusedImports<cr>",  desc = "TS Remove Unused",       ft = { "typescript", "typescriptreact" } },
      { "<leader>tsf", "<cmd>TSToolsFixAll<cr>",               desc = "TS Fix All",             ft = { "typescript", "typescriptreact" } },
      { "<leader>tsd", "<cmd>TSToolsGoToSourceDefinition<cr>", desc = "TS Source Definition",   ft = { "typescript", "typescriptreact" } },
    },
  },

  -- ── nvim-lint ─────────────────────────────────────────────────────────
  {
    "mfussenegger/nvim-lint",
    optional = true,
    init = function()
      local ok, lint = pcall(require, "lint")
      if not ok then return end
      lint.linters_by_ft.typescript      = { "eslint_d" }
      lint.linters_by_ft.typescriptreact = { "eslint_d" }
    end,
  },

  -- ── Conform ───────────────────────────────────────────────────────────
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters_by_ft = opts.formatters_by_ft or {}
      opts.formatters_by_ft.typescriptreact = { "prettier" }
    end,
  },

  -- ── Treesitter ────────────────────────────────────────────────────────
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "typescript", "tsx" })
      end
    end,
  },
}
