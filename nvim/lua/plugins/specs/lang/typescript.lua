-- lua/plugins/specs/lang/typescript.lua

return {
  -- NOTE (Fix #1): nvim-dap-vscode-js removed — dap.lua already registers
  -- pwa-node directly via js-debug-adapter. Same fix as javascript.lua.

  -- TypeScript LSP extras (import organizer, tsserver actions, inlay hints)
  -- FIX #2: typescript-tools.nvim manages its own tsserver instance.
  -- lsp.lua's ts_ls entry has been removed (see lsp.lua) to prevent two
  -- tsserver clients attaching per TypeScript buffer.
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
    keys = {
      -- FIX #6: Added typescriptreact to ft — TSTools commands work in .tsx
      -- files but the original keys were inaccessible there.
      { "<leader>tso", "<cmd>TSToolsOrganizeImports<cr>",     desc = "TS Organize Imports",   ft = { "typescript", "typescriptreact" } },
      { "<leader>tsi", "<cmd>TSToolsAddMissingImports<cr>",   desc = "TS Add Missing Imports", ft = { "typescript", "typescriptreact" } },
      { "<leader>tsr", "<cmd>TSToolsRemoveUnusedImports<cr>", desc = "TS Remove Unused",       ft = { "typescript", "typescriptreact" } },
      { "<leader>tsf", "<cmd>TSToolsFixAll<cr>",              desc = "TS Fix All",             ft = { "typescript", "typescriptreact" } },
      { "<leader>tsd", "<cmd>TSToolsGoToSourceDefinition<cr>",desc = "TS Source Definition",   ft = { "typescript", "typescriptreact" } },
    },
  },

  -- NOTE (Fix #3): neotest-vitest and neotest-jest registrations removed —
  -- test.lua already handles both centrally. Double insertion produced
  -- duplicate test results. Same fix as javascript.lua #3.

  -- Lint: eslint_d
  {
    "mfussenegger/nvim-lint",
    optional = true,
    -- FIX #4: optional=true config doesn't run — moved to init.
    -- Targeted assignment avoids overwriting lsp.lua's linter table.
    init = function()
      vim.api.nvim_create_autocmd("BufReadPost", {
        pattern  = { "*.ts", "*.tsx" },
        once     = true,
        callback = function()
          local ok, lint = pcall(require, "lint")
          if not ok then return end
          lint.linters_by_ft.typescript      = { "eslint_d" }
          lint.linters_by_ft.typescriptreact = { "eslint_d" }
        end,
      })
    end,
  },

  -- Conform: prettier for TSX (TS already registered in lsp.lua)
  {
    "stevearc/conform.nvim",
    optional = true,
    -- FIX #5: Removed typescript entry — lsp.lua already registers it.
    -- Kept typescriptreact which lsp.lua doesn't cover.
    opts = {
      formatters_by_ft = {
        typescriptreact = { "prettier" },
      },
    },
  },

  -- Treesitter
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
