-- lua/plugins/specs/lang/typescript.lua

return {
  -- DAP: vscode-js adapter
  {
    "mxsdev/nvim-dap-vscode-js",
    ft           = { "typescript", "typescriptreact" },
    dependencies = { "mfussenegger/nvim-dap" },
    opts         = { adapters = { "pwa-node", "pwa-chrome" } },
  },

  -- TypeScript LSP extras (twoslash queries, import organizer, tsserver actions)
  {
    "pmizio/typescript-tools.nvim",
    ft           = { "typescript", "typescriptreact", "javascript", "javascriptreact" },
    dependencies = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
    opts = {
      settings = {
        tsserver_file_preferences = {
          importModuleSpecifierPreference    = "non-relative",
          includeInlayParameterNameHints     = "literals",
          includeInlayVariableTypeHints      = false,
          includeInlayFunctionLikeReturnTypeHints = true,
        },
      },
    },
    keys = {
      { "<leader>tso", "<cmd>TSToolsOrganizeImports<cr>",    desc = "TS Organize Imports",   ft = "typescript" },
      { "<leader>tsi", "<cmd>TSToolsAddMissingImports<cr>",  desc = "TS Add Missing Imports", ft = "typescript" },
      { "<leader>tsr", "<cmd>TSToolsRemoveUnusedImports<cr>", desc = "TS Remove Unused",      ft = "typescript" },
      { "<leader>tsf", "<cmd>TSToolsFixAll<cr>",             desc = "TS Fix All",            ft = "typescript" },
      { "<leader>tsd", "<cmd>TSToolsGoToSourceDefinition<cr>", desc = "TS Source Definition", ft = "typescript" },
    },
  },

  -- Neotest adapter
  {
    "nvim-neotest/neotest",
    optional = true,
    dependencies = { "marilari88/neotest-vitest", "haydenmeade/neotest-jest" },
    opts = function(_, opts)
      opts.adapters = opts.adapters or {}
      table.insert(opts.adapters, require("neotest-vitest"))
      table.insert(opts.adapters, require("neotest-jest")({
        jestCommand = "npm test --",
      }))
    end,
  },

  -- Lint: eslint_d
  {
    "mfussenegger/nvim-lint",
    optional = true,
    config = function()
      local lint = require("lint")
      lint.linters_by_ft = vim.tbl_extend("force", lint.linters_by_ft or {}, {
        typescript      = { "eslint_d" },
        typescriptreact = { "eslint_d" },
      })
    end,
  },

  -- Conform: prettier
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = {
      formatters_by_ft = {
        typescript      = { "prettier" },
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
