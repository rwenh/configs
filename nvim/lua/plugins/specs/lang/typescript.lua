-- lua/plugins/specs/lang/typescript.lua — TypeScript development
--

local shared = require("plugins.specs.lang.shared")

return {
  -- ── typescript-tools ───────────────────────────────────────────────────────
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
        desc = "TS Organize Imports",         ft = shared.JS_TS_FT },
      { "<leader>tsi", "<cmd>TSToolsAddMissingImports<cr>",
        desc = "TS Add Missing Imports",      ft = shared.JS_TS_FT },
      { "<leader>tsr", "<cmd>TSToolsRemoveUnusedImports<cr>",
        desc = "TS Remove Unused Imports",    ft = shared.JS_TS_FT },
      { "<leader>tsf", "<cmd>TSToolsFixAll<cr>",
        desc = "TS Fix All Auto-fixable",     ft = shared.JS_TS_FT },
      { "<leader>tsd", "<cmd>TSToolsGoToSourceDefinition<cr>",
        desc = "TS Go To Source Def (.ts not .d.ts)", ft = shared.JS_TS_FT },
    },
  },

  -- ── Neogen: TSDoc (TypeScript-only; JS uses jsdoc convention) ─────────────

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
