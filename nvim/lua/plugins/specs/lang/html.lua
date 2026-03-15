-- lua/plugins/specs/lang/html.lua - HTML development

return {
  -- HTMLHint linting
  {
    "mfussenegger/nvim-lint",
    optional = true,
    config = function()
      local lint = require("lint")
      lint.linters_by_ft = vim.tbl_extend("force", lint.linters_by_ft or {}, {
        html = { "htmlhint" },
      })
    end,
  },

  -- Conform: prettier for HTML
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = {
      formatters_by_ft = {
        html = { "prettier" },
      },
    },
  },

  -- Treesitter: HTML parser
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "html" })
      end
    end,
  },

  -- LSP extras: html-lsp specific settings
  {
    "neovim/nvim-lspconfig",
    optional = true,
    opts = {
      servers = {
        html = {
          filetypes = { "html", "htmldjango", "jinja.html" },
          init_options = {
            provideFormatter = false, -- defer to prettier
          },
        },
      },
    },
  },
}
