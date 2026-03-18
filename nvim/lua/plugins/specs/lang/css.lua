-- lua/plugins/specs/lang/css.lua - CSS development
-- cssmodules_ls is registered via nvim-lspconfig optional override,
-- consistent with the unified vim.lsp.config pattern in lsp.lua.

return {
  -- cssmodules LSP (CSS Modules intellisense)
  {
    "neovim/nvim-lspconfig",
    optional = true,
    config = function()
      -- Only enable if binary is present (installed via npm: cssmodules-language-server)
      if vim.fn.executable("cssmodules-language-server") == 1 then
        vim.lsp.config("cssmodules_ls", {
          init_options = { isCSSModules = true },
          filetypes    = { "css", "scss", "less", "typescriptreact", "javascriptreact" },
        })
        vim.lsp.enable("cssmodules_ls")
      end
    end,
  },

  -- Tailwind CSS intellisense
  {
    "luckasRanarison/tailwind-tools.nvim",
    ft = {
      "html", "css", "javascript", "typescript",
      "jsx", "tsx", "typescriptreact", "javascriptreact",
      "svelte", "vue",
    },
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    opts = {
      document_color = { enabled = true, kind = "inline" },
      conceal        = { enabled = false },
    },
  },

  -- Treesitter: CSS / SCSS parsers
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "css", "scss" })
      end
    end,
  },

  -- Conform: CSS formatters
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = {
      formatters_by_ft = {
        css  = { "prettier" },
        scss = { "prettier" },
        less = { "prettier" },
      },
    },
  },

  -- Lint: stylelint for CSS / SCSS
  {
    "mfussenegger/nvim-lint",
    optional = true,
    config = function()
      local lint = require("lint")
      lint.linters_by_ft = vim.tbl_extend("force", lint.linters_by_ft or {}, {
        css  = { "stylelint" },
        scss = { "stylelint" },
      })
    end,
  },
}
