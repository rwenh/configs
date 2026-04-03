-- lua/plugins/specs/lang/css.lua - CSS development

return {
  -- cssmodules LSP (CSS Modules intellisense)
  {
    "neovim/nvim-lspconfig",
    optional = true,
    init = function()
      if vim.fn.executable("cssmodules-language-server") == 1 then
        pcall(function()
          vim.lsp.config("cssmodules_ls", {
            init_options = { isCSSModules = true },
            filetypes    = { "css", "scss", "less", "typescriptreact", "javascriptreact" },
          })
          vim.lsp.enable("cssmodules_ls")
        end)
      end
    end,
  },

  -- Tailwind CSS intellisense
  -- NOTE: server.override = false prevents tailwind-tools from using the
  -- deprecated require('lspconfig') framework. The tailwindcss LSP is owned
  -- by lsp.lua via vim.lsp.config / vim.lsp.enable (Nvim 0.11+ API).
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
      server         = { override = false }, -- don't touch lspconfig
    },
    config = function(_, opts)
      pcall(function() require("tailwind-tools").setup(opts) end)
    end,
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
    opts = function(_, opts)
      opts.formatters_by_ft = opts.formatters_by_ft or {}
      opts.formatters_by_ft.css  = { "prettier" }
      opts.formatters_by_ft.scss = { "prettier" }
      opts.formatters_by_ft.less = { "prettier" }
    end,
  },

  -- Lint: stylelint for CSS / SCSS
  {
    "mfussenegger/nvim-lint",
    optional = true,
    init = function()
      vim.api.nvim_create_autocmd("BufReadPost", {
        pattern  = { "*.css", "*.scss" },
        once     = true,
        group    = vim.api.nvim_create_augroup("CssLint", { clear = true }),
        callback = function()
          local ok, lint = pcall(require, "lint")
          if not ok then return end
          lint.linters_by_ft.css  = { "stylelint" }
          lint.linters_by_ft.scss = { "stylelint" }
        end,
      })
    end,
  },
}
