-- lua/plugins/specs/lang/css.lua - CSS development

return {
  -- CSS variables / custom property completion
  {
    "antonk52/cssmodules-language-server",
    ft = { "css", "scss", "less", "typescriptreact", "javascriptreact" },
    config = function()
      require("lspconfig").cssmodules_ls.setup({
        init_options = { isCSSModules = true },
      })
    end,
  },

  -- Tailwind CSS intellisense
  {
    "luckasRanarison/tailwind-tools.nvim",
    ft = { "html", "css", "javascript", "typescript", "jsx", "tsx", "typescriptreact", "javascriptreact", "svelte", "vue" },
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    opts = {
      document_color = { enabled = true, kind = "inline" },
      conceal = { enabled = false },
    },
  },

  -- PostCSS / SCSS syntax via treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "css", "scss" })
      end
    end,
  },

  -- Conform: add CSS formatters
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

  -- Lint: stylelint for CSS/SCSS
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
