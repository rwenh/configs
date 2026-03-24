-- lua/plugins/specs/lang/css.lua - CSS development
-- cssmodules_ls is registered via nvim-lspconfig optional override,
-- consistent with the unified vim.lsp.config pattern in lsp.lua.

return {
  -- cssmodules LSP (CSS Modules intellisense)
  {
    "neovim/nvim-lspconfig",
    optional = true,
    -- FIX #5: optional=true specs do not run their config function — Lazy
    -- only merges opts. Moved registration into init which does run on
    -- optional specs, guarded by executable check.
    init = function()
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
    -- FIX #6: optional=true config functions don't run — moved to init.
    -- FIX #7: Use targeted per-key assignment instead of tbl_extend on the
    -- whole table. tbl_extend risks losing entries set by lsp.lua's config
    -- if load order causes a full table replacement.
    init = function()
      vim.api.nvim_create_autocmd("BufReadPost", {
        pattern  = { "*.css", "*.scss" },
        once     = true,
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
