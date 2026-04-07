-- lua/plugins/specs/lang/html.lua - HTML development
--
-- FIX (v2.2.5):
--   • vim.lsp.config() is Nvim 0.11-only API — calling it unguarded on 0.10
--     stable raises "attempt to call nil value" at BufReadPost. Guarded behind
--     vim.fn.has("nvim-0.11"); falls back to lspconfig.setup() on 0.10.

return {
  -- HTMLHint linting
  {
    "mfussenegger/nvim-lint",
    optional = true,
    init = function()
      vim.api.nvim_create_autocmd("BufReadPost", {
        pattern  = "*.html",
        once     = true,
        group    = vim.api.nvim_create_augroup("HtmlLint", { clear = true }),
        callback = function()
          local ok, lint = pcall(require, "lint")
          if not ok then return end
          lint.linters_by_ft.html = { "htmlhint" }
        end,
      })
    end,
  },

  -- Conform: prettier for HTML
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters_by_ft = opts.formatters_by_ft or {}
      opts.formatters_by_ft.html = { "prettier" }
    end,
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
    init = function()
      local cfg = {
        filetypes    = { "html", "htmldjango", "jinja.html" },
        init_options = { provideFormatter = false },
      }
      if vim.fn.has("nvim-0.11") == 1 then
        pcall(function() vim.lsp.config("html", cfg) end)
      else
        vim.api.nvim_create_autocmd("BufReadPost", {
          pattern = { "*.html", "*.htmldjango" },
          once    = true,
          group   = vim.api.nvim_create_augroup("HtmlLspCfg", { clear = true }),
          callback = function()
            local ok, lspconfig = pcall(require, "lspconfig")
            if ok then pcall(function() lspconfig.html.setup(cfg) end) end
          end,
        })
      end
    end,
  },
}
