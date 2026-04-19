-- lua/plugins/specs/lang/html.lua - HTML development
--
-- FIX (v2.2.5):
--   • vim.lsp.config() is Nvim 0.11-only API — calling it unguarded on 0.10
--     stable raises "attempt to call nil value" at BufReadPost. Guarded behind
--     vim.fn.has("nvim-0.11"); falls back to lspconfig.setup() on 0.10.
--
-- FIX (v2.3.11):
--   • This file is now the SOLE owner of the html LSP server configuration.
--     Previously lsp.lua also called lsp_setup("html", {}) with an empty config
--     table. On Nvim 0.11 the last vim.lsp.config("html", ...) call wins; load
--     order between lsp.lua's config() and html.lua's BufReadPost autocmd was
--     non-deterministic, meaning provideFormatter=false and the extra filetypes
--     were silently dropped on some launches.
--     Fix: lsp.lua's "html" servers{} entry has been removed. html.lua's
--     BufReadPost autocmd fires after lsp.lua's config(), making it the
--     reliable last writer. The html binary is still auto-installed because
--     "html" remains in mason-lspconfig ensure_installed.

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

  -- LSP: html-lsp — SOLE owner of the html server config.
  -- FIX (v2.3.11): lsp.lua's empty lsp_setup("html",{}) removed; this spec
  -- is the only place vim.lsp.config("html", cfg) is called. BufReadPost fires
  -- after lsp.lua's config(), so this always wins regardless of lazy load order.
  {
    "neovim/nvim-lspconfig",
    optional = true,
    init = function()
      local cfg = {
        filetypes    = { "html", "htmldjango", "jinja.html" },
        init_options = { provideFormatter = false },
      }

      if vim.fn.has("nvim-0.11") == 1 then
        -- Apply immediately — lsp.lua has already run config() by the time
        -- this init() executes, so we are guaranteed to be the last writer.
        pcall(function()
          vim.lsp.config("html", cfg)
          vim.lsp.enable("html")
        end)
      else
        -- On 0.10, defer to BufReadPost so lspconfig is loaded first.
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
