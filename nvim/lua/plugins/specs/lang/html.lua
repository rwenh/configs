-- lua/plugins/specs/lang/html.lua - HTML development
--
-- FIX (v2.2.5): vim.lsp.config() is Nvim 0.11-only; guarded + 0.10 fallback.
-- FIX (v2.3.11): html.lua is SOLE owner of html LSP config. BufReadPost in
--   init() fires after lsp.lua's config(), making this the reliable last writer.
--   "html" stays in mason-lspconfig ensure_installed for binary auto-install.
--
-- OPT (v2.3.13):
--   • nvim-lint spec: BufReadPost + once=true autocmd wrapper removed.
--     Direct init() assignment is sufficient (see css.lua note).
--     IMPORTANT: the html LSP spec MUST keep its BufReadPost (0.10 path) and
--     direct init() call (0.11 path) — that is load-order logic, not lint
--     boilerplate, and cannot be simplified away.

return {
  -- ── HTMLHint linting ──────────────────────────────────────────────────
  {
    "mfussenegger/nvim-lint",
    optional = true,
    init = function()
      local ok, lint = pcall(require, "lint")
      if not ok then return end
      lint.linters_by_ft.html = { "htmlhint" }
    end,
  },

  -- ── Conform ───────────────────────────────────────────────────────────
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters_by_ft = opts.formatters_by_ft or {}
      opts.formatters_by_ft.html = { "prettier" }
    end,
  },

  -- ── Treesitter ────────────────────────────────────────────────────────
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "html" })
      end
    end,
  },

  -- ── LSP: html-lsp — SOLE owner of html server config ─────────────────
  {
    "neovim/nvim-lspconfig",
    optional = true,
    init = function()
      local cfg = {
        filetypes    = { "html", "htmldjango", "jinja.html" },
        init_options = { provideFormatter = false },
      }

      if vim.fn.has("nvim-0.11") == 1 then
        -- init() runs after lsp.lua config() — guaranteed last writer on 0.11
        pcall(function()
          vim.lsp.config("html", cfg)
          vim.lsp.enable("html")
        end)
      else
        -- 0.10: defer until lspconfig is loaded
        vim.api.nvim_create_autocmd("BufReadPost", {
          pattern  = { "*.html", "*.htmldjango" },
          once     = true,
          group    = vim.api.nvim_create_augroup("HtmlLspCfg", { clear = true }),
          callback = function()
            local ok, lspconfig = pcall(require, "lspconfig")
            if ok then pcall(function() lspconfig.html.setup(cfg) end) end
          end,
        })
      end
    end,
  },
}
