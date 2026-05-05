-- lua/plugins/specs/lang/html.lua — HTML development
--
-- LSP:    html-lsp (this file — sole owner, not lsp.lua)
-- Format: prettier via lsp.lua conform
-- Lint:   htmlhint via this file
-- Deps:   web.lua must load before html.lua (autotag + emmet)
--

local shared = require("plugins.specs.lang.shared")

return {
  -- ── HTMLHint linting ────────────────────────────────────────────────────────
  {
    "mfussenegger/nvim-lint",
    optional = true,
    init = function()
      if vim.fn.executable("htmlhint") ~= 1 then return end
      local ok, lint = pcall(require, "lint")
      if not ok then return end
      lint.linters_by_ft.html = { "htmlhint" }
    end,
  },

  -- ── Conform ────────────────────────────────────────────────────────────────

  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters_by_ft = opts.formatters_by_ft or {}
      opts.formatters_by_ft.html = { "prettier" }
    end,
  },

  shared.treesitter({ "html" }),

  -- ── LSP: html-lsp — SOLE owner of html server config ──────────────────────

  {
    "neovim/nvim-lspconfig",
    optional = true,
    init = function()
      local cfg = {
        filetypes    = { "html", "htmldjango", "jinja.html" },
        init_options = {
          provideFormatter = false,
        },
      }

      if vim.fn.executable("vscode-html-language-server") ~= 1 then
        vim.notify("[html] html-lsp not found — run :MasonInstall html-lsp",
          vim.log.levels.WARN)
        return
      end

      if vim.fn.has("nvim-0.11") == 1 then
        pcall(function()
          vim.lsp.config("html", cfg)
          vim.lsp.enable("html")
        end)
      else
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
