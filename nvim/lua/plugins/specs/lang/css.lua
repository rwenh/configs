-- lua/plugins/specs/lang/css.lua — CSS development
--
-- LSP:    cssls + cssmodules_ls via this file + lsp.lua
-- Format: prettier via lsp.lua conform
-- Lint:   stylelint via this file
-- Tailwind: tailwind-tools (gated on tailwind.config.* presence)
--

local shared = require("plugins.specs.lang.shared")

return {
  -- ── cssmodules LSP ──────────────────────────────────────────────────────────

  {
    "neovim/nvim-lspconfig",
    optional = true,
    init = function()
      vim.api.nvim_create_autocmd("BufReadPost", {
        pattern  = { "*.css", "*.scss", "*.less", "*.tsx", "*.jsx" },
        group    = vim.api.nvim_create_augroup("CssModulesLsp", { clear = true }),
        callback = function()
          if vim.fn.executable("cssmodules-language-server") ~= 1 then return end
          local cfg = {
            init_options = { isCSSModules = true },
            filetypes    = { "css", "scss", "less", "typescriptreact", "javascriptreact" },
          }
          if vim.fn.has("nvim-0.11") == 1 then
            pcall(function()
              vim.lsp.config("cssmodules_ls", cfg)
              vim.lsp.enable("cssmodules_ls")
            end)
          else
            local ok, lspconfig = pcall(require, "lspconfig")
            if ok then
              pcall(function() lspconfig.cssmodules_ls.setup(cfg) end)
            end
          end
        end,
      })
    end,
  },

  -- ── Tailwind CSS ───────────────────────────────────────────────────────────

  {
    "luckasRanaringer/tailwind-tools.nvim",
    cond = function()
      return vim.fn.findfile("tailwind.config.js",  ".;") ~= ""
          or vim.fn.findfile("tailwind.config.ts",  ".;") ~= ""
          or vim.fn.findfile("tailwind.config.cjs", ".;") ~= ""
    end,
    ft = shared.WEB_FT,
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    opts = {
      document_color = { enabled = true, kind = "inline" },
      conceal        = { enabled = false },
      -- server.override=false: tailwindcss LSP managed by lsp.lua servers table.
      server = { override = false },
    },
    config = function(_, opts)
      pcall(function() require("tailwind-tools").setup(opts) end)
    end,
  },

  shared.treesitter({ "css", "scss" }),

  -- ── Conform ────────────────────────────────────────────────────────────────
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

  -- ── nvim-lint: stylelint ───────────────────────────────────────────────────

  {
    "mfussenegger/nvim-lint",
    optional = true,
    init = function()
      if vim.fn.executable("stylelint") ~= 1 then return end
      local ok, lint = pcall(require, "lint")
      if not ok then return end
      lint.linters_by_ft.css  = { "stylelint" }
      lint.linters_by_ft.scss = { "stylelint" }
    end,
  },
}
