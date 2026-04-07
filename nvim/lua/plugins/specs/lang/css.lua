-- lua/plugins/specs/lang/css.lua - CSS development
--
-- FIX (v2.2.5):
--   • vim.lsp.config() + vim.lsp.enable() are Nvim 0.11-only API — calling
--     them unguarded on 0.10 stable raises "attempt to call nil value".
--     Both call sites now check vim.fn.has("nvim-0.11") and fall back to
--     lspconfig.setup() on 0.10 (which mason-lspconfig already wires for
--     cssmodules_ls if the binary is present).
--   • cssmodules binary check was in init() which runs at spec-parse time
--     (startup), running a vim.fn.executable() probe on every launch even
--     for non-CSS projects. Moved into a BufReadPost autocmd so the check
--     only fires the first time a CSS file is opened.

return {
  -- cssmodules LSP (CSS Modules intellisense)
  {
    "neovim/nvim-lspconfig",
    optional = true,
    init = function()
      vim.api.nvim_create_autocmd("BufReadPost", {
        pattern  = { "*.css", "*.scss", "*.less", "*.tsx", "*.jsx" },
        once     = true,
        group    = vim.api.nvim_create_augroup("CssModulesLsp", { clear = true }),
        callback = function()
          if vim.fn.executable("cssmodules-language-server") ~= 1 then return end
          if vim.fn.has("nvim-0.11") == 1 then
            pcall(function()
              vim.lsp.config("cssmodules_ls", {
                init_options = { isCSSModules = true },
                filetypes    = { "css", "scss", "less", "typescriptreact", "javascriptreact" },
              })
              vim.lsp.enable("cssmodules_ls")
            end)
          else
            local ok, lspconfig = pcall(require, "lspconfig")
            if ok then
              pcall(function()
                lspconfig.cssmodules_ls.setup({
                  init_options = { isCSSModules = true },
                  filetypes    = { "css", "scss", "less", "typescriptreact", "javascriptreact" },
                })
              end)
            end
          end
        end,
      })
    end,
  },

  -- Tailwind CSS intellisense (canonical spec — server.override=false required)
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
      server         = { override = false },
    },
    config = function(_, opts)
      pcall(function() require("tailwind-tools").setup(opts) end)
    end,
  },

  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "css", "scss" })
      end
    end,
  },

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
