-- lua/plugins/specs/lang/css.lua - CSS development
--
-- FIX (v2.2.5):
--   • vim.lsp.config() + vim.lsp.enable() are Nvim 0.11-only API.
--     Guarded behind has("nvim-0.11"); falls back to lspconfig on 0.10.
--   • cssmodules binary check deferred to BufReadPost so it does not run
--     on startup for non-CSS projects.
--
-- OPT (v2.3.13):
--   • nvim-lint spec: BufReadPost + once=true autocmd wrapper removed.
--     lsp.lua owns the single BufWritePost/InsertLeave lint trigger; lang
--     specs only need to populate lint.linters_by_ft, which can be done
--     directly in init() without an extra autocmd layer.

return {
  -- ── cssmodules LSP (CSS Modules intellisense) ─────────────────────────
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

  -- ── Tailwind CSS intellisense (canonical — server.override=false) ─────
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

  -- ── Treesitter ────────────────────────────────────────────────────────
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "css", "scss" })
      end
    end,
  },

  -- ── Conform ───────────────────────────────────────────────────────────
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

  -- ── nvim-lint ─────────────────────────────────────────────────────────
  -- OPT: direct init() assignment — no autocmd wrapper needed.
  -- lsp.lua's BufWritePost/InsertLeave autocmd drives try_lint(); lang specs
  -- only need to populate linters_by_ft before that fires.
  {
    "mfussenegger/nvim-lint",
    optional = true,
    init = function()
      local ok, lint = pcall(require, "lint")
      if not ok then return end
      lint.linters_by_ft.css  = { "stylelint" }
      lint.linters_by_ft.scss = { "stylelint" }
    end,
  },
}
