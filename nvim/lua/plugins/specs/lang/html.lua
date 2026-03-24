-- lua/plugins/specs/lang/html.lua - HTML development

return {
  -- HTMLHint linting
  {
    "mfussenegger/nvim-lint",
    optional = true,
    -- FIX #1: optional=true config doesn't run — moved to init.
    -- FIX #2: Use targeted assignment instead of tbl_extend on whole table
    -- to avoid overwriting entries registered by lsp.lua's lint config.
    init = function()
      vim.api.nvim_create_autocmd("BufReadPost", {
        pattern  = "*.html",
        once     = true,
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
    opts = {
      formatters_by_ft = {
        html = { "prettier" },
      },
    },
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
    -- FIX #3: opts.servers is the LazyVim pattern and doesn't work with this
    -- config's LSP setup (lsp.lua uses vim.lsp.config/enable directly).
    -- Moved to init using the same pattern as lsp.lua optional servers.
    init = function()
      vim.lsp.config("html", {
        filetypes    = { "html", "htmldjango", "jinja.html" },
        init_options = {
          provideFormatter = false,  -- defer formatting to prettier
        },
      })
    end,
  },
}
