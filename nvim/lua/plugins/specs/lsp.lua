-- lua/plugins/specs/lsp.lua - LSP configuration

return {
  -- Mason
  {
    "williamboman/mason.nvim",
    cmd = "Mason",
    build = ":MasonUpdate",
    opts = {},
  },

  -- Mason LSP config
  {
    "williamboman/mason-lspconfig.nvim",
    dependencies = "mason.nvim",
    opts = {
      ensure_installed = {
        "lua_ls", "pyright", "rust_analyzer",
        "ts_ls", "html", "cssls", "jsonls", "yamlls",
        "clangd",
      },
      automatic_installation = true,
    },
  },

  -- LSP config
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
      "mason-lspconfig.nvim",
      "hrsh7th/cmp-nvim-lsp",
    },
    config = function()
      local capabilities = require("cmp_nvim_lsp").default_capabilities()

      vim.api.nvim_create_autocmd("LspAttach", {
        group = vim.api.nvim_create_augroup("LspConfig", {}),
        callback = function(e)
          local opts = { buffer = e.buf }
          local map = vim.keymap.set
          map("n", "gd", vim.lsp.buf.definition, opts)
          map("n", "gD", vim.lsp.buf.declaration, opts)
          map("n", "gi", vim.lsp.buf.implementation, opts)
          map("n", "gr", vim.lsp.buf.references, opts)
          map("n", "K", vim.lsp.buf.hover, opts)
          map("n", "<leader>rn", vim.lsp.buf.rename, opts)
          map("n", "<leader>ca", vim.lsp.buf.code_action, opts)
          map("v", "<leader>ca", vim.lsp.buf.code_action, opts)
        end,
      })

      -- Setup each server using vim.lsp.config (new API)
      local servers = {
        lua_ls = {},
        pyright = {},
        rust_analyzer = {},
        ts_ls = {},
        html = {},
        cssls = {},
        jsonls = {},
        yamlls = {},
        clangd = {},
      }

      for server, config in pairs(servers) do
        config.capabilities = capabilities
        vim.lsp.config(server, config)
        vim.lsp.enable(server)
      end

      vim.diagnostic.config({
        virtual_text = { prefix = "●" },
        signs = true,
        underline = true,
        severity_sort = true,
        float = { border = "rounded" },
      })
    end,
  },

  -- Formatting
  {
    "stevearc/conform.nvim",
    event = "BufWritePre",
    opts = {
      formatters_by_ft = {
        lua = { "stylua" },
        python = { "black", "isort" },
        javascript = { "prettier" },
        typescript = { "prettier" },
        json = { "prettier" },
        yaml = { "prettier" },
        sh = { "shfmt" },
      },
      format_on_save = { timeout_ms = 500, lsp_format = "fallback" },
    },
  },

  -- Linting
  {
    "mfussenegger/nvim-lint",
    event = "BufReadPre",
    config = function()
      require("lint").linters_by_ft = {
        python = { "ruff" },
        javascript = { "eslint_d" },
        sh = { "shellcheck" },
      }
      vim.api.nvim_create_autocmd({ "BufWritePost", "InsertLeave" }, {
        callback = function() require("lint").try_lint() end,
      })
    end,
  },
}