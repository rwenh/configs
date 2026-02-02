-- lua/plugins/specs/lsp.lua - LSP configuration (Fixed for Nvim 0.11+)

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
        "lua_ls", "basedpyright", "rust_analyzer",
        "ts_ls", "html", "cssls", "jsonls", "yamlls",
        "clangd", "gopls",
        "solargraph",
        "elixirls",
        "kotlin_language_server",
        "zls",
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

      local servers = {
        lua_ls = {
          settings = {
            Lua = {
              diagnostics = { globals = { "vim" } },
              workspace = { checkThirdParty = false },
              telemetry = { enable = false },
            },
          },
        },
        basedpyright = {
          settings = {
            basedpyright = {
              analysis = {
                typeCheckingMode = "basic",
              },
            },
          },
        },
        rust_analyzer = {
          settings = {
            ["rust-analyzer"] = {
              checkOnSave = { command = "clippy" },
            },
          },
        },
        ts_ls = {},
        html = {},
        cssls = {},
        jsonls = {},
        yamlls = {},
        gopls = {
          settings = {
            gopls = {
              analyses = { unusedparams = true },
              staticcheck = true,
            },
          },
        },
        clangd = {},
        solargraph = {
          settings = {
            solargraph = {
              diagnostics = true,
              completion = true,
            },
          },
        },
        elixirls = {
          cmd = { vim.fn.expand("~/.local/share/nvim/mason/bin/elixir-ls") },
        },
        kotlin_language_server = {},
        zls = {},
      }

      -- CRITICAL FIX: Register config THEN enable
      for server, config in pairs(servers) do
        config.capabilities = capabilities
        vim.lsp.config(server, config)  -- Register the config
        vim.lsp.enable(server)          -- Then enable the server
      end
      
      -- VHDL LSP setup (optional - only if vhdl_ls is installed manually)
      if vim.fn.executable("vhdl_ls") == 1 then
        vim.lsp.config('vhdl_ls', {
          capabilities = capabilities,
          filetypes = { "vhdl", "vhd" },
        })
        vim.lsp.enable('vhdl_ls')
      end

      -- Fortran LSP (optional - only if fortls is installed)
      if vim.fn.executable("fortls") == 1 then
        vim.lsp.config('fortls', {
          capabilities = capabilities,
        })
        vim.lsp.enable('fortls')
      end

      -- COBOL LSP (optional - only if cobol_ls is installed)
      if vim.fn.executable("cobol-language-server") == 1 then
        vim.lsp.config('cobol_ls', {
          capabilities = capabilities,
          filetypes = { "cobol" },
          settings = {
            cobol = {
              dialects = { "gnucobol", "ibm" },
            },
          },
        })
        vim.lsp.enable('cobol_ls')
      end

      -- SQL LSP (optional - only if sqls is installed)
      if vim.fn.executable("sqls") == 1 then
        vim.lsp.config('sqls', {
          capabilities = capabilities,
        })
        vim.lsp.enable('sqls')
      end

      local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }
      
      vim.diagnostic.config({
        virtual_text = { prefix = "●" },
        signs = { text = signs },
        underline = true,
        severity_sort = true,
        float = { border = "rounded" },
      })
    end,
  },

  -- LSP signature
  {
    "ray-x/lsp_signature.nvim",
    event = "LspAttach",
    opts = {
      bind = true,
      handler_opts = { border = "rounded" },
    },
  },

  -- LSP progress
  {
    "j-hui/fidget.nvim",
    event = "LspAttach",
    opts = {},
  },

  -- Formatting
  {
    "stevearc/conform.nvim",
    event = "BufReadPre", -- Changed from BufWritePre for better performance
    opts = {
      formatters_by_ft = {
        lua = { "stylua" },
        python = { "black", "isort" },
        javascript = { "prettier" },
        typescript = { "prettier" },
        json = { "prettier" },
        yaml = { "prettier" },
        sh = { "shfmt" },
        go = { "goimports", "gofumpt" },
        ruby = { "rubocop" },
        kotlin = { "ktlint" },
        elixir = { "mix" },
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
        ruby = { "rubocop" },
      }
      vim.api.nvim_create_autocmd({ "BufWritePost", "InsertLeave" }, {
        callback = function() require("lint").try_lint() end,
      })
    end,
  },
}
