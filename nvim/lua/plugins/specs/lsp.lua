-- lua/plugins/specs/lsp.lua - LSP configuration (Nvim 0.11+, blink.cmp aware)

return {
  -- Mason
  {
    "williamboman/mason.nvim",
    cmd   = "Mason",
    build = ":MasonUpdate",
    opts  = { ui = { border = "rounded" } },
  },

  -- Mason LSP bridge
  {
    "williamboman/mason-lspconfig.nvim",
    dependencies = "mason.nvim",
    opts = {
      ensure_installed = {
        "lua_ls", "basedpyright", "rust_analyzer",
        "ts_ls", "html", "cssls", "jsonls", "yamlls",
        "clangd", "gopls", "solargraph", "elixirls",
        "kotlin_language_server", "zls",
      },
      automatic_installation = true,
    },
  },

  -- Core LSP config
  {
    "neovim/nvim-lspconfig",
    event        = { "BufReadPre", "BufNewFile" },
    dependencies = { "mason-lspconfig.nvim", "saghen/blink.cmp" },
    config = function()
      -- Inject blink.cmp capabilities globally (Nvim 0.11+ pattern)
      vim.lsp.config("*", {
        capabilities = require("blink.cmp").get_lsp_capabilities(),
      })

      -- Shared on_attach keymaps
      vim.api.nvim_create_autocmd("LspAttach", {
        group    = vim.api.nvim_create_augroup("LspKeymaps", { clear = true }),
        callback = function(e)
          local map = function(keys, fn, desc)
            vim.keymap.set("n", keys, fn, { buffer = e.buf, desc = "LSP: " .. desc })
          end
          map("gd",         vim.lsp.buf.definition,      "Go to Definition")
          map("gD",         vim.lsp.buf.declaration,     "Go to Declaration")
          map("gi",         vim.lsp.buf.implementation,  "Go to Implementation")
          map("gr",         vim.lsp.buf.references,      "References")
          map("K",          vim.lsp.buf.hover,           "Hover Docs")
          map("<leader>k",  vim.lsp.buf.signature_help,  "Signature Help")
          map("<leader>,r", vim.lsp.buf.rename,          "Rename")
          map("<leader>,a", vim.lsp.buf.code_action,     "Code Action")
          map("<leader>,f", function() vim.lsp.buf.format({ timeout_ms = 3000 }) end, "Format")
          map("<leader>,d", vim.diagnostic.open_float,   "Diagnostic Float")
          map("<leader>,l", vim.diagnostic.setloclist,   "Diagnostic List")
          map("<leader>td", vim.lsp.buf.type_definition, "Type Definition")
          map("]d",         vim.diagnostic.goto_next,    "Next Diagnostic")
          map("[d",         vim.diagnostic.goto_prev,    "Prev Diagnostic")
          vim.keymap.set("v", "<leader>,a", vim.lsp.buf.code_action, { buffer = e.buf, desc = "LSP: Code Action" })
          vim.keymap.set("v", "<leader>,f", function()
            vim.lsp.buf.format({ timeout_ms = 3000, range = true })
          end, { buffer = e.buf, desc = "LSP: Format Range" })
        end,
      })

      -- Server configs
      local servers = {
        lua_ls = {
          settings = {
            Lua = {
              diagnostics   = { globals = { "vim" } },
              workspace     = { checkThirdParty = false },
              telemetry     = { enable = false },
            },
          },
        },
        basedpyright = {
          settings = {
            basedpyright = { analysis = { typeCheckingMode = "basic" } },
          },
        },
        rust_analyzer = {
          settings = {
            ["rust-analyzer"] = { checkOnSave = { command = "clippy" } },
          },
        },
        gopls = {
          settings = {
            gopls = {
              analyses    = { unusedparams = true },
              staticcheck = true,
              gofumpt     = true,
            },
          },
        },
        solargraph = {
          settings = { solargraph = { diagnostics = true, completion = true } },
        },
        elixirls = {
          cmd = { vim.fn.expand("~/.local/share/nvim/mason/bin/elixir-ls") },
        },
        ts_ls                  = {},
        html                   = {},
        cssls                  = {},
        jsonls                 = {},
        yamlls                 = {},
        clangd                 = {},
        kotlin_language_server = {},
        zls                    = {},
      }

      for server, config in pairs(servers) do
        vim.lsp.config(server, config)
        vim.lsp.enable(server)
      end

      -- Optional servers: explicit binary checks (no gsub heuristics)
      local optional = {
        {
          name   = "vhdl_ls",
          binary = "vhdl_ls",
          config = { filetypes = { "vhdl", "vhd" } },
        },
        {
          name   = "fortls",
          binary = "fortls",
          config = {},
        },
        {
          name   = "sqls",
          binary = "sqls",
          config = {},
        },
        {
          name   = "cobol_ls",
          binary = "cobol-language-server",
          config = {
            filetypes = { "cobol" },
            settings  = { cobol = { dialects = { "gnucobol", "ibm" } } },
          },
        },
      }

      for _, entry in ipairs(optional) do
        if vim.fn.executable(entry.binary) == 1 then
          vim.lsp.config(entry.name, entry.config)
          vim.lsp.enable(entry.name)
        end
      end

      -- Diagnostics UI
      vim.diagnostic.config({
        virtual_text     = { prefix = "●", spacing = 4 },
        signs            = { text = { Error = " ", Warn = " ", Hint = " ", Info = " " } },
        underline        = true,
        severity_sort    = true,
        float            = { border = "rounded", source = "always" },
        update_in_insert = false,
      })
    end,
  },

  -- LSP signature help
  {
    "ray-x/lsp_signature.nvim",
    event = "LspAttach",
    opts  = { bind = true, handler_opts = { border = "rounded" } },
  },

  -- LSP progress indicator
  {
    "j-hui/fidget.nvim",
    event = "LspAttach",
    opts  = { notification = { window = { winblend = 0 } } },
  },

  -- Formatting
  {
    "stevearc/conform.nvim",
    event = "BufWritePre",
    opts  = {
      formatters_by_ft = {
        lua        = { "stylua" },
        python     = { "black", "isort" },
        javascript = { "prettier" },
        typescript = { "prettier" },
        json       = { "prettier" },
        yaml       = { "prettier" },
        sh         = { "shfmt" },
        go         = { "goimports", "gofumpt" },
        ruby       = { "rubocop" },
        kotlin     = { "ktlint" },
        elixir     = { "mix" },
      },
      format_on_save = {
        timeout_ms = 500,
        lsp_format = "fallback",
      },
    },
  },

  -- Linting
  {
    "mfussenegger/nvim-lint",
    event  = "BufReadPre",
    config = function()
      require("lint").linters_by_ft = {
        python     = { "ruff" },
        javascript = { "eslint_d" },
        typescript = { "eslint_d" },
        sh         = { "shellcheck" },
        ruby       = { "rubocop" },
      }
      vim.api.nvim_create_autocmd({ "BufWritePost", "InsertLeave" }, {
        callback = function() require("lint").try_lint() end,
      })
    end,
  },
}
