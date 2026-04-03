-- lua/plugins/specs/lsp.lua - LSP configuration (Nvim 0.11+, blink.cmp aware)

return {
  {
    "williamboman/mason.nvim",
    cmd   = "Mason",
    build = ":MasonUpdate",
    opts  = { ui = { border = "rounded" } },
  },

  {
    "williamboman/mason-lspconfig.nvim",
    dependencies = "mason.nvim",
    opts = {
      ensure_installed = {
        "lua_ls", "basedpyright",
        "html", "cssls", "jsonls", "yamlls",
        "clangd", "gopls", "solargraph",
        "kotlin_language_server",
        "zls",
        "tailwindcss", -- owned here; tailwind-tools handles UI only
      },
      automatic_installation = true,
    },
  },

  {
    "neovim/nvim-lspconfig",
    event        = { "BufReadPre", "BufNewFile" },
    dependencies = { "mason-lspconfig.nvim", "saghen/blink.cmp" },
    config = function()
      -- Safe blink.cmp capabilities injection
      local ok_blink, blink = pcall(require, "blink.cmp")
      local capabilities = ok_blink and blink.get_lsp_capabilities() or nil

      if capabilities then
        vim.lsp.config("*", { capabilities = capabilities })
      else
        vim.notify("blink.cmp capabilities not available", vim.log.levels.WARN)
      end

      -- LspAttach keymaps — SOLE OWNER
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

          -- Format via conform.nvim
          map("<leader>,f", function()
            pcall(function()
              require("conform").format({ bufnr = e.buf, lsp_fallback = true })
            end)
          end, "Format")

          map("<leader>,d", vim.diagnostic.open_float,   "Diagnostic Float")
          map("<leader>,l", vim.diagnostic.setloclist,   "Diagnostic List")
          map("<leader>ty", vim.lsp.buf.type_definition, "Type Definition")
          map("]d",         vim.diagnostic.goto_next,    "Next Diagnostic")
          map("[d",         vim.diagnostic.goto_prev,    "Prev Diagnostic")

          -- Visual mode keymaps
          vim.keymap.set("v", "<leader>,a", vim.lsp.buf.code_action,
            { buffer = e.buf, desc = "LSP: Code Action (fallback)" })
          vim.keymap.set("v", "<leader>,f", function()
            pcall(function()
              require("conform").format({ bufnr = e.buf, lsp_fallback = true })
            end)
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
          cmd = { vim.fn.stdpath("data") .. "/mason/bin/elixir-ls" },
        },
        -- tailwindcss LSP is owned here; tailwind-tools.nvim handles UI only
        -- (server.override = false is set in css.lua)
        tailwindcss            = {},
        html                   = {},
        cssls                  = {},
        jsonls                 = {},
        yamlls                 = {},
        clangd                 = {},
        kotlin_language_server = {},
        zls                    = {},
      }

      for server, config in pairs(servers) do
        pcall(function()
          vim.lsp.config(server, config)
          vim.lsp.enable(server)
        end)
      end

      -- Optional servers with binary checks
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
          pcall(function()
            vim.lsp.config(entry.name, entry.config)
            vim.lsp.enable(entry.name)
          end)
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

  {
    "j-hui/fidget.nvim",
    event = "LspAttach",
    opts  = { notification = { window = { winblend = 0 } } },
  },

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
        html       = { "prettier" },
        css        = { "prettier", "stylelint" },
        c          = { "clang-format" },
        cpp        = { "clang-format" },
        fortran    = { "fprettify" },
      },
      format_on_save = {
        timeout_ms = 500,
        lsp_format = "fallback",
      },
    },
  },

  {
    "mfussenegger/nvim-lint",
    event  = "BufReadPost",
    config = function()
      require("lint").linters_by_ft = {
        python     = { "ruff" },
        javascript = { "eslint_d" },
        typescript = { "eslint_d" },
        sh         = { "shellcheck" },
        ruby       = { "rubocop" },
      }

      vim.api.nvim_create_autocmd({ "BufWritePost", "InsertLeave" }, {
        group    = vim.api.nvim_create_augroup("NvimLint", { clear = true }),
        callback = function()
          pcall(function() require("lint").try_lint() end)
        end,
      })
    end,
  },
}
