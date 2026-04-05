-- lua/plugins/specs/lsp.lua
--
-- FIX (v2.2.3):
--   • <leader>,r: duplicate map removed. The non-expr version (map helper) was
--     registering first, then the raw vim.keymap.set with expr=true overwrote
--     it on some Neovim builds. Now a single registration with expr=true.
--   • conform format_on_save: added vim.g.disable_autoformat check so
--     ToggleAutoformat command in commands.lua actually works.
--   • actions-preview spec retained; <leader>,t toggle diagnostics present.

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
        "tailwindcss",
      },
      automatic_installation = true,
    },
  },

  {
    "aznhe21/actions-preview.nvim",
    lazy = true,
    opts = {
      telescope = {
        sorting_strategy = "ascending",
        layout_strategy  = "vertical",
        layout_config    = { width = 0.8, height = 0.9, prompt_position = "top" },
      },
    },
    config = function(_, opts)
      pcall(function() require("actions-preview").setup(opts) end)
    end,
  },

  {
    "neovim/nvim-lspconfig",
    event        = { "BufReadPre", "BufNewFile" },
    dependencies = {
      "mason-lspconfig.nvim",
      "saghen/blink.cmp",
      "aznhe21/actions-preview.nvim",
    },
    config = function()
      -- ── Capabilities helper ───────────────────────────────────────────
      local function get_capabilities()
        local ok, blink = pcall(require, "blink.cmp")
        if ok then
          return blink.get_lsp_capabilities()
        end
        return vim.lsp.protocol.make_client_capabilities()
      end

      local function lsp_setup(server, config)
        config = config or {}
        config.capabilities = get_capabilities()
        pcall(function()
          vim.lsp.config(server, config)
          vim.lsp.enable(server)
        end)
      end

      -- ── LspAttach keymaps ─────────────────────────────────────────────
      vim.api.nvim_create_autocmd("LspAttach", {
        group    = vim.api.nvim_create_augroup("LspKeymaps", { clear = true }),
        callback = function(e)
          local map = function(keys, fn, desc, mode)
            vim.keymap.set(mode or "n", keys, fn,
              { buffer = e.buf, desc = "LSP: " .. desc })
          end

          map("gd",        vim.lsp.buf.definition,     "Go to Definition")
          map("gD",        vim.lsp.buf.declaration,     "Go to Declaration")
          map("gi",        vim.lsp.buf.implementation,  "Go to Implementation")
          map("gr",        vim.lsp.buf.references,      "References")
          map("K",         vim.lsp.buf.hover,           "Hover Docs")
          map("<leader>k", vim.lsp.buf.signature_help,  "Signature Help")

          local function code_action()
            local ok, ap = pcall(require, "actions-preview")
            if ok then ap.code_actions() else vim.lsp.buf.code_action() end
          end
          map("<leader>,a", code_action, "Code Action")
          map("<leader>,a", code_action, "Code Action", "v")

          -- FIX: single registration with expr=true only.
          -- Previously registered twice (once without expr, once with),
          -- causing the literal string to fire on some Neovim builds.
          vim.keymap.set("n", "<leader>,r", function()
            return ":IncRename " .. vim.fn.expand("<cword>")
          end, { buffer = e.buf, expr = true, desc = "LSP: Rename Symbol" })

          local function fmt()
            pcall(function()
              require("conform").format({ bufnr = e.buf, lsp_fallback = true })
            end)
          end
          map("<leader>,f", fmt, "Format")
          map("<leader>,f", fmt, "Format Range", "v")

          map("<leader>,d", vim.diagnostic.open_float,   "Diagnostic Float")
          map("<leader>,l", vim.diagnostic.setloclist,   "Diagnostic List")

          -- Toggle diagnostics (buffer-local)
          map("<leader>,t", function()
            local bufnr = e.buf
            local enabled = vim.diagnostic.is_enabled({ bufnr = bufnr })
            vim.diagnostic.enable(not enabled, { bufnr = bufnr })
            vim.notify("Diagnostics " .. (enabled and "disabled" or "enabled"))
          end, "Toggle Diagnostics")

          map("<leader>ty", vim.lsp.buf.type_definition, "Type Definition")
          map("]d",         vim.diagnostic.goto_next,    "Next Diagnostic")
          map("[d",         vim.diagnostic.goto_prev,    "Prev Diagnostic")

          local client = vim.lsp.get_client_by_id(e.data.client_id)
          if client and client.server_capabilities.inlayHintProvider then
            map("<leader>,i", function()
              local enabled = vim.lsp.inlay_hint.is_enabled({ bufnr = e.buf })
              vim.lsp.inlay_hint.enable(not enabled, { bufnr = e.buf })
              vim.notify("Inlay hints " .. (enabled and "off" or "on"))
            end, "Toggle Inlay Hints")
          end

          if client and client.server_capabilities.documentSymbolProvider then
            local ok_navic, navic = pcall(require, "nvim-navic")
            if ok_navic then
              pcall(function() navic.attach(client, e.buf) end)
            end
          end

          map("<leader>,o", "<cmd>AerialToggle<cr>", "Code Outline")
        end,
      })

      -- ── Server configurations ─────────────────────────────────────────
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
        lsp_setup(server, config)
      end

      local optional = {
        { name = "vhdl_ls",  binary = "vhdl_ls",
          config = { filetypes = { "vhdl", "vhd" } } },
        { name = "fortls",   binary = "fortls",   config = {} },
        { name = "sqls",     binary = "sqls",     config = {} },
        { name = "cobol_ls", binary = "cobol-language-server",
          config = {
            filetypes = { "cobol" },
            settings  = { cobol = { dialects = { "gnucobol", "ibm" } } },
          },
        },
      }

      for _, entry in ipairs(optional) do
        if vim.fn.executable(entry.binary) == 1 then
          lsp_setup(entry.name, entry.config)
        end
      end

      -- ── Diagnostics UI ────────────────────────────────────────────────
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
        css        = { "prettier" },
        c          = { "clang-format" },
        cpp        = { "clang-format" },
        fortran    = { "fprettify" },
      },
      -- FIX: respect vim.g.disable_autoformat so ToggleAutoformat works.
      -- Previously this was an unconditional table; the command was a no-op.
      format_on_save = function(bufnr)
        if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then
          return
        end
        return { timeout_ms = 500, lsp_format = "fallback" }
      end,
    },
  },

  {
    "mfussenegger/nvim-lint",
    event  = "BufReadPost",
    config = function()
      local lint = require("lint")

      local base = {
        python     = { "ruff" },
        javascript = { "eslint_d" },
        typescript = { "eslint_d" },
        sh         = { "shellcheck" },
        ruby       = { "rubocop" },
      }

      for ft, linters in pairs(base) do
        lint.linters_by_ft[ft] = lint.linters_by_ft[ft] or {}
        for _, l in ipairs(linters) do
          local found = false
          for _, existing in ipairs(lint.linters_by_ft[ft]) do
            if existing == l then found = true; break end
          end
          if not found then table.insert(lint.linters_by_ft[ft], l) end
        end
      end

      if vim.fn.executable("pylint") == 1 then
        lint.linters_by_ft.python = lint.linters_by_ft.python or {}
        if not vim.tbl_contains(lint.linters_by_ft.python, "pylint") then
          table.insert(lint.linters_by_ft.python, "pylint")
        end
      end

      if vim.fn.executable("shellcheck") == 1 then
        lint.linters_by_ft.bash = { "shellcheck" }
        lint.linters_by_ft.sh   = { "shellcheck" }
      end

      vim.api.nvim_create_autocmd({ "BufWritePost", "InsertLeave" }, {
        group    = vim.api.nvim_create_augroup("NvimLint", { clear = true }),
        callback = function()
          pcall(function() lint.try_lint() end)
        end,
      })
    end,
  },
}
