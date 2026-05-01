-- lua/plugins/specs/lsp.lua
--

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
      -- Keep in sync with MASON_PACKAGES.lsp in commands.lua.
      ensure_installed = {
        "lua_ls", "basedpyright",
        "html", "cssls", "jsonls", "yamlls",
        "clangd", "gopls", "solargraph",
        "kotlin_language_server",
        "zls", "tailwindcss", "elixirls",
        "fortls", "sqls", "jdtls",
      },
      automatic_installation = true,
      -- Suppress default handler to prevent double-attach on Nvim 0.11.
      handlers = { function() end },
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
      local nvim_011 = vim.fn.has("nvim-0.11") == 1

      -- ── Capabilities ────────────────────────────────────────────────────────

      local function get_capabilities()
        local ok, blink = pcall(require, "blink.cmp")
        if ok then
          local ok2, caps = pcall(blink.get_lsp_capabilities)
          if ok2 and type(caps) == "table" and caps.textDocument ~= nil then
            return caps
          end
        end
        return vim.lsp.protocol.make_client_capabilities()
      end

      -- ── lsp_setup ───────────────────────────────────────────────────────────

      local function lsp_setup(server, config)
        config = config or {}
        config.capabilities = vim.tbl_deep_extend(
          "force",
          get_capabilities(),
          config.capabilities or {}
        )
        if nvim_011 then
          pcall(function()
            vim.lsp.config(server, config)
            vim.lsp.enable(server)
          end)
        else
          local ok_lc, lspconfig = pcall(require, "lspconfig")
          if ok_lc then
            pcall(function() lspconfig[server].setup(config) end)
          end
        end
      end

      -- ── diag_jump ────────────────────────────────────────────────────────────

      local function diag_jump(count)
        if nvim_011 then
          pcall(function()
            vim.diagnostic.jump({ count = count, float = true })
          end)
        else
          local fn   = count > 0 and vim.diagnostic.goto_next or vim.diagnostic.goto_prev
          local steps = math.abs(count)
          for _ = 1, steps do
            pcall(fn, { float = true })
          end
        end
      end

      -- ── cmd_or_fallback ──────────────────────────────────────────────────────

      local function cmd_or_fallback(cmd_str, fallback_fn)
        local ok = pcall(vim.cmd, cmd_str)
        if not ok then pcall(fallback_fn) end
      end

      -- ── LspAttach keymaps ────────────────────────────────────────────────────
      vim.api.nvim_create_autocmd("LspAttach", {
        group    = vim.api.nvim_create_augroup("LspKeymaps", { clear = true }),
        callback = function(e)
          local function map(keys, fn, desc, mode)
            vim.keymap.set(mode or "n", keys, fn,
              { buffer = e.buf, desc = "LSP: " .. desc })
          end

          map("gd",        vim.lsp.buf.definition,     "Go to Definition")
          map("gD",        vim.lsp.buf.declaration,    "Go to Declaration")
          map("gi",        vim.lsp.buf.implementation, "Go to Implementation")
          map("gr",        vim.lsp.buf.references,     "References")
          map("K",         vim.lsp.buf.hover,          "Hover Docs")
          map("<leader>k", vim.lsp.buf.signature_help, "Signature Help")

          -- Code action
          local function code_action()
            cmd_or_fallback(
              "ActionsPreview",
              vim.lsp.buf.code_action
            )
          end
          map("<leader>,a", code_action, "Code Action")
          map("<leader>,a", code_action, "Code Action", "v")

          map("<leader>,r", function()
            local word = vim.fn.expand("<cword>")
            cmd_or_fallback(
              "IncRename " .. word,
              vim.lsp.buf.rename
            )
          end, "Rename Symbol")

          local function fmt()
            local timeout = (vim.g.format_timeout_ms or 500)
            pcall(function()
              require("conform").format({
                bufnr      = e.buf,
                timeout_ms = timeout,
                lsp_format = "fallback",
              })
            end)
          end
          map("<leader>,f", fmt, "Format")
          map("<leader>,f", fmt, "Format Range", "v")

          map("<leader>,d", vim.diagnostic.open_float, "Diagnostic Float")
          map("<leader>,l", vim.diagnostic.setloclist, "Diagnostic List")

          map("<leader>,t", function()
            local en = vim.diagnostic.is_enabled({ bufnr = e.buf })
            vim.diagnostic.enable(not en, { bufnr = e.buf })
            vim.notify("Diagnostics " .. (en and "disabled" or "enabled"))
          end, "Toggle Diagnostics")

          map("<leader>ty", vim.lsp.buf.type_definition, "Type Definition")

          map("]d", function() diag_jump(1)  end, "Next Diagnostic")
          map("[d", function() diag_jump(-1) end, "Prev Diagnostic")

          local client = vim.lsp.get_client_by_id(e.data.client_id)

          if client and client.server_capabilities.inlayHintProvider then
            map("<leader>,i", function()
              local en = vim.lsp.inlay_hint.is_enabled({ bufnr = e.buf })
              vim.lsp.inlay_hint.enable(not en, { bufnr = e.buf })
              vim.notify("Inlay hints " .. (en and "off" or "on"))
            end, "Toggle Inlay Hints")
          end

          if client and client.server_capabilities.documentSymbolProvider then
            local ok_navic, navic = pcall(require, "nvim-navic")
            if ok_navic then
              pcall(function() navic.attach(client, e.buf) end)
            end
          end

          map("<leader>,o", function()
            cmd_or_fallback(
              "Trouble lsp_document_symbols toggle",
              vim.lsp.buf.document_symbol
            )
          end, "Code Outline (Trouble symbols)")
        end,
      })

      -- ── Server configurations ────────────────────────────────────────────────
      -- Servers with non-trivial configuration.
      local servers = {
        lua_ls = {
          settings = {
            Lua = {
              diagnostics = { globals = { "vim" } },
              workspace   = { checkThirdParty = false },
              telemetry   = { enable = false },
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
          cmd = (function()
            local data = vim.fn.stdpath("data")
            -- Mason package layout: packages/elixir-ls/language_server.sh
            local mason_ls = data .. "/mason/packages/elixir-ls/language_server.sh"
            if vim.fn.filereadable(mason_ls) == 1 then return { mason_ls } end
            -- System install fallback
            local sys = vim.fn.exepath("elixir-ls")
            if sys ~= "" then return { sys } end
            return { "elixir-ls" }
          end)(),
          settings = {
            elixirLS = {
              dialyzerEnabled  = true,
              fetchDeps        = false,
              enableTestLenses = true,
              suggestSpecs     = true,
            },
          },
        },
      }

      local DEFAULT_SERVERS = {
        "tailwindcss", "cssls", "jsonls", "yamlls",
        "clangd", "kotlin_language_server", "zls",
      }

      for server, config in pairs(servers) do
        lsp_setup(server, config)
      end
      for _, server in ipairs(DEFAULT_SERVERS) do
        lsp_setup(server, {})
      end

      -- ── Optional servers (binary-gated) ─────────────────────────────────────

      local optional = {
        {
          server = "vhdl_ls",
          binary = "vhdl_ls",      -- differs from server name
          config = { filetypes = { "vhdl", "vhd" } },
        },
        {
          server = "fortls",
          config = {
            filetypes = { "fortran" },
            settings  = {
              fortls = {
                notifyInit         = true,
                nthreads           = 4,
                hover_signature    = true,
                use_signature_help = true,
              },
            },
          },
        },
        {
          server = "sqls",
          config = {
            filetypes = { "sql", "mysql" },
            on_attach = function(client, _)
              client.server_capabilities.renameProvider = false
            end,
          },
        },
        {
          server = "cobol_ls",
          binary = "cobol-language-server",  -- differs from server name
          config = {
            filetypes = { "cobol" },
            settings  = { cobol = { dialects = { "gnucobol", "ibm" } } },
          },
        },
      }

      for _, entry in ipairs(optional) do
        local bin = entry.binary or entry.server
        if vim.fn.executable(bin) == 1 then
          lsp_setup(entry.server, entry.config)
        end
      end

      -- ── Diagnostics UI ──────────────────────────────────────────────────────
      local icons = require("core.util.icons")
      vim.diagnostic.config({
        virtual_text  = { prefix = "●", spacing = 4 },
        signs         = { text = {
          Error = icons.diagnostics.Error,
          Warn  = icons.diagnostics.Warn,
          Hint  = icons.diagnostics.Hint,
          Info  = icons.diagnostics.Info,
        }},
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

  -- ── Conform (formatters) ────────────────────────────────────────────────────

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
        -- rust  → owned by rust.lua  (rustfmt ships with rustup)
        -- zig   → owned by zig.lua   (zigfmt ships with zig)
      },
      format_on_save = function(bufnr)
        if vim.g.disable_autoformat then return nil end
        local ok_b, buf_disable = pcall(function()
          return vim.b[bufnr].disable_autoformat
        end)
        if ok_b and buf_disable then return nil end
        -- FIX B3: timeout respects vim.g.format_timeout_ms.
        return {
          timeout_ms = vim.g.format_timeout_ms or 500,
          lsp_format = "fallback",
        }
      end,
    },
  },

  -- ── nvim-lint ───────────────────────────────────────────────────────────────

  {
    "mfussenegger/nvim-lint",
    event  = "BufReadPost",
    config = function()
      local lint = require("lint")

      local function merge_linters(ft, linters)
        lint.linters_by_ft[ft] = lint.linters_by_ft[ft] or {}
        local existing_set = {}
        for _, l in ipairs(lint.linters_by_ft[ft]) do
          existing_set[l] = true
        end
        for _, l in ipairs(linters) do
          if not existing_set[l] then
            table.insert(lint.linters_by_ft[ft], l)
            existing_set[l] = true
          end
        end
      end

      merge_linters("python",     { "ruff" })
      merge_linters("javascript", { "eslint_d" })
      merge_linters("typescript", { "eslint_d" })
      merge_linters("ruby",       { "rubocop" })

      if vim.fn.executable("pylint") == 1 then
        merge_linters("python", { "pylint" })
      end

      -- eslint_d guard: it is a persistent daemon process; if the binary is
      -- absent nvim-lint logs a warning on every save and may leave orphaned
      -- processes.  Guard is identical to the shellcheck pattern.
      if vim.fn.executable("eslint_d") == 1 then
        merge_linters("javascript",      { "eslint_d" })
        merge_linters("typescript",      { "eslint_d" })
        merge_linters("javascriptreact", { "eslint_d" })
        merge_linters("typescriptreact", { "eslint_d" })
      end

      if vim.fn.executable("shellcheck") == 1 then
        merge_linters("sh",   { "shellcheck" })
        merge_linters("bash", { "shellcheck" })
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
