-- lua/plugins/specs/lsp.lua — v2.4.1
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
      ensure_installed       = require("core.util.packages").lspconfig,
      automatic_installation = true,
      handlers               = { function() end },
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

      -- ── Capabilities (memoised) ──────────────────────────────────────────────
      local _caps = nil
      local function get_capabilities()
        if _caps then return _caps end
        local ok, blink = pcall(require, "blink.cmp")
        if ok then
          local ok2, caps = pcall(blink.get_lsp_capabilities)
          if ok2 and type(caps) == "table" and caps.textDocument ~= nil then
            _caps = caps; return _caps
          end
        end
        _caps = vim.lsp.protocol.make_client_capabilities()
        return _caps
      end

      -- ── lsp_setup ────────────────────────────────────────────────────────────
      local function lsp_setup(server, config)
        config = config or {}
        config.capabilities = vim.tbl_deep_extend(
          "force", get_capabilities(), config.capabilities or {}
        )
        if nvim_011 then
          pcall(function() vim.lsp.config(server, config); vim.lsp.enable(server) end)
        else
          local ok, lspconfig = pcall(require, "lspconfig")
          if ok then pcall(function() lspconfig[server].setup(config) end) end
        end
      end

      -- ── diag_jump ────────────────────────────────────────────────────────────
      local function diag_jump(count)
        if nvim_011 then
          pcall(function() vim.diagnostic.jump({ count = count, float = true }) end)
        else
          local fn = count > 0 and vim.diagnostic.goto_next or vim.diagnostic.goto_prev
          for _ = 1, math.abs(count) do pcall(fn, { float = true }) end
        end
      end

      -- ── cmd_or_fallback ──────────────────────────────────────────────────────
      local function cmd_or_fallback(cmd_str, fallback_fn)
        if not pcall(vim.cmd, cmd_str) then pcall(fallback_fn) end
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

          local function code_action()
            cmd_or_fallback("ActionsPreview", vim.lsp.buf.code_action)
          end
          map("<leader>,a", code_action, "Code Action")
          map("<leader>,a", code_action, "Code Action", "v")

          map("<leader>,r", function()
            cmd_or_fallback("IncRename " .. vim.fn.expand("<cword>"), vim.lsp.buf.rename)
          end, "Rename Symbol")

          local function fmt()
            pcall(function()
              require("conform").format({
                bufnr      = e.buf,
                timeout_ms = vim.g.format_timeout_ms or 1500,
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
            local ok, navic = pcall(require, "nvim-navic")
            if ok then pcall(function() navic.attach(client, e.buf) end) end
          end

          map("<leader>,o", function()
            cmd_or_fallback("Trouble lsp_document_symbols toggle",
              vim.lsp.buf.document_symbol)
          end, "Code Outline (Trouble symbols)")
        end,
      })

      -- ── Servers ──────────────────────────────────────────────────────────────
      local servers = {
        lua_ls = { settings = { Lua = {
          diagnostics = { globals = { "vim" } },
          workspace   = { checkThirdParty = false },
          telemetry   = { enable = false },
        }}},
        basedpyright = {
          settings = { basedpyright = { analysis = { typeCheckingMode = "basic" } } },
        },
        gopls = { settings = { gopls = {
          analyses    = { unusedparams = true },
          staticcheck = true, gofumpt = true,
        }}},
        solargraph = {
          settings = { solargraph = { diagnostics = true, completion = true } },
        },
        elixirls = {
          cmd = (function()
            local data     = vim.fn.stdpath("data")
            local mason_ls = data .. "/mason/packages/elixir-ls/language_server.sh"
            if vim.fn.filereadable(mason_ls) == 1 then return { mason_ls } end
            local sys = vim.fn.exepath("elixir-ls")
            return sys ~= "" and { sys } or { "elixir-ls" }
          end)(),
          settings = { elixirLS = {
            dialyzerEnabled  = true, fetchDeps        = false,
            enableTestLenses = true, suggestSpecs     = true,
          }},
        },
      }

      for server, config in pairs(servers) do lsp_setup(server, config) end

      for _, s in ipairs({
        "tailwindcss", "cssls", "jsonls", "yamlls",
        "clangd", "kotlin_language_server", "zls",
      }) do lsp_setup(s, {}) end

      -- ── TypeScript fallback ──────────────────────────────────────────────────
      do
        local ts_tools_present = (function()
          if package.loaded["typescript-tools"] then return true end
          local hits = vim.api.nvim_get_runtime_file(
            "lua/typescript-tools/init.lua", false)
          return #hits > 0
        end)()

        if not ts_tools_present then
          if vim.fn.executable("typescript-language-server") == 1 then
            lsp_setup("ts_ls", {})
          else
            vim.schedule(function()
              vim.notify(
                "[lsp] typescript-tools.nvim not found and typescript-language-server"
                .. " not on PATH.\nTypeScript LSP unavailable.\n"
                .. "Fix: :Lazy install  OR  npm i -g typescript-language-server",
                vim.log.levels.WARN
              )
            end)
          end
        end
      end

      -- ── Optional servers (binary-gated) ─────────────────────────────────────
      for _, entry in ipairs({
        {
          server = "vhdl_ls",
          binary = "vhdl_ls",
          config = { filetypes = { "vhdl", "vhd" } },
        },
        {
          server = "fortls",
          binary = "fortls",
          config = {
            filetypes = { "fortran" },
            settings  = { fortls = {
              notifyInit        = true,
              nthreads          = 4,
              hover_signature   = true,
              use_signature_help = true,
            }},
          },
        },
        {
          server = "sqls",
          binary = "sqls",
          config = {
            filetypes = { "sql", "mysql" },
            on_attach = function(client, _)
              client.server_capabilities.renameProvider = false
            end,
          },
        },
        {
          server = "cobol_ls",
          binary = "cobol-language-server",
          config = {
            filetypes = { "cobol" },
            settings  = { cobol = { dialects = { "gnucobol", "ibm" } } },
          },
        },
      }) do
        if vim.fn.executable(entry.binary or entry.server) == 1 then
          lsp_setup(entry.server, entry.config)
        end
      end

      -- ── Diagnostics UI ──────────────────────────────────────────────────────
      local icons = require("core.util.icons")
      vim.diagnostic.config({
        virtual_text  = { prefix = "●", spacing = 4 },
        signs         = { text = {
          Error = icons.diagnostics.Error, Warn  = icons.diagnostics.Warn,
          Hint  = icons.diagnostics.Hint,  Info  = icons.diagnostics.Info,
        }},
        underline = true, severity_sort = true, update_in_insert = false,
        float     = { border = "rounded", source = "always" },
      })
    end,
  },

  { "j-hui/fidget.nvim", event = "LspAttach",
    opts = { notification = { window = { winblend = 0 } } } },

  -- ── Conform — single source for all formatters_by_ft ────────────────────────
  {
    "stevearc/conform.nvim",
    event = "BufWritePre",
    opts  = {
      formatters_by_ft = {
        lua             = { "stylua" },
        python          = { "black", "isort" },
        go              = { "goimports", "gofumpt" },
        rust            = { "rustfmt" },
        javascript      = { "prettier" },
        javascriptreact = { "prettier" },
        typescript      = { "prettier" },
        typescriptreact = { "prettier" },
        html            = { "prettier" },
        css             = { "prettier" },
        scss            = { "prettier" },
        less            = { "prettier" },
        json            = { "prettier" },
        yaml            = { "prettier" },
        markdown        = { "prettier" },
        sh              = { "shfmt" },
        ruby            = { "rubocop" },
        kotlin          = { "ktlint" },
        c               = { "clang-format" },
        cpp             = { "clang-format" },
        fortran         = { "fprettify" },
        zig             = { "zigfmt" },
        vhdl            = { "vsg" },
        elixir          = { "mix" },
      },
      format_on_save = function(bufnr)
        if vim.g.disable_autoformat then return nil end
        local ok, v = pcall(function() return vim.b[bufnr].disable_autoformat end)
        if ok and v then return nil end
        return {
          timeout_ms = vim.g.format_timeout_ms or 1500,
          lsp_format = "fallback",
        }
      end,
    },
    config = function(_, opts)
      if vim.fn.executable("sqlfmt") == 1 then
        opts.formatters_by_ft.sql   = { "sqlfmt" }
        opts.formatters_by_ft.mysql = { "sqlfmt" }
      end
      local ok, conform = pcall(require, "conform")
      if ok then pcall(conform.setup, opts) end
    end,
  },

  -- ── nvim-lint — single source for all linters ───────────────────────────────
  {
    "mfussenegger/nvim-lint",
    event  = "BufReadPost",
    config = function()
      local lint = require("lint")

      local function merge_linters(ft, linters)
        lint.linters_by_ft[ft] = lint.linters_by_ft[ft] or {}
        local seen = {}
        for _, l in ipairs(lint.linters_by_ft[ft]) do seen[l] = true end
        for _, l in ipairs(linters) do
          if not seen[l] then table.insert(lint.linters_by_ft[ft], l); seen[l] = true end
        end
      end

      merge_linters("python",     { "ruff" })
      merge_linters("javascript", { "eslint_d" })
      merge_linters("typescript", { "eslint_d" })
      merge_linters("ruby",       { "rubocop" })

      if vim.fn.executable("pylint") == 1 then
        merge_linters("python", { "pylint" })
      end
      if vim.fn.executable("eslint_d") == 1 then
        for _, ft in ipairs({
          "javascript", "typescript", "javascriptreact", "typescriptreact"
        }) do merge_linters(ft, { "eslint_d" }) end
      end
      if vim.fn.executable("shellcheck") == 1 then
        merge_linters("sh", { "shellcheck" }); merge_linters("bash", { "shellcheck" })
      end
      if vim.fn.executable("stylelint")  == 1 then
        merge_linters("css", { "stylelint" }); merge_linters("scss", { "stylelint" })
      end
      if vim.fn.executable("htmlhint")   == 1 then merge_linters("html", { "htmlhint"   }) end
      if vim.fn.executable("clang-tidy") == 1 then merge_linters("c",    { "clang-tidy" }) end

      vim.api.nvim_create_autocmd({ "BufWritePost", "InsertLeave" }, {
        group    = vim.api.nvim_create_augroup("NvimLint", { clear = true }),
        callback = function() pcall(function() lint.try_lint() end) end,
      })
    end,
  },
}
