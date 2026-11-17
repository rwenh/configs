-- lua/plugins/specs/lsp.lua — language servers, LSP keymaps, diagnostics, formatting and linting
--
-- Servers are registered through core.util.lspreg, the single registration path. Servers another plugin
-- starts (jdtls, rustaceanvim, typescript-tools, elixir-tools) are configured in their own specs.

local flags = require("core.flags")
local gates = require("plugins.specs.lang.shared")
local icons = require("core.util.icons")
local lspreg = require("core.util.lspreg")
local packages = require("core.util.packages")

local function typescript_tools_present()
  if package.loaded["typescript-tools"] then
    return true
  end
  local ok, lazy_cfg = pcall(require, "lazy.core.config")
  if ok and lazy_cfg.plugins and lazy_cfg.plugins["typescript-tools.nvim"] then
    return true
  end
  return #vim.api.nvim_get_runtime_file("lua/typescript-tools/init.lua", false) > 0
end

return {
  { "williamboman/mason.nvim", cmd = "Mason", build = ":MasonUpdate", opts = { ui = { border = "rounded" } } },

  {
    "williamboman/mason-lspconfig.nvim",
    dependencies = "mason.nvim",
    opts = {
      ensure_installed = packages.lspconfig,
      automatic_enable = false, -- core.util.lspreg enables the servers
    },
  },

  {
    "aznhe21/actions-preview.nvim",
    lazy = true,
    opts = {
      telescope = {
        sorting_strategy = "ascending",
        layout_strategy = "vertical",
        layout_config = { width = 0.8, height = 0.9, prompt_position = "top" },
      },
    },
    config = function(_, opts)
      require("core.util.safe_setup").run("actions-preview", function()
        require("actions-preview").setup(opts)
      end, "run :Lazy update actions-preview.nvim")
    end,
  },

  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = { "mason-lspconfig.nvim", "saghen/blink.cmp", "aznhe21/actions-preview.nvim" },
    config = function()
      -- ── Buffer-local keymaps, set on attach ────────────────────────────────
      vim.api.nvim_create_autocmd("LspAttach", {
        group = vim.api.nvim_create_augroup("LspKeymaps", { clear = true }),
        callback = function(e)
          local function map(keys, fn, desc, mode)
            vim.keymap.set(mode or "n", keys, fn, { buffer = e.buf, desc = "LSP: " .. desc })
          end

          map("gd", vim.lsp.buf.definition, "Go to Definition")
          map("gD", vim.lsp.buf.declaration, "Go to Declaration")
          map("gi", vim.lsp.buf.implementation, "Go to Implementation")
          map("gr", vim.lsp.buf.references, "References")
          map("K", vim.lsp.buf.hover, "Hover Docs")
          map("<leader>k", vim.lsp.buf.signature_help, "Signature Help")

          local function code_action()
            if not pcall(function()
              require("actions-preview").code_actions()
            end) then
              vim.lsp.buf.code_action()
            end
          end
          map("<leader>,a", code_action, "Code Action")
          map("<leader>,a", code_action, "Code Action", "v")

          map("<leader>,r", function()
            if not pcall(vim.cmd, "IncRename " .. vim.fn.expand("<cword>")) then
              vim.lsp.buf.rename()
            end
          end, "Rename Symbol")

          local function fmt()
            local format = require("core.util.format")
            local mode = vim.fn.mode()
            if mode == "v" or mode == "V" or mode == "\22" then
              -- Format only the selected lines.
              local l1, l2 = vim.fn.line("v"), vim.fn.line(".")
              vim.api.nvim_feedkeys(vim.keycode("<Esc>"), "nx", false)
              format.run({ bufnr = e.buf, line1 = l1, line2 = l2 })
            else
              format.run({ bufnr = e.buf })
            end
          end
          map("<leader>,f", fmt, "Format")
          map("<leader>,f", fmt, "Format Range", "v")

          map("<leader>,d", vim.diagnostic.open_float, "Diagnostic Float")
          map("<leader>,l", vim.diagnostic.setloclist, "Diagnostic List")
          map("<leader>,t", function()
            local on = vim.diagnostic.is_enabled({ bufnr = e.buf })
            vim.diagnostic.enable(not on, { bufnr = e.buf })
            vim.notify("Diagnostics " .. (on and "disabled" or "enabled"))
          end, "Toggle Diagnostics")
          map("<leader>ty", vim.lsp.buf.type_definition, "Type Definition")
          map("]d", function()
            vim.diagnostic.jump({ count = 1, float = true })
          end, "Next Diagnostic")
          map("[d", function()
            vim.diagnostic.jump({ count = -1, float = true })
          end, "Prev Diagnostic")

          local client = vim.lsp.get_client_by_id(e.data.client_id)
          if client and client.server_capabilities.inlayHintProvider then
            map("<leader>,i", function()
              local on = vim.lsp.inlay_hint.is_enabled({ bufnr = e.buf })
              vim.lsp.inlay_hint.enable(not on, { bufnr = e.buf })
              vim.notify("Inlay hints " .. (on and "off" or "on"))
            end, "Toggle Inlay Hints")
          end
          if client and client.server_capabilities.documentSymbolProvider then
            local ok, navic = pcall(require, "nvim-navic")
            if ok then
              pcall(navic.attach, client, e.buf)
            end
          end
          map("<leader>,o", function()
            if not pcall(vim.cmd, "Trouble lsp_document_symbols toggle") then
              vim.lsp.buf.document_symbol()
            end
          end, "Code Outline")
        end,
      })

      -- ── Servers ────────────────────────────────────────────────────────────
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
          settings = { basedpyright = { analysis = { typeCheckingMode = "basic" } } },
        },
        gopls = {
          settings = { gopls = { analyses = { unusedparams = true }, staticcheck = true, gofumpt = true } },
        },
        solargraph = {
          settings = { solargraph = { diagnostics = true, completion = true } },
        },
        clangd = {
          cmd = {
            "clangd",
            "--background-index",
            "--clang-tidy",
            "--header-insertion=iwyu",
            "--completion-style=detailed",
          },
        },
        html = {
          filetypes = { "html", "htmldjango", "jinja.html" },
          init_options = { provideFormatter = false },
        },
        cssls = {},
        jsonls = {},
        yamlls = {},
        tailwindcss = {},
        kotlin_language_server = {},
        zls = {},
      }
      for server, config in pairs(servers) do
        lspreg.register(server, config)
      end

      -- Elixir: elixir-ls unless NextLS is requested.
      if flags.get("elixir_use_nextls", false) ~= true then
        local mason_ls = vim.fn.stdpath("data") .. "/mason/packages/elixir-ls/language_server.sh"
        local sys = vim.fn.exepath("elixir-ls")
        local bin = vim.fn.filereadable(mason_ls) == 1 and mason_ls or (sys ~= "" and sys or "elixir-ls")
        lspreg.register("elixirls", {
          cmd = { bin },
          settings = {
            elixirLS = {
              dialyzerEnabled = true,
              fetchDeps = false,
              enableTestLenses = true,
              suggestSpecs = true,
            },
          },
        })
      end

      -- Optional: registered only when the binary is on PATH, so there is no missing-server notice.
      for _, entry in ipairs({
        { server = "vhdl_ls", config = { filetypes = { "vhdl", "vhd" } } },
        {
          server = "fortls",
          config = {
            filetypes = { "fortran" },
            settings = {
              fortls = { notifyInit = true, nthreads = 4, hover_signature = true, use_signature_help = true },
            },
          },
        },
        {
          server = "sqls",
          config = {
            filetypes = { "sql", "mysql" },
            on_attach = function(client)
              client.server_capabilities.renameProvider = false
            end,
          },
        },
      }) do
        if vim.fn.executable(entry.server) == 1 then
          lspreg.register(entry.server, entry.config)
        end
      end

      -- TypeScript fallback when typescript-tools.nvim is not present.
      if not typescript_tools_present() then
        if vim.fn.executable("typescript-language-server") == 1 then
          lspreg.register("ts_ls", {})
        else
          vim.schedule(function()
            vim.notify(
              "[lsp] typescript-tools.nvim not found and typescript-language-server not on PATH.",
              vim.log.levels.WARN
            )
          end)
        end
      end

      -- ── Diagnostics UI ─────────────────────────────────────────────────────
      vim.diagnostic.config({
        virtual_text = { prefix = "●", spacing = 4 },
        signs = {
          text = {
            [vim.diagnostic.severity.ERROR] = icons.diagnostics.Error,
            [vim.diagnostic.severity.WARN] = icons.diagnostics.Warn,
            [vim.diagnostic.severity.HINT] = icons.diagnostics.Hint,
            [vim.diagnostic.severity.INFO] = icons.diagnostics.Info,
          },
        },
        underline = true,
        severity_sort = true,
        update_in_insert = false,
        float = { border = "rounded", source = true },
      })
    end,
  },

  { "j-hui/fidget.nvim", event = "LspAttach", opts = { notification = { window = { winblend = 0 } } } },

  -- ── Conform ────────────────────────────────────────────────────────────────
  {
    "stevearc/conform.nvim",
    event = "BufWritePre",
    opts = {
      formatters_by_ft = {
        lua = "stylua",
        python = { "black", "isort" },
        go = { "goimports", "gofumpt" },
        rust = "rustfmt",
        javascript = "prettier",
        javascriptreact = "prettier",
        typescript = "prettier",
        typescriptreact = "prettier",
        html = "prettier",
        css = "prettier",
        scss = "prettier",
        less = "prettier",
        json = "prettier",
        yaml = "prettier",
        markdown = "prettier",
        sh = "shfmt",
        ruby = "rubocop",
        kotlin = "ktlint",
        c = "clang-format",
        cpp = "clang-format",
        fortran = "fprettify",
        zig = "zigfmt",
        vhdl = "vsg",
        elixir = "mix",
      },
      format_on_save = function(bufnr)
        return require("core.util.format").on_save(bufnr)
      end,
    },
    config = function(_, opts)
      if vim.fn.executable("sqlfmt") == 1 then
        opts.formatters_by_ft.sql = { "sqlfmt" }
        opts.formatters_by_ft.mysql = { "sqlfmt" }
      end
      local ok, conform = pcall(require, "conform")
      if ok then
        pcall(conform.setup, opts)
      end
    end,
  },

  -- ── nvim-lint ──────────────────────────────────────────────────────────────
  {
    "mfussenegger/nvim-lint",
    event = "BufReadPost",
    config = function()
      local lint = require("lint")

      local function merge_linters(ft, names)
        lint.linters_by_ft[ft] = lint.linters_by_ft[ft] or {}
        local seen = {}
        for _, l in ipairs(lint.linters_by_ft[ft]) do
          seen[l] = true
        end
        for _, l in ipairs(names) do
          if not seen[l] then
            table.insert(lint.linters_by_ft[ft], l)
            seen[l] = true
          end
        end
      end

      -- Linters registered when their binary is present.
      if vim.fn.executable("ruff") == 1 then
        merge_linters("python", { "ruff" })
      end
      if vim.fn.executable("rubocop") == 1 then
        merge_linters("ruby", { "rubocop" })
      end
      if flags.get("enable_pylint", false) == true and vim.fn.executable("pylint") == 1 then
        merge_linters("python", { "pylint" })
      end
      if vim.fn.executable("eslint_d") == 1 then
        for _, ft in ipairs({ "javascript", "typescript", "javascriptreact", "typescriptreact" }) do
          merge_linters(ft, { "eslint_d" })
        end
      end
      if vim.fn.executable("shellcheck") == 1 then
        merge_linters("sh", { "shellcheck" })
        merge_linters("bash", { "shellcheck" })
      end
      -- clang-tidy is registered under "clangtidy", the name nvim-lint runs.
      if vim.fn.executable("clang-tidy") == 1 then
        merge_linters("c", { "clangtidy" })
      end

      -- Project-gated linters (stylelint, htmlhint) are applied here, once nvim-lint is loaded.
      gates.apply_gates()

      local reported = {}
      vim.api.nvim_create_autocmd({ "BufWritePost", "InsertLeave" }, {
        group = vim.api.nvim_create_augroup("NvimLint", { clear = true }),
        callback = function()
          local ok, err = pcall(lint.try_lint)
          if not ok and not reported[tostring(err)] then
            reported[tostring(err)] = true
            vim.notify("[lint] " .. tostring(err), vim.log.levels.WARN)
          end
        end,
      })
    end,
  },
}
