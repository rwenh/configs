-- lua/plugins/specs/lsp.lua — v2.5.0
--

return {
  { "williamboman/mason.nvim", cmd = "Mason", build = ":MasonUpdate", opts = { ui = { border = "rounded" } } },
  { "williamboman/mason-lspconfig.nvim", dependencies = "mason.nvim",
    opts = {
      ensure_installed = require("core.util.packages").lspconfig,
      automatic_enable = false,
      handlers = { function() end },   -- v1 compat: never auto-setup
    } },

  { "aznhe21/actions-preview.nvim", lazy = true,
    opts = { telescope = { sorting_strategy = "ascending", layout_strategy = "vertical",
      layout_config = { width = 0.8, height = 0.9, prompt_position = "top" } } },
    config = function(_, opts) pcall(function() require("actions-preview").setup(opts) end) end },

  {
    "neovim/nvim-lspconfig",
    event        = { "BufReadPre", "BufNewFile" },
    dependencies = { "mason-lspconfig.nvim", "saghen/blink.cmp", "aznhe21/actions-preview.nvim" },
    config = function()
      local nvim_011 = vim.fn.has("nvim-0.11") == 1

      -- ── Capabilities (memoised) ────────────────────────────────────────────
      local _caps = nil
      local function get_capabilities()
        if _caps then return _caps end
        local ok, blink = pcall(require, "blink.cmp")
        if ok then
          local ok2, caps = pcall(blink.get_lsp_capabilities)
          if ok2 and type(caps) == "table" and caps.textDocument ~= nil then _caps = caps; return _caps end
        end
        _caps = vim.lsp.protocol.make_client_capabilities()
        return _caps
      end

      -- ── Per-server on_attach override table ───────────────────────────────
      local function run_user_on_attach(server_name, client, bufnr)
        local overrides = type(vim.g.lsp_on_attach_overrides) == "table"
          and vim.g.lsp_on_attach_overrides or {}
        local fn = overrides[server_name]
        if type(fn) == "function" then pcall(fn, client, bufnr) end
      end

      -- ── Per-project .lspconfig.lua loader ─────────────────────────────────
      local _project_lsp_cache = {}

      local function load_project_lsp_config()
        local ok_path, path_util = pcall(require, "core.util.path")
        local root = (ok_path and path_util.find_root()) or vim.fn.getcwd()
        if not root or root == "" then return {} end
        if _project_lsp_cache[root] then return _project_lsp_cache[root] end

        local cfg_file = root .. "/.lspconfig.lua"
        if vim.fn.filereadable(cfg_file) ~= 1 then
          _project_lsp_cache[root] = {}; return {}
        end

        local ok, result = require("core.util.secure").load(cfg_file)   -- trust-gated, not dofile
        if not ok or type(result) ~= "table" then
          vim.notify("[lsp] .lspconfig.lua error: " .. tostring(result), vim.log.levels.WARN)
          _project_lsp_cache[root] = {}; return {}
        end

        vim.notify("[lsp] loaded project overrides from .lspconfig.lua", vim.log.levels.INFO)
        _project_lsp_cache[root] = result
        return result
      end

      vim.api.nvim_create_autocmd({ "DirChanged" }, {
        group    = vim.api.nvim_create_augroup("LspProjectConfigCache", { clear = true }),
        callback = function() _project_lsp_cache = {} end,
      })

      -- ── lsp_setup ─────────────────────────────────────────────────────────
      local function lsp_setup(server, config)
        config = config or {}
        local project_overrides = load_project_lsp_config()
        if project_overrides[server] then
          config = vim.tbl_deep_extend("force", config, project_overrides[server])
        end
        config.capabilities = vim.tbl_deep_extend("force", get_capabilities(), config.capabilities or {})
        local user_on_attach = config.on_attach
        config.on_attach = function(client, bufnr)
          if type(user_on_attach) == "function" then pcall(user_on_attach, client, bufnr) end
          run_user_on_attach(server, client, bufnr)
        end
        if nvim_011 then
          pcall(function() vim.lsp.config(server, config); vim.lsp.enable(server) end)
        else
          local ok, lspconfig = pcall(require, "lspconfig")
          if ok then pcall(function() lspconfig[server].setup(config) end) end
        end
      end

      -- ── diag_jump ─────────────────────────────────────────────────────────
      local function diag_jump(count)
        if nvim_011 then pcall(function() vim.diagnostic.jump({ count = count, float = true }) end)
        else
          local fn = count > 0 and vim.diagnostic.goto_next or vim.diagnostic.goto_prev
          for _ = 1, math.abs(count) do pcall(fn, { float = true }) end
        end
      end

      local function cmd_or_fallback(cmd_str, fallback_fn)
        if not pcall(vim.cmd, cmd_str) then pcall(fallback_fn) end
      end

      -- ── LspAttach keymaps ─────────────────────────────────────────────────
      vim.api.nvim_create_autocmd("LspAttach", {
        group    = vim.api.nvim_create_augroup("LspKeymaps", { clear = true }),
        callback = function(e)
          local function map(keys, fn, desc, mode)
            vim.keymap.set(mode or "n", keys, fn, { buffer = e.buf, desc = "LSP: " .. desc })
          end
          map("gd", vim.lsp.buf.definition,     "Go to Definition")
          map("gD", vim.lsp.buf.declaration,    "Go to Declaration")
          map("gi", vim.lsp.buf.implementation, "Go to Implementation")
          map("gr", vim.lsp.buf.references,     "References")
          map("K",  vim.lsp.buf.hover,          "Hover Docs")
          map("<leader>k", vim.lsp.buf.signature_help, "Signature Help")

          local function code_action()
            local ok = pcall(function()
              require("actions-preview").code_actions()
            end)
            if not ok then vim.lsp.buf.code_action() end
          end
          map("<leader>,a", code_action, "Code Action")
          map("<leader>,a", code_action, "Code Action", "v")
          map("<leader>,r", function()
            cmd_or_fallback("IncRename " .. vim.fn.expand("<cword>"), vim.lsp.buf.rename)
          end, "Rename Symbol")

          local function fmt()
            local format = require("core.util.format")
            local mode   = vim.fn.mode()
            if mode == "v" or mode == "V" or mode == "\22" then
              -- The visual-mode mapping used to format the WHOLE buffer (range ignored).
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
            cmd_or_fallback("Trouble lsp_document_symbols toggle", vim.lsp.buf.document_symbol)
          end, "Code Outline")
        end,
      })

-- ── Servers ───────────────────────────────────────────────────────────────

      local servers = {
        lua_ls = {
          settings = { Lua = { diagnostics = { globals = { "vim" } }, workspace = { checkThirdParty = false }, telemetry = { enable = false } } },
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
      }

      for server, config in pairs(servers) do lsp_setup(server, config) end

      -- ── Elixir: only start elixir-ls when NextLS is NOT wanted ────────────
      if not vim.g.elixir_use_nextls then
        -- vim.lsp.config() needs `cmd` as a list (a function must return an RPC client),
        -- so resolve the binary eagerly.
        local elixirls_cmd = (function()
          local mason_ls = vim.fn.stdpath("data") .. "/mason/packages/elixir-ls/language_server.sh"
          if vim.fn.filereadable(mason_ls) == 1 then return { mason_ls } end
          local sys = vim.fn.exepath("elixir-ls")
          return sys ~= "" and { sys } or { "elixir-ls" }
        end)()
        lsp_setup("elixirls", {
          cmd = elixirls_cmd,
          settings = { elixirLS = {
            dialyzerEnabled  = true,
            fetchDeps        = false,
            enableTestLenses = true,
            suggestSpecs     = true,
          }},
        })
      end

      for _, s in ipairs({ "tailwindcss","cssls","jsonls","yamlls","clangd","kotlin_language_server","zls" }) do
        lsp_setup(s, {})
      end

      -- html (moved here from lang/html.lua, whose spec `init` was overwritten by other
      -- lang specs, so the server was never enabled).
      lsp_setup("html", {
        filetypes    = { "html", "htmldjango", "jinja.html" },
        init_options = { provideFormatter = false },
      })

      -- ── TypeScript fallback ────────────────────────────────────────────────
      do
        local ts_tools_present = (function()
          if package.loaded["typescript-tools"] then return true end
          -- typescript-tools is lazy (ft = ...), so it is NOT on the runtimepath yet; probing
          -- the rtp always failed and warned on every startup. Ask lazy.nvim instead.
          local ok, lazy_cfg = pcall(require, "lazy.core.config")
          if ok and lazy_cfg.plugins and lazy_cfg.plugins["typescript-tools.nvim"] then return true end
          return #vim.api.nvim_get_runtime_file("lua/typescript-tools/init.lua", false) > 0
        end)()
        if not ts_tools_present then
          if vim.fn.executable("typescript-language-server") == 1 then lsp_setup("ts_ls", {})
          else
            vim.schedule(function()
              vim.notify("[lsp] typescript-tools.nvim not found and typescript-language-server not on PATH.", vim.log.levels.WARN)
            end)
          end
        end
      end

      -- ── Optional servers ───────────────────────────────────────────────────
      for _, entry in ipairs({
        { server = "vhdl_ls",   binary = "vhdl_ls",              config = { filetypes = { "vhdl","vhd" } } },
        { server = "fortls",    binary = "fortls",                config = { filetypes = { "fortran" }, settings = { fortls = { notifyInit = true, nthreads = 4, hover_signature = true, use_signature_help = true } } } },
        { server = "sqls",      binary = "sqls",                  config = { filetypes = { "sql","mysql" }, on_attach = function(client, _) client.server_capabilities.renameProvider = false end } },
        { server = "cobol_ls",  binary = "cobol-language-server", config = { filetypes = { "cobol" }, settings = { cobol = { dialects = { "gnucobol","ibm" } } } } },
      }) do
        if vim.fn.executable(entry.binary or entry.server) == 1 then lsp_setup(entry.server, entry.config) end
      end

      -- ── Diagnostics UI ────────────────────────────────────────────────────
      local icons = require("core.util.icons")
      vim.diagnostic.config({
        virtual_text  = { prefix = "●", spacing = 4 },
        -- keys must be severity constants; string keys like `Error` are ignored (no signs)
        signs         = { text = {
          [vim.diagnostic.severity.ERROR] = icons.diagnostics.Error,
          [vim.diagnostic.severity.WARN]  = icons.diagnostics.Warn,
          [vim.diagnostic.severity.HINT]  = icons.diagnostics.Hint,
          [vim.diagnostic.severity.INFO]  = icons.diagnostics.Info,
        } },
        underline = true, severity_sort = true, update_in_insert = false,
        float     = { border = "rounded", source = true },
      })
    end,
  },

  { "j-hui/fidget.nvim", event = "LspAttach", opts = { notification = { window = { winblend = 0 } } } },

  -- ── Conform ───────────────────────────────────────────────────────────────
  {
    "stevearc/conform.nvim",
    event = "BufWritePre",
    opts  = {
      formatters_by_ft = {
        lua="stylua", python={"black","isort"}, go={"goimports","gofumpt"}, rust="rustfmt",
        javascript="prettier", javascriptreact="prettier", typescript="prettier", typescriptreact="prettier",
        html="prettier", css="prettier", scss="prettier", less="prettier",
        json="prettier", yaml="prettier", markdown="prettier",
        sh="shfmt", ruby="rubocop", kotlin="ktlint", c="clang-format", cpp="clang-format",
        fortran="fprettify", zig="zigfmt", vhdl="vsg", elixir="mix",
      },
      format_on_save = function(bufnr) return require("core.util.format").on_save(bufnr) end,
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

  -- ── nvim-lint ─────────────────────────────────────────────────────────────
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
      merge_linters("ruby",       { "rubocop" })

      if vim.g.enable_pylint == true and vim.fn.executable("pylint") == 1 then
        merge_linters("python", { "pylint" })
      end

      if vim.fn.executable("eslint_d") == 1 then
        for _, ft in ipairs({ "javascript","typescript","javascriptreact","typescriptreact" }) do
          merge_linters(ft, { "eslint_d" })
        end
      end
      if vim.fn.executable("shellcheck") == 1 then
        merge_linters("sh",   { "shellcheck" })
        merge_linters("bash", { "shellcheck" })
      end
      if vim.fn.executable("htmlhint")   == 1 then merge_linters("html", { "htmlhint" })   end
      if vim.fn.executable("clang-tidy") == 1 then merge_linters("c",    { "clang-tidy" }) end
      vim.api.nvim_create_autocmd({ "BufWritePost","InsertLeave" }, {
        group    = vim.api.nvim_create_augroup("NvimLint", { clear = true }),
        callback = function() pcall(function() lint.try_lint() end) end,
      })
    end,
  },
}
