-- lua/plugins/specs/lsp.lua — v2.5.0
--

return {
  { "williamboman/mason.nvim", cmd = "Mason", build = ":MasonUpdate", opts = { ui = { border = "rounded" } } },
  {
    "williamboman/mason-lspconfig.nvim",
    dependencies = "mason.nvim",
    opts = {
      ensure_installed = require("core.util.packages").lspconfig,
      automatic_enable = false,
      handlers = { function() end }, -- v1 compat: never auto-setup
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
      pcall(function()
        require("actions-preview").setup(opts)
      end)
    end,
  },

  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = { "mason-lspconfig.nvim", "saghen/blink.cmp", "aznhe21/actions-preview.nvim" },
    config = function()
      local nvim_011 = vim.fn.has("nvim-0.11") == 1
      local packages = require("core.util.packages")
      local projconf = require("core.util.projconf")

      -- ── Capabilities (memoised) ────────────────────────────────────────────
      local _caps = nil
      local function get_capabilities()
        if _caps then
          return _caps
        end
        local ok, blink = pcall(require, "blink.cmp")
        if ok then
          local ok2, caps = pcall(blink.get_lsp_capabilities)
          if ok2 and type(caps) == "table" and caps.textDocument ~= nil then
            _caps = caps
            return _caps
          end
        end
        _caps = vim.lsp.protocol.make_client_capabilities()
        return _caps
      end

      -- ── Per-server on_attach override table ───────────────────────────────
      local function run_user_on_attach(server_name, client, bufnr)
        local overrides = type(vim.g.lsp_on_attach_overrides) == "table" and vim.g.lsp_on_attach_overrides or {}
        local fn = overrides[server_name]
        if type(fn) == "function" then
          pcall(fn, client, bufnr)
        end
      end

      -- ── Per-project .lspconfig.lua (core.util.projconf) ─────────────────────
      local function load_project_lsp_config()
        local ok_path, path_util = pcall(require, "core.util.path")
        local root = (ok_path and path_util.find_root()) or vim.fn.getcwd()
        local res = projconf.load(root, ".lspconfig.lua")
        if res.ok then
          return res.value
        end
        if not res.absent then
          vim.notify("[lsp] .lspconfig.lua not used: " .. res.reason, vim.log.levels.WARN)
        end
        return {}
      end
      -- Read once, when the servers are configured. A later change of directory does not re-apply it.
      local project_overrides = load_project_lsp_config()

      -- ── lsp_setup ─────────────────────────────────────────────────────────
      -- Each server's binary comes from core.util.packages. A server whose binary is absent is skipped
      -- and reported once, instead of failing on every attach. A config-supplied cmd takes precedence.
      local missing_servers = {}

      -- nvim-lspconfig resolves html/css/json/yaml/tailwind from node_modules/.bin under the
      -- project root before the global name. Accept the cwd's node_modules too, so a locally
      -- installed server is not skipped. (Limitation: checked once per session, from the cwd
      -- at first attach, not per buffer root.)
      local function available(bin)
        return vim.fn.executable(bin) == 1 or vim.fn.executable(vim.fn.getcwd() .. "/node_modules/.bin/" .. bin) == 1
      end

      local function lsp_setup(server, config)
        config = config or {}
        local bin = (config.cmd and config.cmd[1]) or packages.bin(server)
        if bin and not available(bin) then
          table.insert(missing_servers, server .. " (" .. bin .. ")")
          return
        end
        if project_overrides[server] then
          config = vim.tbl_deep_extend("force", config, project_overrides[server])
        end
        config.capabilities = vim.tbl_deep_extend("force", get_capabilities(), config.capabilities or {})
        local user_on_attach = config.on_attach
        config.on_attach = function(client, bufnr)
          if type(user_on_attach) == "function" then
            pcall(user_on_attach, client, bufnr)
          end
          run_user_on_attach(server, client, bufnr)
        end
                local function report_setup(err)
          vim.notify(("[lsp] %s failed to configure: %s"):format(server, tostring(err)), vim.log.levels.ERROR)
        end
        if nvim_011 then
          local ok, err = pcall(function()
            vim.lsp.config(server, config)
            vim.lsp.enable(server)
          end)
          if not ok then
            report_setup(err)
          end
        else
          local ok, lspconfig = pcall(require, "lspconfig")
          if not ok then
            report_setup("lspconfig is not loadable")
            return
          end
          local ok2, err = pcall(function()
            lspconfig[server].setup(config)
          end)
          if not ok2 then
            report_setup(err)
          end
        end
      end

      -- ── diag_jump ─────────────────────────────────────────────────────────
      local function diag_jump(count)
        if nvim_011 then
          pcall(function()
            vim.diagnostic.jump({ count = count, float = true })
          end)
        else
          local fn = count > 0 and vim.diagnostic.goto_next or vim.diagnostic.goto_prev
          for _ = 1, math.abs(count) do
            pcall(fn, { float = true })
          end
        end
      end

      local function cmd_or_fallback(cmd_str, fallback_fn)
        if not pcall(vim.cmd, cmd_str) then
          pcall(fallback_fn)
        end
      end

      -- ── LspAttach keymaps ─────────────────────────────────────────────────
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
            local ok = pcall(function()
              require("actions-preview").code_actions()
            end)
            if not ok then
              vim.lsp.buf.code_action()
            end
          end
          map("<leader>,a", code_action, "Code Action")
          map("<leader>,a", code_action, "Code Action", "v")
          map("<leader>,r", function()
            cmd_or_fallback("IncRename " .. vim.fn.expand("<cword>"), vim.lsp.buf.rename)
          end, "Rename Symbol")

          local function fmt()
            local format = require("core.util.format")
            local mode = vim.fn.mode()
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
          map("]d", function()
            diag_jump(1)
          end, "Next Diagnostic")
          map("[d", function()
            diag_jump(-1)
          end, "Prev Diagnostic")

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
            if ok then
              pcall(function()
                navic.attach(client, e.buf)
              end)
            end
          end
          map("<leader>,o", function()
            cmd_or_fallback("Trouble lsp_document_symbols toggle", vim.lsp.buf.document_symbol)
          end, "Code Outline")
        end,
      })

      -- ── Servers ───────────────────────────────────────────────────────────────

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
      }

      for server, config in pairs(servers) do
        lsp_setup(server, config)
      end

      -- ── Elixir: only start elixir-ls when NextLS is NOT wanted ────────────
      if not vim.g.elixir_use_nextls then
        local elixirls_cmd = (function()
          local mason_ls = vim.fn.stdpath("data") .. "/mason/packages/elixir-ls/language_server.sh"
          if vim.fn.filereadable(mason_ls) == 1 then
            return { mason_ls }
          end
          local sys = vim.fn.exepath("elixir-ls")
          return sys ~= "" and { sys } or { "elixir-ls" }
        end)()
        lsp_setup("elixirls", {
          cmd = elixirls_cmd,
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

      for _, s in ipairs({ "tailwindcss", "cssls", "jsonls", "yamlls", "kotlin_language_server", "zls" }) do
        lsp_setup(s, {})
      end

      lsp_setup("clangd", {
        cmd = {
          "clangd",
          "--background-index",
          "--clang-tidy",
          "--header-insertion=iwyu",
          "--completion-style=detailed",
        },
      })

      lsp_setup("html", {
        filetypes = { "html", "htmldjango", "jinja.html" },
        init_options = { provideFormatter = false },
      })

      -- ── TypeScript fallback ────────────────────────────────────────────────
      do
        local ts_tools_present = (function()
          if package.loaded["typescript-tools"] then
            return true
          end
          local ok, lazy_cfg = pcall(require, "lazy.core.config")
          if ok and lazy_cfg.plugins and lazy_cfg.plugins["typescript-tools.nvim"] then
            return true
          end
          return #vim.api.nvim_get_runtime_file("lua/typescript-tools/init.lua", false) > 0
        end)()
        if not ts_tools_present then
          if vim.fn.executable("typescript-language-server") == 1 then
            lsp_setup("ts_ls", {})
          else
            vim.schedule(function()
              vim.notify(
                "[lsp] typescript-tools.nvim not found and typescript-language-server not on PATH.",
                vim.log.levels.WARN
              )
            end)
          end
        end
      end

      -- ── Optional servers ───────────────────────────────────────────────────
      for _, entry in ipairs({
        { server = "vhdl_ls", binary = "vhdl_ls", config = { filetypes = { "vhdl", "vhd" } } },
        {
          server = "fortls",
          binary = "fortls",
          config = {
            filetypes = { "fortran" },
            settings = {
              fortls = { notifyInit = true, nthreads = 4, hover_signature = true, use_signature_help = true },
            },
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
      }) do
        if vim.fn.executable(entry.binary or entry.server) == 1 then
          lsp_setup(entry.server, entry.config)
        end
      end

      if #missing_servers > 0 then
        vim.notify(
          "[lsp] "
            .. #missing_servers
            .. " language server(s) not on PATH, skipped: "
            .. table.concat(missing_servers, ", ")
            .. "\nInstall them with :MasonInstallAll (or :Mason). Their filetypes run without LSP until then.",
          vim.log.levels.INFO
        )
      end

      -- ── Diagnostics UI ────────────────────────────────────────────────────
      local icons = require("core.util.icons")
      vim.diagnostic.config({
        virtual_text = { prefix = "●", spacing = 4 },
        -- keys must be severity constants; string keys like `Error` are ignored (no signs)
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

  -- ── Conform ───────────────────────────────────────────────────────────────
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

  -- ── nvim-lint ─────────────────────────────────────────────────────────────
  {
    "mfussenegger/nvim-lint",
    event = "BufReadPost",
    config = function()
      local lint = require("lint")
      local function merge_linters(ft, linters)
        lint.linters_by_ft[ft] = lint.linters_by_ft[ft] or {}
        local seen = {}
        for _, l in ipairs(lint.linters_by_ft[ft]) do
          seen[l] = true
        end
        for _, l in ipairs(linters) do
          if not seen[l] then
            table.insert(lint.linters_by_ft[ft], l)
            seen[l] = true
          end
        end
      end
      if vim.fn.executable("ruff") == 1 then
        merge_linters("python", { "ruff" })
      end
      if vim.fn.executable("rubocop") == 1 then
        merge_linters("ruby", { "rubocop" })
      end

      if vim.g.enable_pylint == true and vim.fn.executable("pylint") == 1 then
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
      -- html's htmlhint is owned by lang/html.lua (gated on its config file). The clang-tidy linter is
      -- registered under "clangtidy": nvim-lint's key has no hyphen, and the hyphenated name never ran.
      if vim.fn.executable("clang-tidy") == 1 then
        merge_linters("c", { "clangtidy" })
      end
      local _lint_reported = {}
      vim.api.nvim_create_autocmd({ "BufWritePost", "InsertLeave" }, {
        group = vim.api.nvim_create_augroup("NvimLint", { clear = true }),
        callback = function()
          local ok, err = pcall(lint.try_lint)
          if not ok and not _lint_reported[tostring(err)] then
            _lint_reported[tostring(err)] = true
            vim.notify("[lint] " .. tostring(err), vim.log.levels.WARN)
          end
        end,
      })
    end,
  },
}
