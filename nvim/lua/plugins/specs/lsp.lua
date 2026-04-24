-- lua/plugins/specs/lsp.lua
--
-- OPT (v2.3.14):
--   • merge_linters(ft, linters) helper extracted. The manual
--     deduplication loop that existed in nvim-lint's config() is replaced
--     by a single reusable call, making the merge intent explicit and the
--     loop itself disappear from the reader's view.
--   • Optional server table call shape unified. Each entry now passes its
--     config directly to lsp_setup() the same way the primary `servers`
--     table does — no structural difference between the two paths.
--
-- FIX (v2.3.15):
--   • shellcheck was unconditionally registered for "sh" (line outside the
--     executable guard) AND then registered again inside the guard — making
--     the guard's "sh" entry dead code and, more importantly, causing
--     nvim-lint to attempt invoking a missing shellcheck binary on every
--     sh-file write. The unconditional call is removed; both "bash" and "sh"
--     are now registered only when shellcheck is present, consistent with
--     every other binary-gated linter in this file.

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
        -- html kept here so Mason auto-installs the binary; config owned by html.lua
        "html", "cssls", "jsonls", "yamlls",
        "clangd", "gopls", "solargraph",
        "kotlin_language_server",
        "zls",
        "tailwindcss",
        "elixir-ls",
        "fortls",
        "sqls",
        "jdtls",
      },
      automatic_installation = true,
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
      -- ── Capabilities helper ────────────────────────────────────────────
      local function get_capabilities()
        local ok, blink = pcall(require, "blink.cmp")
        if ok then return blink.get_lsp_capabilities() end
        return vim.lsp.protocol.make_client_capabilities()
      end

      local nvim_011 = vim.fn.has("nvim-0.11") == 1

      local function lsp_setup(server, config)
        config = config or {}
        config.capabilities = get_capabilities()
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

      -- ── Diagnostic jump helper ─────────────────────────────────────────
      local function diag_jump(count)
        if nvim_011 then
          pcall(function()
            vim.diagnostic.jump({ count = count, float = true })
          end)
        else
          if count > 0 then
            pcall(vim.diagnostic.goto_next, { float = true })
          else
            pcall(vim.diagnostic.goto_prev, { float = true })
          end
        end
      end

      -- ── LspAttach keymaps ──────────────────────────────────────────────
      vim.api.nvim_create_autocmd("LspAttach", {
        group    = vim.api.nvim_create_augroup("LspKeymaps", { clear = true }),
        callback = function(e)
          local map = function(keys, fn, desc, mode)
            vim.keymap.set(mode or "n", keys, fn,
              { buffer = e.buf, desc = "LSP: " .. desc })
          end

          map("gd",        vim.lsp.buf.definition,    "Go to Definition")
          map("gD",        vim.lsp.buf.declaration,   "Go to Declaration")
          map("gi",        vim.lsp.buf.implementation,"Go to Implementation")
          map("gr",        vim.lsp.buf.references,    "References")
          map("K",         vim.lsp.buf.hover,         "Hover Docs")
          map("<leader>k", vim.lsp.buf.signature_help,"Signature Help")

          local function code_action()
            local ok, ap = pcall(require, "actions-preview")
            if ok then ap.code_actions() else vim.lsp.buf.code_action() end
          end
          map("<leader>,a", code_action, "Code Action")
          map("<leader>,a", code_action, "Code Action", "v")

          vim.keymap.set("n", "<leader>,r", function()
            local word = vim.fn.expand("<cword>")
            local ok_cmd = pcall(vim.cmd, "IncRename " .. word)
            if not ok_cmd then vim.lsp.buf.rename() end
          end, { buffer = e.buf, desc = "LSP: Rename Symbol" })

          local function fmt()
            pcall(function()
              require("conform").format({ bufnr = e.buf, lsp_format = "fallback" })
            end)
          end
          map("<leader>,f", fmt, "Format")
          map("<leader>,f", fmt, "Format Range", "v")

          map("<leader>,d", vim.diagnostic.open_float, "Diagnostic Float")
          map("<leader>,l", vim.diagnostic.setloclist, "Diagnostic List")

          map("<leader>,t", function()
            local bufnr   = e.buf
            local enabled = vim.diagnostic.is_enabled({ bufnr = bufnr })
            vim.diagnostic.enable(not enabled, { bufnr = bufnr })
            vim.notify("Diagnostics " .. (enabled and "disabled" or "enabled"))
          end, "Toggle Diagnostics")

          map("<leader>ty", vim.lsp.buf.type_definition, "Type Definition")

          map("]d", function() diag_jump(1)  end, "Next Diagnostic")
          map("[d", function() diag_jump(-1) end, "Prev Diagnostic")

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

          map("<leader>,o", function()
            local ok_t = pcall(vim.cmd, "Trouble lsp_document_symbols toggle")
            if not ok_t then pcall(vim.lsp.buf.document_symbol) end
          end, "Code Outline (Trouble symbols)")
        end,
      })

      -- ── Server configurations ──────────────────────────────────────────
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
            local ep = vim.fn.exepath("elixir-ls")
            if ep ~= "" then return { ep } end
            local mason_ep = vim.fn.stdpath("data") .. "/mason/bin/elixir-ls"
            if vim.fn.filereadable(mason_ep) == 1 then return { mason_ep } end
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
        tailwindcss            = {},
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

      -- ── Optional servers (binary-presence gated) ───────────────────────
      -- OPT (v2.3.14): call shape now matches the primary `servers` table —
      -- each entry's config is passed directly to lsp_setup() without any
      -- intermediate reshaping. Previously each entry carried a redundant
      -- `name` and `binary` key distinct from its `config` sub-table.
      local optional = {
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
      }

      for _, entry in ipairs(optional) do
        if vim.fn.executable(entry.binary) == 1 then
          lsp_setup(entry.server, entry.config)
        end
      end

      -- ── Diagnostics UI ─────────────────────────────────────────────────
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
        -- NOTE: rust = { "rustfmt" } is owned by rust.lua (optional=true spec).
        -- rustfmt ships with rustup and does not need a Mason entry.
      },
      format_on_save = function(bufnr)
        if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then
          return nil
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

      -- OPT (v2.3.14): merge_linters() replaces the hand-rolled nested loop.
      -- Adds each linter to lint.linters_by_ft[ft] only if not already present,
      -- preventing duplicates when lang specs also register linters.
      local function merge_linters(ft, linters)
        lint.linters_by_ft[ft] = lint.linters_by_ft[ft] or {}
        local existing = lint.linters_by_ft[ft]
        for _, l in ipairs(linters) do
          if not vim.tbl_contains(existing, l) then
            table.insert(existing, l)
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

      -- FIX (v2.3.15): shellcheck is now registered for both "sh" and "bash"
      -- only when the binary is present. The previous unconditional
      -- merge_linters("sh", {"shellcheck"}) outside this guard meant nvim-lint
      -- tried to invoke a missing binary on every sh-file write. The duplicate
      -- guarded "sh" entry inside the block was dead code and is removed.
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
