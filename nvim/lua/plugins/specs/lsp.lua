-- lua/plugins/specs/lsp.lua
--
-- FIX (v2.3.12):
--   • elixirls cmd: was hardcoded as
--       { vim.fn.stdpath("data") .. "/mason/bin/elixir-ls" }
--     Mason's symlink for the elixir-ls package is named "elixir-ls" on
--     some systems but the actual launcher script that the DAP/LSP protocol
--     expects is "language_server.sh" / "elixir-ls". Using vim.fn.exepath()
--     is the same pattern every other Mason binary uses (mason_bin helper in
--     dap.lua). Fixed: resolve via vim.fn.exepath("elixir-ls") with Mason
--     fallback, consistent with all other server cmd resolutions.
--   • fortls optional entry: was config={} (empty). fortls requires at least
--     notifyInit=true to surface its ready signal; without it the LSP attaches
--     silently and the first hover/completion often races the init. Added
--     minimal settings block with the options that matter in practice.
--   • sqls optional entry: was config={} (empty). sqls needs workspace/
--     connections config to be useful, but at minimum it should declare its
--     filetypes explicitly so it doesn't attach to non-SQL buffers opened
--     while a .sql file is in the session. Added filetypes guard.

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
        -- html kept here so Mason auto-installs the binary; config is owned by html.lua
        "html", "cssls", "jsonls", "yamlls",
        "clangd", "gopls", "solargraph",
        "kotlin_language_server",
        "zls",
        "tailwindcss",
        "elixir-ls",
        "fortls",
        "sqls",
        -- jdtls: java.lua uses nvim-jdtls directly (bypasses mason-lspconfig)
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
            -- Mason fallback: the package installs as "elixir-ls" in mason/bin
            local mason_ep = vim.fn.stdpath("data") .. "/mason/bin/elixir-ls"
            if vim.fn.filereadable(mason_ep) == 1 then return { mason_ep } end
            return { "elixir-ls" }  -- last-resort; will fail with a clear error
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
      local optional = {
        -- NOTE: Mason package is "rust_hdl"; its bin symlink is "vhdl_ls".
        { name = "vhdl_ls",  binary = "vhdl_ls",
          config = { filetypes = { "vhdl", "vhd" } } },

        -- FIX (v2.3.12): fortls needs notifyInit=true to surface its ready
        -- signal; without it the first hover/completion races LSP init and
        -- returns empty results. Also set nthreads for responsiveness.
        { name = "fortls",   binary = "fortls",
          config = {
            filetypes = { "fortran" },
            settings  = {
              fortls = {
                notifyInit      = true,
                nthreads        = 4,
                hover_signature = true,
                use_signature_help = true,
              },
            },
          },
        },

        { name = "sqls",     binary = "sqls",
          config = {
            filetypes = { "sql", "mysql" },
            on_attach = function(client, _)
              -- sqls has a workspace/execute command but its hover is weak;
              -- disable rename to avoid accidental SQL identifier renames.
              client.server_capabilities.renameProvider = false
            end,
          },
        },

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
