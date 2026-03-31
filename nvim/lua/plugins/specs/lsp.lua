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
        "lua_ls", "basedpyright",
        -- NOTE: rust_analyzer removed — rustaceanvim owns it (see rust.lua)
        -- NOTE: ts_ls removed — typescript-tools.nvim owns tsserver (see typescript.lua)
        "html", "cssls", "jsonls", "yamlls",
        "clangd", "gopls", "solargraph",
        -- NOTE: elixirls removed from ensure_installed — mason-lspconfig alias
        -- "elixirls" → Mason package "elixir-ls" is a known breakage point across
        -- versions. elixir-tools.nvim has elixirls.enable=false anyway so this
        -- server is managed by lsp.lua directly; ensure MasonInstallAll installs
        -- "elixir-ls" (see commands.lua) and the cmd path in servers table handles
        -- the rest. No auto-install via mason-lspconfig needed.
        "kotlin_language_server", -- Mason package: kotlin-language-server ✓
        "zls",
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
      -- NOTE (Fix #1): This is the SOLE owner of LspAttach keymaps. The
      -- LspAttach block previously duplicated in keymaps.lua has been removed.
      -- Both would fire on every attach, double-registering all LSP bindings.
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
          -- NOTE: <leader>,a is also mapped in editor.lua's actions-preview.nvim
          -- spec. That spec-level key takes precedence as a lazy-load trigger;
          -- once loaded, actions-preview intercepts the binding. This fallback
          -- fires only before actions-preview loads or if it's removed.
          map("<leader>,a", vim.lsp.buf.code_action,     "Code Action")
          -- FIX #2: Format via conform.nvim, not vim.lsp.buf.format().
          -- lsp.lua's buffer-local binding was silently overriding the conform
          -- fix already applied in keymaps.lua. conform handles the full
          -- formatter chain (prettier, stylua, black, etc.) with lsp_fallback.
          map("<leader>,f", function()
            require("conform").format({ bufnr = e.buf, lsp_fallback = true })
          end, "Format")
          map("<leader>,d", vim.diagnostic.open_float,   "Diagnostic Float")
          map("<leader>,l", vim.diagnostic.setloclist,   "Diagnostic List")
          -- FIX #6: Renamed <leader>td → <leader>ty (Type definition) to
          -- resolve collision with test.lua's <leader>td (Test Debug Nearest).
          -- test.lua's binding is global across all filetypes; this one is
          -- buffer-local but the naming clash caused confusion in which-key.
          map("<leader>ty", vim.lsp.buf.type_definition, "Type Definition")
          map("]d",         vim.diagnostic.goto_next,    "Next Diagnostic")
          map("[d",         vim.diagnostic.goto_prev,    "Prev Diagnostic")
          vim.keymap.set("v", "<leader>,a", vim.lsp.buf.code_action, { buffer = e.buf, desc = "LSP: Code Action (fallback)" })
          -- FIX #2 (visual range): Also route through conform for range format.
          vim.keymap.set("v", "<leader>,f", function()
            require("conform").format({ bufnr = e.buf, lsp_fallback = true })
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
        -- NOTE (Fix rust.lua #2): rust_analyzer removed — rustaceanvim in
        -- rust.lua manages rust-analyzer exclusively via vim.g.rustaceanvim.
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
          -- FIX #3: Use stdpath("data") instead of hardcoded ~/.local/share/nvim
          -- for portability with custom $XDG_DATA_HOME configurations.
          cmd = { vim.fn.stdpath("data") .. "/mason/bin/elixir-ls" },
        },
        -- NOTE: ts_ls removed — typescript-tools.nvim in typescript.lua
        -- manages tsserver exclusively to prevent double client attachment.
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
          binary = "vhdl_ls",   -- installed by Mason package "rust_hdl"
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
          binary = "cobol-language-server", -- installed by Mason package "cobol-language-support"
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

  -- NOTE (Fix #7): lsp_signature.nvim removed — blink.cmp already provides
  -- signature help via `signature = { enabled = true }` in completion.lua.
  -- Having both active causes duplicate signature popups on function calls.
  -- blink's built-in integrates better with the completion menu.

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
        -- FIX #8: Added missing formatters that are installed by MasonInstallAll
        -- but were not wired into conform — they were installed but never invoked.
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

  -- Linting
  {
    "mfussenegger/nvim-lint",
    -- FIX #4: Changed BufReadPre → BufReadPost. nvim-lint needs buffer content
    -- to lint; BufReadPre fires before content is loaded.
    event  = "BufReadPost",
    config = function()
      require("lint").linters_by_ft = {
        python     = { "ruff" },
        javascript = { "eslint_d" },
        typescript = { "eslint_d" },
        sh         = { "shellcheck" },
        ruby       = { "rubocop" },
      }
      -- FIX #5: Added augroup so this autocmd doesn't accumulate duplicates
      -- on plugin reload or re-source.
      vim.api.nvim_create_autocmd({ "BufWritePost", "InsertLeave" }, {
        group    = vim.api.nvim_create_augroup("NvimLint", { clear = true }),
        callback = function() require("lint").try_lint() end,
      })
    end,
  },
}
