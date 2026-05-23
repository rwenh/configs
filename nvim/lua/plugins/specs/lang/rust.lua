-- lua/plugins/specs/lang/rust.lua — Rust language support
--

return {
  {
    "mrcjkb/rustaceanvim",
    version = "^5",
    lazy    = false,
    ft      = { "rust" },
    init = function()
      vim.g.rustaceanvim = {
        server = {
          on_attach = function(client, bufnr)
            if client.server_capabilities.inlayHintProvider then
              local already = pcall(function()
                return vim.lsp.inlay_hint.is_enabled({ bufnr = bufnr })
              end)
              if not already then
                pcall(function()
                  vim.lsp.inlay_hint.enable(true, { bufnr = bufnr })
                end)
              end
            end
          end,
          default_settings = {
            ["rust-analyzer"] = {
              assist = {
                importMergeBehavior = "last",
                importPrefix        = "by_self",
              },
              cargo = { loadOutDirsFromCheck = true },
              procMacro = { enable = true },
              checkOnSave = {
                command    = "clippy",
                extraArgs  = { "--all-targets", "--all-features" },
              },
              inlayHints = {
                parameterHints      = { enable = true },
                typeHints           = { enable = true },
                closingBraceHints   = { minLines = 25 },
              },
            },
          },
        },
        dap = {
          adapter = (function()
            local mason = require("core.util.mason")
            local cmd   = mason.bin("codelldb")
            if vim.fn.executable(cmd) ~= 1 then
              vim.schedule(function()
                vim.notify(
                  "[rust] codelldb not found — Rust debug unavailable.\n"
                  .. "Run: :MasonInstall codelldb",
                  vim.log.levels.WARN
                )
              end)
            end
            return "codelldb"
          end)(),
        },
        tools = {
          test_executor = "swole",
          hover_actions = { auto_focus = false },
        },
      }
    end,

    keys = (function()
      local entries = {
        { "<leader>rh", "RustLsp hover",       "Rust Hover Actions" },
        { "<leader>ra", "RustLsp codeAction",  "Rust Code Action"   },
        { "<leader>rd", "RustLsp debuggables", "Rust Debuggables"   },
        { "<leader>rt", "RustLsp testables",   "Rust Testables"     },
      }
      local keys = {}
      for _, e in ipairs(entries) do
        table.insert(keys, {
          e[1], "<cmd>" .. e[2] .. "<cr>",
          desc = e[3], ft = "rust",
        })
      end
      return keys
    end)(),
  },

  -- ── Conform: rustfmt custom config ─────────────────────────────────────────

  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters = opts.formatters or {}

      local _edition_cache = {}
      local function detect_edition()
        local cargo = vim.fn.findfile("Cargo.toml", ".;")
        if cargo == "" then return "2021" end
        local abs = vim.fn.fnamemodify(cargo, ":p")
        if _edition_cache[abs] then return _edition_cache[abs] end
        for _, line in ipairs(vim.fn.readfile(abs)) do
          local ed = line:match('^edition%s*=%s*"(%d+)"')
          if ed then _edition_cache[abs] = ed; return ed end
        end
        _edition_cache[abs] = "2021"
        return "2021"
      end

      opts.formatters.rustfmt = {
        command = "rustfmt",
        args    = function()
          return { "--edition", detect_edition(), "--emit=stdout" }
        end,
        stdin   = true,
      }
    end,
  },
}
