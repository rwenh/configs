-- nvim/lua/plugins/specs/lang/rust.lua - Rust language support

return {
  {
    "mrcjkb/rustaceanvim",
    version = "^5",
    lazy = false,
    ft = { "rust" },
    init = function()
      -- RECALIBRATION: Safe vim.g.rustaceanvim setup
      vim.g.rustaceanvim = {
        server = {
          on_attach = function(client, bufnr)
            if client.server_capabilities.inlayHintProvider then
              pcall(function() vim.lsp.inlay_hint.enable(true, { bufnr = bufnr }) end)
            end
          end,
          default_settings = {
            ["rust-analyzer"] = {
              assist = {
                importMergeBehavior = "last",
                importPrefix = "by_self",
              },
              cargo = {
                loadOutDirsFromCheck = true,
              },
              procMacro = {
                enable = true,
              },
              checkOnSave = {
                command = "clippy",
                extraArgs = { "--all-targets", "--all-features" },
              },
              inlayHints = {
                bindingModeHints = { enable = false },
                closingBraceHints = { minLines = 25 },
                closureCaptureHints = { enable = false },
                closureReturnTypeHints = { enable = false },
                discriminantHints = { enable = "fieldless" },
                implicitDrops = { enable = false },
                lifetimeElisionHints = { enable = "never" },
                parameterHints = { enable = true },
                rangeHints = { enable = false },
                renderColonAfterFunctionParameter = false,
                typeHints = {
                  enable = true,
                  hideClosureInitialization = false,
                  hideNamedConstructor = false,
                },
              },
            },
          },
        },
        dap = {
          adapter = "rt_lldb",
        },
        tools = {
          test_executor = "swole",
          hover_actions = {
            auto_focus = false,
          },
          code_action_group = {
            enable = true,
          },
        },
      }
    end,
    keys = {
      { "<leader>rh", "<cmd>RustHoverActions<CR>", desc = "Rust Hover Actions" },
      { "<leader>ra", "<cmd>RustCodeAction<CR>", desc = "Rust Code Action" },
      { "<leader>rd", "<cmd>RustDebuggables<CR>", desc = "Rust Debuggables" },
      { "<leader>rt", "<cmd>RustTest<CR>", desc = "Rust Test" },
    },
  },

  {
    "andythigpen/nvim-coverage",
    dependencies = { "nvim-lua/plenary.nvim" },
    cmd = {
      "Coverage",
      "CoverageLoad",
      "CoverageClear",
      "CoverageHide",
      "CoverageShow",
      "CoverageToggle",
      "CoverageSummary",
    },
    opts = {
      auto_reload = true,
    },
  },
}
