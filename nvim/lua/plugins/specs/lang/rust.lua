-- lua/plugins/specs/lang/rust.lua - Rust language support
--
-- FIX (v2.2.3):
--   • rustaceanvim v5 renamed commands:
--       :RustHoverActions  → :RustLsp hover
--       :RustCodeAction    → :RustLsp codeAction
--       :RustDebuggables   → :RustLsp debuggables
--       :RustTest          → :RustLsp testables
--     The old names are deprecated stubs; they may be removed in a future
--     patch. All keymaps updated to the v5 canonical form.
--   • nvim-coverage duplicate removed. test.lua is the sole owner.
--     Having it here caused two config() calls; the last one won, silently
--     resetting any test.lua customisation.

return {
  {
    "mrcjkb/rustaceanvim",
    version = "^5",
    lazy = false,
    ft = { "rust" },
    init = function()
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
                bindingModeHints            = { enable = false },
                closingBraceHints           = { minLines = 25 },
                closureCaptureHints         = { enable = false },
                closureReturnTypeHints      = { enable = false },
                discriminantHints           = { enable = "fieldless" },
                implicitDrops               = { enable = false },
                lifetimeElisionHints        = { enable = "never" },
                parameterHints              = { enable = true },
                rangeHints                  = { enable = false },
                renderColonAfterFunctionParameter = false,
                typeHints = {
                  enable                    = true,
                  hideClosureInitialization = false,
                  hideNamedConstructor      = false,
                },
              },
            },
          },
        },
        dap = {
          adapter = "codelldb",
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
    -- FIX: v5 canonical command syntax — :RustLsp <subcommand>
    keys = {
      { "<leader>rh", "<cmd>RustLsp hover<cr>",        desc = "Rust Hover Actions" },
      { "<leader>ra", "<cmd>RustLsp codeAction<cr>",   desc = "Rust Code Action" },
      { "<leader>rd", "<cmd>RustLsp debuggables<cr>",  desc = "Rust Debuggables" },
      { "<leader>rt", "<cmd>RustLsp testables<cr>",    desc = "Rust Test" },
    },
  },
  -- nvim-coverage intentionally removed — test.lua is the sole owner.
}
