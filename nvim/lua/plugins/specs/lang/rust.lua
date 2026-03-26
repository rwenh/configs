-- lua/plugins/specs/lang/rust.lua - Rust development

return {
  {
    "mrcjkb/rustaceanvim",
    version = "^5",
    ft      = "rust",
    -- FIX #1: vim.g.rustaceanvim must be set in init, not config.
    -- rustaceanvim reads it at plugin load time — setting it in config is a
    -- race that may fire after the plugin has already initialised.
    -- FIX #2: lsp.lua's rust_analyzer entry has been removed (see lsp.lua).
    -- rustaceanvim manages rust-analyzer exclusively — having both caused two
    -- rust-analyzer clients to attach per Rust buffer (duplicate diagnostics,
    -- completions, hover).
    init = function()
      vim.g.rustaceanvim = {
        server = {
          on_attach = function(_, bufnr)
            -- NOTE (Fix #4): lsp.lua's LspAttach also sets gd/K/<leader>,f
            -- etc. for this buffer. Use <leader>rsh for the richer Rust hover
            -- actions; K remains plain hover from the LSP layer.
            vim.keymap.set("n", "<leader>rsa", function()
              vim.cmd.RustLsp("codeAction")
            end, { buffer = bufnr, desc = "Rust Code Action" })

            vim.keymap.set("n", "<leader>rsd", function()
              vim.cmd.RustLsp("debuggables")
            end, { buffer = bufnr, desc = "Rust Debuggables" })

            vim.keymap.set("n", "<leader>rsr", function()
              vim.cmd.RustLsp("runnables")
            end, { buffer = bufnr, desc = "Rust Runnables" })

            vim.keymap.set("n", "<leader>rse", function()
              vim.cmd.RustLsp("expandMacro")
            end, { buffer = bufnr, desc = "Rust Expand Macro" })

            vim.keymap.set("n", "<leader>rsh", function()
              vim.cmd.RustLsp("hover", "actions")
            end, { buffer = bufnr, desc = "Rust Hover Actions" })
          end,
        },
      }
    end,
  },

  {
    "saecki/crates.nvim",
    event = "BufRead Cargo.toml",
    config = function()
      require("crates").setup({
        lsp = {
          enabled    = true,
          actions    = true,
          completion = true,
          hover      = true,
        },
      })

      -- FIX #3: Crate keymaps scoped to the current buffer (Cargo.toml).
      -- The originals were global — active in every buffer.
      local crates = require("crates")
      local buf    = { buffer = 0 }
      vim.keymap.set("n", "<leader>rsc", crates.toggle,         vim.tbl_extend("force", buf, { desc = "Toggle crates" }))
      vim.keymap.set("n", "<leader>rsu", crates.update_crate,   vim.tbl_extend("force", buf, { desc = "Update crate" }))
      vim.keymap.set("v", "<leader>rsu", crates.update_crates,  vim.tbl_extend("force", buf, { desc = "Update crates" }))
      vim.keymap.set("n", "<leader>rsU", crates.upgrade_crate,  vim.tbl_extend("force", buf, { desc = "Upgrade crate" }))
      vim.keymap.set("v", "<leader>rsU", crates.upgrade_crates, vim.tbl_extend("force", buf, { desc = "Upgrade crates" }))
    end,
  },
}
