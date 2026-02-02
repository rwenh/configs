-- lua/plugins/specs/lang/rust.lua - Rust development (SAFE KEYMAPS)

return {
  {
    "mrcjkb/rustaceanvim",
    version = "^5",
    ft = "rust",
    config = function()
      vim.g.rustaceanvim = {
        server = {
          on_attach = function(_, bufnr)
            -- Use 'rs' prefix for Rust-specific actions
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
          enabled = true,
          actions = true,
          completion = true,
          hover = true,
        },
      })
      
      -- Crate management keymaps
      local crates = require("crates")
      vim.keymap.set("n", "<leader>rsc", crates.toggle, { desc = "Toggle crates" })
      vim.keymap.set("n", "<leader>rsu", crates.update_crate, { desc = "Update crate" })
      vim.keymap.set("v", "<leader>rsu", crates.update_crates, { desc = "Update crates" })
      vim.keymap.set("n", "<leader>rsU", crates.upgrade_crate, { desc = "Upgrade crate" })
      vim.keymap.set("v", "<leader>rsU", crates.upgrade_crates, { desc = "Upgrade crates" })
    end,
  },
}
