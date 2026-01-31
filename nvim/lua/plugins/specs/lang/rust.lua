-- lua/plugins/specs/lang/rust.lua - Rust development
return {
  {
    "mrcjkb/rustaceanvim",
    version = "^5",
    ft = "rust",
    config = function()
      vim.g.rustaceanvim = {
        server = {
          on_attach = function(_, bufnr)
            vim.keymap.set("n", "<leader>ra", function() 
              vim.cmd.RustLsp("codeAction") 
            end, { buffer = bufnr, desc = "Rust Code Action" })
            
            vim.keymap.set("n", "<leader>rd", function() 
              vim.cmd.RustLsp("debuggables") 
            end, { buffer = bufnr, desc = "Rust Debuggables" })
            
            vim.keymap.set("n", "<leader>rr", function() 
              vim.cmd.RustLsp("runnables") 
            end, { buffer = bufnr, desc = "Rust Runnables" })
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
    end,
  },
}
