-- lua/plugins/specs/lang/zig.lua - Zig development

return {
  -- Zig LSP and tools
  {
    "neovim/nvim-lspconfig",
    ft = "zig",
    config = function()
      local lspconfig = require("lspconfig")
      local capabilities = require("cmp_nvim_lsp").default_capabilities()
      
      lspconfig.zls.setup({
        capabilities = capabilities,
        settings = {
          zls = {
            enable_autofix = true,
            enable_snippets = true,
            warn_style = true,
          },
        },
      })
    end,
  },

  -- Zig.vim for syntax and utilities
  {
    "ziglang/zig.vim",
    ft = "zig",
    init = function()
      vim.g.zig_fmt_autosave = 1
    end,
  },

  -- Zig build integration
  {
    "akinsho/toggleterm.nvim",
    optional = true,
    keys = {
      {
        "<leader>zb",
        function()
          local Terminal = require("toggleterm.terminal").Terminal
          local zig_build = Terminal:new({
            cmd = "zig build run",
            direction = "float",
            close_on_exit = false,
          })
          zig_build:toggle()
        end,
        desc = "Zig Build Run",
        ft = "zig",
      },
      {
        "<leader>zt",
        function()
          local Terminal = require("toggleterm.terminal").Terminal
          local zig_test = Terminal:new({
            cmd = "zig build test",
            direction = "float",
            close_on_exit = false,
          })
          zig_test:toggle()
        end,
        desc = "Zig Test",
        ft = "zig",
      },
    },
  },
}