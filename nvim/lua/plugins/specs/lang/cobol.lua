-- lua/plugins/specs/lang/cobol.lua - COBOL development

return {
  -- COBOL LSP
  {
    "neovim/nvim-lspconfig",
    ft = "cobol",
    config = function()
      local lspconfig = require("lspconfig")
      local capabilities = require("cmp_nvim_lsp").default_capabilities()
      
      -- Using IBM's COBOL Language Support
      lspconfig.cobol_ls.setup({
        capabilities = capabilities,
        filetypes = { "cobol" },
        settings = {
          cobol = {
            dialects = { "gnucobol", "ibm" },
          },
        },
      })
    end,
  },

  -- COBOL compilation helper
  {
    "akinsho/toggleterm.nvim",
    optional = true,
    keys = {
      {
        "<leader>cb",
        function()
          local file = vim.fn.expand("%:p")
          local exe = vim.fn.expand("%:p:r")
          local Terminal = require("toggleterm.terminal").Terminal
          local cobol_compile = Terminal:new({
            cmd = string.format("cobc -x -o %s %s && %s", exe, file, exe),
            direction = "float",
            close_on_exit = false,
          })
          cobol_compile:toggle()
        end,
        desc = "COBOL Compile & Run",
        ft = "cobol",
      },
    },
  },
}