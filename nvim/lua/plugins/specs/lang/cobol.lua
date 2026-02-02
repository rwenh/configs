-- lua/plugins/specs/lang/cobol.lua - COBOL development
-- Note: LSP (cobol_ls) is configured in lsp.lua, no need to duplicate here

return {
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
