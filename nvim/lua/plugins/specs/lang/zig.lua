-- lua/plugins/specs/lang/zig.lua - Zig development
-- Note: LSP (zls) is configured in lsp.lua, no need to duplicate here

return {
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
