-- lua/plugins/specs/lang/zig.lua - Zig development
-- LSP (zls) is configured in lsp.lua.

return {
  -- Zig syntax and auto-format on save
  {
    "ziglang/zig.vim",
    ft   = "zig",
    init = function()
      vim.g.zig_fmt_autosave = 1
    end,
  },

  -- DAP: lldb for Zig
  {
    "mfussenegger/nvim-dap",
    optional = true,
    ft       = "zig",
    config   = function()
      local dap = require("dap")

      -- Use codelldb (mason) if available, otherwise fall back to lldb-vscode
      local codelldb = vim.fn.stdpath("data") .. "/mason/bin/codelldb"
      local lldb     = vim.fn.exepath("lldb-vscode") ~= "" and vim.fn.exepath("lldb-vscode")
                    or vim.fn.exepath("lldb-dap")

      if vim.fn.filereadable(codelldb) == 1 then
        dap.adapters.zig = {
          type = "server", port = "${port}",
          executable = { command = codelldb, args = { "--port", "${port}" } },
        }
      elseif lldb then
        dap.adapters.zig = { type = "executable", command = lldb }
      end

      dap.configurations.zig = {
        {
          name    = "Launch Zig binary",
          type    = "zig",
          request = "launch",
          program = function()
            return vim.fn.input("Path to exe: ", vim.fn.getcwd() .. "/zig-out/bin/", "file")
          end,
          cwd         = "${workspaceFolder}",
          stopOnEntry = false,
        },
      }
    end,
  },

  -- Build & test via toggleterm
  {
    "akinsho/toggleterm.nvim",
    optional = true,
    keys = {
      {
        "<leader>zb",
        function()
          local Terminal = require("toggleterm.terminal").Terminal
          Terminal:new({ cmd = "zig build run", direction = "float", close_on_exit = false }):toggle()
        end,
        desc = "Zig Build Run",
        ft   = "zig",
      },
      {
        "<leader>zt",
        function()
          local Terminal = require("toggleterm.terminal").Terminal
          Terminal:new({ cmd = "zig build test", direction = "float", close_on_exit = false }):toggle()
        end,
        desc = "Zig Build Test",
        ft   = "zig",
      },
      {
        "<leader>zc",
        function()
          local file = vim.fn.expand("%:p")
          local Terminal = require("toggleterm.terminal").Terminal
          Terminal:new({
            cmd           = string.format("zig run %s", vim.fn.shellescape(file)),
            direction     = "float",
            close_on_exit = false,
          }):toggle()
        end,
        desc = "Zig Run File",
        ft   = "zig",
      },
    },
  },
}
