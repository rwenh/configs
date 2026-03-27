-- lua/plugins/specs/lang/zig.lua - Zig development
-- LSP (zls) is configured in lsp.lua.

return {
  -- Zig syntax support
  {
    "ziglang/zig.vim",
    ft   = "zig",
    init = function()
      -- FIX #7: Disabled zig.vim's built-in fmt-on-save — conform handles
      -- formatting via format_on_save in lsp.lua. Having both active caused
      -- double formatting on BufWritePre with potential output conflicts.
      vim.g.zig_fmt_autosave = 0
    end,
  },

  -- FIX #8: Conform formatter for Zig — needed since zig_fmt_autosave is now 0.
  -- conform has native support for zigfmt (wraps zig fmt).
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = {
      formatters_by_ft = {
        zig = { "zigfmt" },
      },
    },
  },

  -- DAP: codelldb/lldb for Zig
  {
    "mfussenegger/nvim-dap",
    optional = true,
    ft       = "zig",
    -- FIX #5: optional=true config never runs — moved to init.
    -- The Zig DAP adapter was silently never registered, making debugging
    -- non-functional. dap.lua already registers codelldb for C/C++; Zig
    -- reuses the same adapter since codelldb supports Zig binaries.
    init = function()
      vim.api.nvim_create_autocmd("FileType", {
        pattern  = "zig",
        once     = true,
        group    = vim.api.nvim_create_augroup("ZigDap", { clear = true }),
        callback = function()
          local ok, dap = pcall(require, "dap")
          if not ok then return end

          local codelldb = vim.fn.stdpath("data") .. "/mason/bin/codelldb"

          -- FIX #6: Added ~= "" guard on lldb-dap fallback — without it,
          -- exepath returning "" would be truthy and set an empty command.
          local lldb = vim.fn.exepath("lldb-vscode")
          if lldb == "" then lldb = vim.fn.exepath("lldb-dap") end
          if lldb == "" then lldb = nil end

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
      })
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
