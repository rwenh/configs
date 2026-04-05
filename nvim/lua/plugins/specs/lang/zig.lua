-- lua/plugins/specs/lang/zig.lua - Zig development

return {
  -- Zig syntax support
  {
    "ziglang/zig.vim",
    ft   = "zig",
    init = function()
      vim.g.zig_fmt_autosave = 0
    end,
  },

  -- Conform formatter for Zig
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters_by_ft = opts.formatters_by_ft or {}
      opts.formatters_by_ft.zig = { "zigfmt" }
    end,
  },

  -- DAP: codelldb/lldb for Zig
  {
    "mfussenegger/nvim-dap",
    optional = true,
    ft       = "zig",
    init = function()
      vim.api.nvim_create_autocmd("FileType", {
        pattern  = "zig",
        once     = true,
        group    = vim.api.nvim_create_augroup("ZigDap", { clear = true }),
        callback = function()
          local ok, dap = pcall(require, "dap")
          if not ok then
            vim.notify("nvim-dap not available", vim.log.levels.WARN)
            return
          end

          local codelldb = vim.fn.stdpath("data") .. "/mason/bin/codelldb"

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
          else
            vim.notify("[zig] No debugger found (install codelldb or lldb-vscode)", vim.log.levels.WARN)
            return
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
          local ok, term = pcall(require, "toggleterm.terminal")
          if not ok then
            vim.notify("toggleterm not available", vim.log.levels.ERROR)
            return
          end

          term.Terminal:new({ cmd = "zig build run", direction = "float", close_on_exit = false }):toggle()
        end,
        desc = "Zig Build Run",
        ft   = "zig",
      },
      {
        "<leader>zt",
        function()
          local ok, term = pcall(require, "toggleterm.terminal")
          if not ok then
            vim.notify("toggleterm not available", vim.log.levels.ERROR)
            return
          end

          term.Terminal:new({ cmd = "zig build test", direction = "float", close_on_exit = false }):toggle()
        end,
        desc = "Zig Build Test",
        ft   = "zig",
      },
      {
        "<leader>zc",
        function()
          local ok, term = pcall(require, "toggleterm.terminal")
          if not ok then
            vim.notify("toggleterm not available", vim.log.levels.ERROR)
            return
          end

          local file = vim.fn.expand("%:p")
          term.Terminal:new({
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
