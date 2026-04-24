-- lua/plugins/specs/lang/zig.lua - Zig development
--
-- OPT (v2.3.13):
--   • Build keymaps use core.util.term.float() — 3 × boilerplate removed.
--
-- FIX (v2.3.15):
--   • DAP FileType autocmd had once=true, which is the same anti-pattern
--     fixed for neotest-rust in v2.3.10 (INSTALL.md #91). once=true causes
--     the autocmd to be permanently consumed by the FIRST FileType event
--     that fires, regardless of the filetype name — any non-zig buffer
--     opening first would discard the registration and Zig DAP would never
--     configure for the session. Removed once=true; the augroup clear=true
--     already provides idempotency on re-load.

return {
  -- ── Syntax support ────────────────────────────────────────────────────
  {
    "ziglang/zig.vim",
    ft   = "zig",
    init = function() vim.g.zig_fmt_autosave = 0 end,
  },

  -- ── Conform ───────────────────────────────────────────────────────────
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters_by_ft = opts.formatters_by_ft or {}
      opts.formatters_by_ft.zig = { "zigfmt" }
    end,
  },

  -- ── DAP: codelldb / lldb ──────────────────────────────────────────────
  {
    "mfussenegger/nvim-dap",
    optional = true,
    ft       = "zig",
    init = function()
      vim.api.nvim_create_autocmd("FileType", {
        pattern  = "zig",
        -- FIX (v2.3.15): once=true removed. It caused the autocmd to be consumed
        -- by the first FileType event of ANY filetype, not just zig. If any other
        -- buffer opened before a .zig file the Zig DAP adapter was never registered.
        -- The augroup with clear=true already guarantees idempotency on plugin reload.
        group    = vim.api.nvim_create_augroup("ZigDap", { clear = true }),
        callback = function()
          local ok, dap = pcall(require, "dap")
          if not ok then
            vim.notify("nvim-dap not available", vim.log.levels.WARN)
            return
          end

          local codelldb = vim.fn.stdpath("data") .. "/mason/bin/codelldb"
          local lldb     = vim.fn.exepath("lldb-vscode")
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

  -- ── Build & test keymaps ──────────────────────────────────────────────
  {
    "akinsho/toggleterm.nvim",
    optional = true,
    keys = {
      {
        "<leader>zb",
        function() require("core.util.term").float("zig build run") end,
        desc = "Zig Build Run",
        ft   = "zig",
      },
      {
        "<leader>zt",
        function() require("core.util.term").float("zig build test") end,
        desc = "Zig Build Test",
        ft   = "zig",
      },
      {
        "<leader>zc",
        function()
          require("core.util.term").float(
            "zig run " .. vim.fn.shellescape(vim.fn.expand("%:p"))
          )
        end,
        desc = "Zig Run File",
        ft   = "zig",
      },
    },
  },
}
