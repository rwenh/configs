-- lua/plugins/specs/lang/zig.lua — Zig language support
--
-- LSP:    zls via lsp.lua
-- Format: zigfmt via conform (this file)
-- DAP:    codelldb/lldb — references dap.lua's registered adapter
-- Test:   zig build test via runner.lua + build keymaps here
--

return {
  -- If vim.g.zig_fmt_autosave causes issues with a future zig.vim install,
  -- add it back here; for now the setting is meaningless without the plugin.

  -- ── Conform: zigfmt ────────────────────────────────────────────────────────

  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters_by_ft     = opts.formatters_by_ft or {}
      opts.formatters_by_ft.zig = { "zigfmt" }
      opts.formatters            = opts.formatters or {}
      opts.formatters.zigfmt     = {
        command = "zig",
        args    = { "fmt", "--stdin" },
        stdin   = true,
      }
    end,
  },

  -- ── Treesitter ─────────────────────────────────────────────────────────────
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "zig" })
      end
    end,
  },

  -- ── DAP: codelldb / lldb ───────────────────────────────────────────────────

  {
    "mfussenegger/nvim-dap",
    optional = true,
    ft       = "zig",
    init = function()
      vim.api.nvim_create_autocmd("FileType", {
        pattern  = "zig",
        group    = vim.api.nvim_create_augroup("ZigDap", { clear = true }),
        callback = function()
          local ok, dap = pcall(require, "dap")
          if not ok then
            vim.notify("nvim-dap not available", vim.log.levels.WARN)
            return
          end

          dap.configurations.zig = {
            {
              name    = "Launch Zig binary",
              type    = "codelldb",
              request = "launch",
              program = function()
                -- Try to infer the binary name from build.zig.
                local build_zig = vim.fn.findfile("build.zig", ".;")
                local default   = vim.fn.getcwd() .. "/zig-out/bin/"
                if build_zig ~= "" then
                  for _, line in ipairs(vim.fn.readfile(build_zig)) do
                    local name = line:match('addExecutable%([^,]+,?%s*"([^"]+)"')
                    if name then
                      local candidate = vim.fn.fnamemodify(build_zig, ":h")
                        .. "/zig-out/bin/" .. name
                      if vim.fn.filereadable(candidate) == 1 then
                        return candidate
                      end
                    end
                  end
                end
                return vim.fn.input("Path to exe: ", default, "file")
              end,
              cwd         = "${workspaceFolder}",
              stopOnEntry = false,
            },
          }
        end,
      })
    end,
  },

  -- ── Build & test keymaps ───────────────────────────────────────────────────
  {
    "akinsho/toggleterm.nvim",
    optional = true,
    keys = {
      {
        "<leader>zb",
        function()
          if vim.fn.findfile("build.zig", ".;") == "" then
            vim.notify("[zig] build.zig not found in project tree", vim.log.levels.WARN)
            return
          end
          require("core.util.term").float_at_root("zig build run")
        end,
        desc = "Zig Build Run",
        ft   = "zig",
      },
      {
        "<leader>zt",
        function()
          if vim.fn.findfile("build.zig", ".;") == "" then
            vim.notify("[zig] build.zig not found in project tree", vim.log.levels.WARN)
            return
          end
          require("core.util.term").float_at_root("zig build test")
        end,
        desc = "Zig Build Test",
        ft   = "zig",
      },
      {
        "<leader>zc",
        function()
          if vim.fn.executable("zig") ~= 1 then
            vim.notify("[zig] zig not found in PATH", vim.log.levels.ERROR)
            return
          end
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
