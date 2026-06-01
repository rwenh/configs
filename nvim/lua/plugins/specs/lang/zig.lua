-- lua/plugins/specs/lang/zig.lua — Zig language support
--

local shared = require("plugins.specs.lang.shared")

return {
  -- ── Conform: zigfmt custom config ──────────────────────────────────────────

  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters        = opts.formatters or {}
      opts.formatters.zigfmt = {
        command = "zig",
        args    = { "fmt", "--stdin" },
        stdin   = true,
      }
    end,
  },

  shared.treesitter({ "zig" }),

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
          local ok_dap, dap = pcall(require, "dap")
          if not ok_dap then
            vim.notify("[zig] nvim-dap not available — DAP config skipped",
              vim.log.levels.DEBUG)
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
          local exec = require("core.util.exec")
          if not exec.require_bin("zig", "See https://ziglang.org/download/") then
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
