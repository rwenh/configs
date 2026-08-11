-- lua/plugins/specs/lang/zig.lua — Zig language support
--

local shared = require("plugins.specs.lang.shared")

-- ── build.zig detection ─────────────────────────────────────────────────────
--
---@return string  path to build.zig, or "" if none found above the current buffer
local function find_build_zig()
  local dir = vim.fn.expand("%:p:h")
  local search_path = (dir ~= "" and dir or ".") .. ";"
  return vim.fn.findfile("build.zig", search_path)
end

---@param build_zig string  a non-empty path returned by find_build_zig()
---@return string
local function build_zig_dir(build_zig)
  return vim.fn.fnamemodify(build_zig, ":p:h")
end

return {
  { "stevearc/conform.nvim", optional = true,
    opts = function(_, opts)
      opts.formatters = opts.formatters or {}
      opts.formatters.zigfmt = { command = "zig", args = { "fmt", "--stdin" }, stdin = true }
    end },

  shared.treesitter({ "zig" }),

  -- ── DAP: codelldb / lldb ───────────────────────────────────────────────────
  {
    "mfussenegger/nvim-dap", optional = true, ft = "zig",
    init = function()
      vim.api.nvim_create_autocmd("FileType", {
        pattern  = "zig",
        group    = vim.api.nvim_create_augroup("ZigDap", { clear = true }),
        callback = function()
          local ok_dap, dap = pcall(require, "dap")
          if not ok_dap then return end

          dap.configurations.zig = {
            {
              name    = "Launch Zig binary",
              type    = "codelldb",
              request = "launch",
              program = function()
                local build_zig = find_build_zig()
                local default   = vim.fn.getcwd() .. "/zig-out/bin/"
                if build_zig ~= "" then
                  local ok_rf, lines = pcall(vim.fn.readfile, build_zig)
                  if ok_rf then
                    for _, line in ipairs(lines) do
                      local name = line:match('addExecutable%([^,]+,?%s*"([^"]+)"')
                        or line:match('addExecutable%(.name%s*=%s*"([^"]+)"')
                      if name then
                        local candidate = build_zig_dir(build_zig) .. "/zig-out/bin/" .. name
                        if vim.fn.filereadable(candidate) == 1 then return candidate end
                      end
                    end
                  end
                end
                return vim.fn.input("Path to exe: ", default, "file")
              end,
              cwd = "${workspaceFolder}", stopOnEntry = false,
            },
          }
        end,
      })
    end,
  },

  -- ── Build, test, and utility keymaps ─────────────────────────────────────
  {
    "akinsho/toggleterm.nvim",
    keys = {
      {
        "<leader>zb",
        function()
          local build_zig = find_build_zig()
          if build_zig == "" then
            vim.notify("[zig] build.zig not found", vim.log.levels.WARN); return
          end
          local term = require("core.util.term")
          term.float(term.cd_prefix(build_zig_dir(build_zig)) .. "zig build run")
        end,
        desc = "Zig Build Run", ft = "zig",
      },
      {
        "<leader>zt",
        function()
          local build_zig = find_build_zig()
          if build_zig == "" then
            vim.notify("[zig] build.zig not found", vim.log.levels.WARN); return
          end
          local term = require("core.util.term")
          term.float(term.cd_prefix(build_zig_dir(build_zig)) .. "zig build test")
        end,
        desc = "Zig Build Test", ft = "zig",
      },
      {
        "<leader>zc",
        function()
          local exec = require("core.util.exec")
          if not exec.require_bin("zig", "See https://ziglang.org/download/") then return end
          require("core.util.term").float("zig run " .. vim.fn.shellescape(vim.fn.expand("%:p")))
        end,
        desc = "Zig Run File", ft = "zig",
      },

      -- ── zig fetch (add dependency) ────────────────────────────────────────
      {
        "<leader>zf",
        function()
          local exec = require("core.util.exec")
          if not exec.require_bin("zig", "See https://ziglang.org/download/") then return end

          local build_zig = find_build_zig()
          if build_zig == "" then
            vim.notify("[zig] build.zig not found — zig fetch needs a Zig project", vim.log.levels.WARN)
            return
          end

          vim.ui.input({ prompt = "zig fetch URL: " }, function(url)
            if not url or vim.trim(url) == "" then return end
            local term = require("core.util.term")
            term.float(
              term.cd_prefix(build_zig_dir(build_zig))
              .. "zig fetch --save " .. vim.fn.shellescape(url)
            )
          end)
        end,
        desc = "Zig fetch (add dep)", ft = "zig",
      },

      -- ── zig env info panel ────────────────────────────────────────────────
      {
        "<leader>ze",
        function()
          local exec = require("core.util.exec")
          if not exec.require_bin("zig", "See https://ziglang.org/download/") then return end
          local out = vim.fn.system("zig env 2>/dev/null")
          if vim.v.shell_error ~= 0 or out == "" then
            vim.notify("[zig] zig env failed", vim.log.levels.ERROR); return
          end
          -- Display in a floating scratch buffer.
          local buf = vim.api.nvim_create_buf(false, true)
          vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.split(out, "\n"))
          vim.bo[buf].filetype  = "json"
          vim.bo[buf].buftype   = "nofile"
          vim.bo[buf].modifiable = false
          local width  = math.min(80, vim.o.columns - 4)
          local height = math.min(20, vim.o.lines - 6)
          vim.api.nvim_open_win(buf, true, {
            relative = "editor", style = "minimal", border = "rounded",
            width = width, height = height,
            row = math.floor((vim.o.lines - height) / 2),
            col = math.floor((vim.o.columns - width) / 2),
          })
          vim.keymap.set("n", "q", "<cmd>close<cr>", { buffer = buf, silent = true })
        end,
        desc = "Zig env info", ft = "zig",
      },
    },
  },
}
