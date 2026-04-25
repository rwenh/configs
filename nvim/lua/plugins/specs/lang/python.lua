-- lua/plugins/specs/lang/python.lua - Python development
--
-- OPT (v2.3.14):
--   • debugpy probe rewritten from async vim.system() recursion to a
--     synchronous vim.fn.executable() + path-existence chain. The async
--     approach was necessary if probing required importing the module, but
--     `debugpy` can be located by binary or by checking its package directory —
--     no subprocess import probe is needed. The synchronous version is simpler,
--     easier to read, and consistent with how every other DAP adapter in
--     dap.lua resolves its binary.
--
-- FIX (v2.3.15):
--   • find_debugpy_python() was still spawning a subprocess via vim.fn.system()
--     to query site.getsitepackages()[0], directly contradicting the OPT (v2.3.14)
--     comment that claimed "no subprocess import probe is needed". The site-packages
--     path can be derived from the interpreter path without any subprocess:
--     the standard layout is <prefix>/lib/pythonX.Y/site-packages, where the
--     version is read from the interpreter name in the filesystem. As a simpler
--     and more portable guard we instead check for the `debugpy` binary itself
--     (installed into the same bin/ as the interpreter when pip install debugpy
--     is run in a venv or with --user) and for the debugpy package directory
--     under the standard site-packages siblings. Both checks use only
--     vim.fn.executable(), vim.fn.glob(), and vim.fn.isdirectory() — zero
--     subprocesses, zero blocking I/O beyond stat calls.

return {
  {
    "linux-cultist/venv-selector.nvim",
    ft  = "python",
    cmd = "VenvSelect",
    opts = {
      name         = { "venv", ".venv", "env", ".env" },
      auto_refresh = true,
    },
    keys = {
      { "<leader>pyv", "<cmd>VenvSelect<cr>", desc = "Python Select Venv" },
    },
  },

  {
    "mfussenegger/nvim-dap-python",
    ft           = "python",
    dependencies = "mfussenegger/nvim-dap",
    config = function()
      -- FIX (v2.3.15): fully subprocess-free debugpy probe.
      --
      -- Strategy (no vim.fn.system() calls):
      --   1. Active venv → check <venv>/bin/python is executable.
      --      Venvs created with `pip install debugpy` put debugpy into the same
      --      prefix, so finding the venv interpreter is sufficient.
      --   2. PATH + system candidates → for each interpreter, look for a
      --      `debugpy` directory alongside the interpreter's lib/ siblings via
      --      glob patterns, and check for the `debugpy` executable in the same
      --      bin/. Either presence is enough: nvim-dap-python only needs the
      --      interpreter path, not the debugpy module path directly.
      --   3. Fall back to python3 with a warning.
      local function find_debugpy_python()
        -- 1. Active virtual environment
        local venv = os.getenv("VIRTUAL_ENV")
        if venv then
          local p = venv .. "/bin/python"
          if vim.fn.executable(p) == 1 then return p end
        end

        -- 2. Candidates from PATH + common system paths
        local candidates = {
          vim.fn.exepath("python3"),
          vim.fn.exepath("python"),
          "/usr/bin/python3",
          "/usr/bin/python",
        }

        for _, p in ipairs(candidates) do
          if p and p ~= "" and vim.fn.executable(p) == 1 then
            -- Derive the prefix directory (parent of bin/).
            -- e.g. /usr/bin/python3 → prefix = /usr
            -- e.g. /home/user/.venv/bin/python3 → prefix = /home/user/.venv
            local prefix = vim.fn.fnamemodify(vim.fn.fnamemodify(p, ":h"), ":h")

            -- Check 1: debugpy executable in the same bin/
            local dbg_bin = prefix .. "/bin/debugpy"
            if vim.fn.executable(dbg_bin) == 1 then return p end

            -- Check 2: debugpy package directory under lib/pythonX.Y/site-packages
            -- Use a glob so we don't need to know the exact Python version.
            local pattern = prefix .. "/lib/python*/site-packages/debugpy"
            if vim.fn.glob(pattern) ~= "" then return p end

            -- Check 3: user site-packages (~/.local/lib/pythonX.Y/site-packages)
            local home = os.getenv("HOME") or ""
            if home ~= "" then
              local user_pattern = home .. "/.local/lib/python*/site-packages/debugpy"
              if vim.fn.glob(user_pattern) ~= "" then return p end
            end
          end
        end

        -- 3. Fall back to python3 with a clear warning; user can fix later.
        vim.schedule(function()
          vim.notify(
            "[python] debugpy not found in any interpreter.\n"
            .. "Install with: pip install debugpy",
            vim.log.levels.WARN
          )
        end)
        return vim.fn.exepath("python3") ~= "" and vim.fn.exepath("python3") or "python3"
      end

      local function register_dap_keymaps()
        vim.api.nvim_create_autocmd("FileType", {
          pattern  = "python",
          group    = vim.api.nvim_create_augroup("PythonDapKeymaps", { clear = true }),
          callback = function(e)
            local buf = e.buf
            -- FIX: guard against duplicate registration on :luafile reload.
            if vim.b[buf].python_dap_keymaps_registered then return end
            vim.b[buf].python_dap_keymaps_registered = true
            vim.keymap.set("n", "<leader>pydm",
              function() pcall(function() require("dap-python").test_method() end) end,
              { buffer = buf, desc = "Python Debug Method" })
            vim.keymap.set("n", "<leader>pydc",
              function() pcall(function() require("dap-python").test_class() end) end,
              { buffer = buf, desc = "Python Debug Class" })
            vim.keymap.set({ "n", "v" }, "<leader>pyds",
              function() pcall(function() require("dap-python").debug_selection() end) end,
              { buffer = buf, desc = "Python Debug Selection" })
          end,
        })

        -- Retroactively apply to already-open Python buffers.
        for _, buf in ipairs(vim.api.nvim_list_bufs()) do
          if vim.bo[buf].filetype == "python"
          and not vim.b[buf].python_dap_keymaps_registered then
            vim.b[buf].python_dap_keymaps_registered = true
            vim.keymap.set("n", "<leader>pydm",
              function() pcall(function() require("dap-python").test_method() end) end,
              { buffer = buf, desc = "Python Debug Method" })
            vim.keymap.set("n", "<leader>pydc",
              function() pcall(function() require("dap-python").test_class() end) end,
              { buffer = buf, desc = "Python Debug Class" })
            vim.keymap.set({ "n", "v" }, "<leader>pyds",
              function() pcall(function() require("dap-python").debug_selection() end) end,
              { buffer = buf, desc = "Python Debug Selection" })
          end
        end
      end

      local python = find_debugpy_python()
      local ok = pcall(function() require("dap-python").setup(python) end)
      if ok then
        register_dap_keymaps()
      else
        vim.notify(
          "[python] nvim-dap-python setup failed for: " .. python,
          vim.log.levels.WARN
        )
      end
    end,
  },

  -- optional=true — extends the primary neogen spec in advanced.lua.
  {
    "danymat/neogen",
    optional = true,
    ft       = "python",
    opts = {
      languages = {
        python = { template = { annotation_convention = "google_docstrings" } },
      },
    },
    keys = {
      {
        "<leader>pyg",
        function() pcall(function() require("neogen").generate() end) end,
        desc = "Python Generate Docstring",
      },
    },
  },

  {
    "Vigemus/iron.nvim",
    ft = "python",
    config = function()
      pcall(function()
        require("iron.core").setup({
          config = {
            scratch_repl    = true,
            repl_definition = {
              python = {
                command = { "ipython", "--no-autoindent" },
                format  = require("iron.fts.common").bracketed_paste,
              },
            },
            repl_open_cmd = function()
              return require("iron.view").bottom(20)
            end,
          },
          -- Keymaps registered per-buffer in FileType autocmd below —
          -- iron.core.setup() keymaps are GLOBAL and would leak to all buffers.
          keymaps = {},
        })
      end)

      local function register_iron_keymaps(buf)
        local function imap(lhs, fn, desc, mode)
          vim.keymap.set(mode or "n", lhs, fn, { buffer = buf, desc = desc })
        end
        imap("<leader>pyrc", function()
          pcall(function() require("iron.core").send_motion(vim.api.nvim_get_current_buf()) end)
        end, "REPL send motion")
        imap("<leader>pyrv", function()
          pcall(function() require("iron.core").visual_send(vim.api.nvim_get_current_buf()) end)
        end, "REPL send visual", "v")
        imap("<leader>pyrl", function()
          pcall(function() require("iron.core").send_line(vim.api.nvim_get_current_buf()) end)
        end, "REPL send line")
        imap("<leader>pyru", function()
          pcall(function() require("iron.core").send_until_cursor(vim.api.nvim_get_current_buf()) end)
        end, "REPL send until cursor")
        imap("<leader>pyrq", function()
          pcall(function() require("iron.core").close_repl(vim.api.nvim_get_current_buf()) end)
        end, "REPL quit")
        imap("<leader>pyrx", function()
          pcall(function() require("iron.core").send(vim.api.nvim_get_current_buf(), string.char(12)) end)
        end, "REPL clear")
      end

      vim.api.nvim_create_autocmd("FileType", {
        pattern  = "python",
        group    = vim.api.nvim_create_augroup("IronPythonKeymaps", { clear = true }),
        callback = function(e) register_iron_keymaps(e.buf) end,
      })

      for _, buf in ipairs(vim.api.nvim_list_bufs()) do
        if vim.bo[buf].filetype == "python" then
          register_iron_keymaps(buf)
        end
      end
    end,
    keys = {
      { "<leader>pyrs", "<cmd>IronRepl<cr>",      desc = "Python REPL Start" },
      { "<leader>pyrr", "<cmd>IronRestart<cr>",   desc = "Python REPL Restart" },
      { "<leader>pyri", "<cmd>IronInterrupt<cr>", desc = "Python REPL Interrupt" },
    },
  },

  { "Vimjas/vim-python-pep8-indent", ft = "python" },
}
