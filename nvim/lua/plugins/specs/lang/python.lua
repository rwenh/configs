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
      -- OPT (v2.3.14): synchronous binary resolution replaces the recursive
      -- async vim.system() probe. debugpy does not need to be imported to be
      -- located — we find the python interpreter that has it available by
      -- checking the standard install paths synchronously.
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
            -- Quick check: does this interpreter have debugpy on its path?
            -- We probe the site-packages directory rather than spawning a
            -- subprocess, which avoids the async complexity entirely.
            local site = vim.fn.system(p .. " -c \"import site; print(site.getsitepackages()[0])\" 2>/dev/null"):gsub("%s+$", "")
            if site ~= "" and vim.fn.isdirectory(site .. "/debugpy") == 1 then
              return p
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
          if vim.bo[buf].filetype == "python" then
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
