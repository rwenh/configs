-- lua/plugins/specs/lang/python.lua — Python language support
--

return {
  -- ── Venv selector ──────────────────────────────────────────────────────────
  {
    "linux-cultist/venv-selector.nvim",
    ft  = "python",
    cmd = "VenvSelect",
    opts = {
      name         = { "venv", ".venv", "env", ".env" },
      auto_refresh = vim.g.python_venv_auto_refresh == true,
    },
    keys = {
      { "<leader>pyv", "<cmd>VenvSelect<cr>", desc = "Python Select Venv" },
    },
    config = function(_, opts)
      local ok, vs = pcall(require, "venv-selector")
      if not ok then return end

      opts.post_set_venv = function()
        -- Re-initialise nvim-dap-python with the interpreter from the new venv.
        local ok_dpy, dpy = pcall(require, "dap-python")
        if ok_dpy then
          local venv   = os.getenv("VIRTUAL_ENV")
          local python = (venv and venv ~= "") and (venv .. "/bin/python")
                         or vim.fn.exepath("python3")
          if python and python ~= "" then
            local ok_setup, err = pcall(dpy.setup, python)
            if not ok_setup then
              vim.notify(
                "[python] dap-python re-init failed after venv switch.\n"
                .. "Interpreter: " .. python .. "\n"
                .. "Reason: " .. tostring(err) .. "\n"
                .. "Debug sessions in this venv may not work.\n"
                .. "Fix: pip install debugpy  (inside the selected venv)",
                vim.log.levels.WARN
              )
            end
          end
        end
      end

      vs.setup(opts)
    end,
  },

  -- ── DAP: nvim-dap-python ───────────────────────────────────────────────────
  {
    "mfussenegger/nvim-dap-python",
    ft           = "python",
    dependencies = "mfussenegger/nvim-dap",
    config = function()
      local bkm = require("core.util.buf_keymap")

      -- ── Debugpy interpreter resolution ───────────────────────────────────
      --
      -- Auto-detection precedence (first match wins):
      --   0. vim.g.debugpy_python        — explicit escape hatch
      --   1. VIRTUAL_ENV env var          — active venv always wins
      --   2. Non-shim PATH python         — direct system or conda python with debugpy
      --   3. Pyenv version file           — resolves via .python-version walking upward
      --   4. User site-packages           — pip3 install --user debugpy scenario
      --   5. Any executable python        — last resort; warns that debugpy is missing

      local function find_debugpy_python()
        -- 0. Escape hatch — explicit override via vim.g.
        if type(vim.g.debugpy_python) == "string" and vim.g.debugpy_python ~= "" then
          local override = vim.g.debugpy_python
          if vim.fn.executable(override) == 1 then return override end
          vim.notify(
            "[python] vim.g.debugpy_python = '" .. override
            .. "' is not executable — falling through to auto-detection.",
            vim.log.levels.WARN
          )
        end

        -- 1. Active virtualenv — unconditional priority.
        local venv = os.getenv("VIRTUAL_ENV")
        if venv and venv ~= "" then
          local p = venv .. "/bin/python"
          if vim.fn.executable(p) == 1 then return p end
        end

        -- 2. Non-shim PATH python with debugpy present.
        for _, name in ipairs({ "python3", "python" }) do
          local p = vim.fn.exepath(name)
          if p ~= "" and not p:find("shims", 1, true) then
            local prefix = vim.fn.fnamemodify(vim.fn.fnamemodify(p, ":h"), ":h")
            if vim.fn.executable(prefix .. "/bin/debugpy") == 1
            or vim.fn.glob(prefix .. "/lib/python*/site-packages/debugpy") ~= "" then
              return p
            end
          end
        end

        -- 3. Pyenv: resolve via .python-version walking from cwd upward.
        local pyenv_root = os.getenv("PYENV_ROOT")
        if pyenv_root and pyenv_root ~= "" then
          local ver_file = vim.fn.findfile(".python-version", vim.fn.getcwd() .. ";")
          if ver_file ~= "" then
            local lines = vim.fn.readfile(ver_file)
            local ver   = lines[1] and vim.trim(lines[1]) or ""
            if ver ~= "" then
              local resolved = pyenv_root .. "/versions/" .. ver .. "/bin/python3"
              if vim.fn.executable(resolved) == 1 then return resolved end
            end
          end
        end

        -- 4. User site-packages debugpy (pip install --user debugpy).
        local home = os.getenv("HOME") or ""
        if home ~= ""
        and vim.fn.glob(home .. "/.local/lib/python*/site-packages/debugpy") ~= "" then
          local p = vim.fn.exepath("python3")
          if p ~= "" then return p end
        end

        -- 5. Last resort — any python; warn about missing debugpy.
        local fallback = vim.fn.exepath("python3") ~= ""
          and vim.fn.exepath("python3") or "python3"
        vim.schedule(function()
          vim.notify(
            "[python] debugpy not found in any interpreter.\n"
            .. "Install with: pip install debugpy\n"
            .. "Or pin an interpreter: vim.g.debugpy_python = '/path/to/python'",
            vim.log.levels.WARN
          )
        end)
        return fallback
      end

      -- ── DAP keymaps ───────────────────────────────────────────────────────

      local DAP_MAPS = {
        { "n",       "<leader>pydm", function()
            pcall(function() require("dap-python").test_method() end)
          end, "Python Debug Method" },
        { "n",       "<leader>pydc", function()
            pcall(function() require("dap-python").test_class() end)
          end, "Python Debug Class" },
        { {"n","v"}, "<leader>pyds", function()
            pcall(function() require("dap-python").debug_selection() end)
          end, "Python Debug Selection" },
      }

      bkm.on_ft("python", DAP_MAPS, "python_dap_keymaps_registered")

      local python = find_debugpy_python()
      local ok = pcall(function() require("dap-python").setup(python) end)
      if not ok then
        vim.notify(
          "[python] nvim-dap-python setup failed for: " .. python,
          vim.log.levels.WARN
        )
      end
    end,
  },

  -- ── Neogen docstrings ──────────────────────────────────────────────────────

  {
    "danymat/neogen",
    optional = true,
    ft       = "python",
    opts = function(_, opts)
      opts.languages = opts.languages or {}
      local style = vim.g.python_docstring_style or "google_docstrings"
      opts.languages.python = { template = { annotation_convention = style } }
    end,
    keys = {
      {
        "<leader>pyg",
        function() pcall(function() require("neogen").generate() end) end,
        desc = "Python Generate Docstring",
      },
    },
  },

  -- ── iron.nvim REPL ─────────────────────────────────────────────────────────
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
                command = vim.fn.executable("ipython") == 1
                  and { "ipython", "--no-autoindent" }
                  or  { "python3" },
                format = require("iron.fts.common").bracketed_paste,
              },
            },
            repl_open_cmd = function()
              return require("iron.view").bottom(20)
            end,
          },
          keymaps = {},
        })
      end)

      local bkm = require("core.util.buf_keymap")

      -- ── REPL keymaps ─────────────────────────────────────────────────────

      local IRON_MAPS = {
        { "n", "<leader>pyrc", function()
            local prev_opfunc = vim.o.operatorfunc
            vim.o.operatorfunc = "v:lua.require'iron.core'.send_motion"
            vim.api.nvim_feedkeys("g@", "n", false)
            vim.api.nvim_create_autocmd("ModeChanged", {
              pattern  = "no:*",
              once     = true,
              group    = vim.api.nvim_create_augroup(
                           "IronOpFuncRestore", { clear = true }),
              callback = function()
                vim.o.operatorfunc = prev_opfunc
              end,
              desc = "Restore operatorfunc after iron send_motion",
            })
          end, "REPL send motion (operator)" },

        { "v", "<leader>pyrv", function()
            pcall(function()
              require("iron.core").visual_send(vim.api.nvim_get_current_buf())
            end)
          end, "REPL send visual" },
        { "n", "<leader>pyrl", function()
            pcall(function()
              require("iron.core").send_line(vim.api.nvim_get_current_buf())
            end)
          end, "REPL send line" },
        { "n", "<leader>pyru", function()
            pcall(function()
              require("iron.core").send_until_cursor(vim.api.nvim_get_current_buf())
            end)
          end, "REPL send until cursor" },
        { "n", "<leader>pyrq", function()
            pcall(function()
              require("iron.core").close_repl(vim.api.nvim_get_current_buf())
            end)
          end, "REPL quit" },
        { "n", "<leader>pyrx", function()
            pcall(function()
              require("iron.core").send(
                vim.api.nvim_get_current_buf(), string.char(12))
            end)
          end, "REPL clear" },
      }

      bkm.on_ft("python", IRON_MAPS, "python_iron_keymaps_registered")
    end,
    keys = {
      { "<leader>pyrs", "<cmd>IronRepl<cr>",      desc = "Python REPL Start"     },
      { "<leader>pyrr", "<cmd>IronRestart<cr>",   desc = "Python REPL Restart"   },
      { "<leader>pyri", "<cmd>IronInterrupt<cr>", desc = "Python REPL Interrupt"  },
    },
  },

  -- ── PEP8 indent ────────────────────────────────────────────────────────────

  { "Vimjas/vim-python-pep8-indent", ft = "python" },
}
