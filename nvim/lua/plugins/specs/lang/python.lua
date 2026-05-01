-- lua/plugins/specs/lang/python.lua — Python language support
--
-- LSP:    basedpyright via lsp.lua
-- Format: black + isort via lsp.lua conform
-- Lint:   ruff via lsp.lua nvim-lint
-- DAP:    nvim-dap-python (this file)
-- REPL:   iron.nvim (this file)
-- Venv:   venv-selector.nvim (this file)
-- Docs:   neogen optional spec (this file)
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
        -- Re-initialise nvim-dap-python with the new interpreter.
        local ok_dpy, dpy = pcall(require, "dap-python")
        if ok_dpy then
          local python = require("core.util.path") and
            -- Re-probe with updated VIRTUAL_ENV env var.
            (os.getenv("VIRTUAL_ENV") and os.getenv("VIRTUAL_ENV") .. "/bin/python")
            or vim.fn.exepath("python3")
          if python and python ~= "" then
            pcall(dpy.setup, python)
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

      -- ── Debugpy probe strategies ────────────────────────────────────────

      -- Strategy 1: active virtual environment.
      local function check_venv()
        local venv = os.getenv("VIRTUAL_ENV")
        if not venv then return nil end
        local p = venv .. "/bin/python"
        return vim.fn.executable(p) == 1 and p or nil
      end

      -- Strategy 2: PATH candidates + debugpy presence check.
      local function check_path_candidates()
        local candidates = {}
        for _, name in ipairs({ "python3", "python" }) do
          local p = vim.fn.exepath(name)
          if p ~= "" then
            if p:find("shims", 1, true) then
              -- Try pyenv root resolution.
              local pyenv_root = os.getenv("PYENV_ROOT")
              if pyenv_root then
                local ver_file = vim.fn.findfile(".python-version", ".;")
                if ver_file ~= "" then
                  local ver = vim.fn.readfile(ver_file)[1]
                  if ver and ver ~= "" then
                    local resolved = pyenv_root
                      .. "/versions/" .. ver:gsub("%s+", "")
                      .. "/bin/python3"
                    if vim.fn.executable(resolved) == 1 then
                      table.insert(candidates, resolved)
                    end
                  end
                end
              end
            else
              table.insert(candidates, p)
            end
          end
        end
        -- Append common system paths as fallback.
        vim.list_extend(candidates, { "/usr/bin/python3", "/usr/bin/python" })

        for _, p in ipairs(candidates) do
          if vim.fn.executable(p) == 1 then
            local prefix = vim.fn.fnamemodify(vim.fn.fnamemodify(p, ":h"), ":h")
            if vim.fn.executable(prefix .. "/bin/debugpy") == 1 then return p end
            if vim.fn.glob(prefix .. "/lib/python*/site-packages/debugpy") ~= "" then
              return p
            end
          end
        end
        return nil
      end

      -- Strategy 3: user site-packages.
      local function check_user_sitepackages()
        local home = os.getenv("HOME") or ""
        if home == "" then return nil end
        local pattern = home .. "/.local/lib/python*/site-packages/debugpy"
        if vim.fn.glob(pattern) ~= "" then
          return vim.fn.exepath("python3") ~= "" and vim.fn.exepath("python3") or nil
        end
        return nil
      end

      local function find_debugpy_python()
        return check_venv()
          or check_path_candidates()
          or check_user_sitepackages()
          or (function()
            vim.schedule(function()
              vim.notify(
                "[python] debugpy not found in any interpreter.\n"
                .. "Install with: pip install debugpy",
                vim.log.levels.WARN
              )
            end)
            return vim.fn.exepath("python3") ~= "" and vim.fn.exepath("python3") or "python3"
          end)()
      end

      -- ── DAP keymaps ─────────────────────────────────────────────────────

      local DAP_MAPS = {
        { "n",        "<leader>pydm", function()
            pcall(function() require("dap-python").test_method() end)
          end, "Python Debug Method" },
        { "n",        "<leader>pydc", function()
            pcall(function() require("dap-python").test_class() end)
          end, "Python Debug Class" },
        { {"n","v"},  "<leader>pyds", function()
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
          keymaps = {},   -- registered per-buffer below
        })
      end)

      local bkm = require("core.util.buf_keymap")

      local IRON_MAPS = {
        { "n", "<leader>pyrc", function()
            local buf = vim.api.nvim_get_current_buf()
            vim.opt.operatorfunc = "v:lua.require'iron.core'.send_motion"
            vim.api.nvim_feedkeys("g@", "n", false)
            _ = buf  -- suppress unused warning
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
  -- If vim-python-pep8-indent is removed, re-enable treesitter Python indent.
  { "Vimjas/vim-python-pep8-indent", ft = "python" },
}
