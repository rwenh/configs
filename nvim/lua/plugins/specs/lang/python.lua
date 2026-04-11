-- lua/plugins/specs/lang/python.lua - Python development
--
-- FIX (v2.3.1):
--   • iron.nvim keymaps (send_motion, visual_send, send_line, etc.) were
--     passed via the `keymaps` table in iron.core.setup(). iron registers
--     these as GLOBAL normal/visual mappings at setup() time — not per-buffer.
--     If any non-Python file triggers iron loading (e.g. a .py import opened
--     while editing a .lua file), those keymaps pollute every buffer.
--     Fix: keymaps table removed from iron.core.setup(). Keymaps are now
--     registered per-buffer via a FileType autocmd on "python", and also
--     retroactively applied to any already-open Python buffers.
--     iron's send functions accept a bufnr argument so per-buffer binding works.
--
-- FIX (v2.3.1b):
--   • neogen spec marked optional=true so it extends the primary spec in
--     advanced.lua rather than competing with it.
--   • config() removed from the neogen spec — the primary spec in advanced.lua
--     owns initialisation. This spec only contributes the python language opts
--     and the <leader>pyg keymap.
--   • annotation_convention corrected to "google_docstrings" (the valid neogen
--     identifier); "google" is not a recognised convention string and silently
--     falls back to neogen's default.

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
      local candidates = {
        vim.fn.exepath("python3"),
        vim.fn.exepath("python"),
        "/usr/bin/python3",
        "/usr/bin/python",
      }

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

      local function try_setup(index)
        if index > #candidates then
          vim.notify("debugpy not found. Install with: pip install debugpy",
            vim.log.levels.WARN)
          return
        end

        local python = candidates[index]
        if python == "" then return try_setup(index + 1) end

        vim.system({ python, "-c", "import debugpy" }, {}, function(result)
          vim.schedule(function()
            if result.code == 0 then
              local ok = pcall(function() require("dap-python").setup(python) end)
              if ok then register_dap_keymaps() end
            else
              try_setup(index + 1)
            end
          end)
        end)
      end

      try_setup(1)
    end,
  },

  -- FIX: optional=true — extends the primary neogen spec in advanced.lua.
  -- config() removed; advanced.lua's config() owns setup(). This spec only
  -- contributes the python language opts and the <leader>pyg keymap.
  -- annotation_convention corrected to "google_docstrings" (valid neogen id).
  {
    "danymat/neogen",
    optional = true,
    ft       = "python",
    opts = {
      languages = {
        -- FIX: "google" is not a valid neogen convention id — corrected to
        -- "google_docstrings" which is the identifier neogen recognises.
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
          -- FIX: keymaps table removed from iron.core.setup().
          -- iron registers these as GLOBAL mappings at setup() time regardless
          -- of filetype — they leak into every buffer.
          -- Keymaps are now applied per-buffer in the FileType autocmd below.
          keymaps = {},
        })
      end)

      -- FIX: per-buffer iron keymaps via FileType autocmd.
      -- iron's send functions work fine called from buffer-local mappings.
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

      -- Apply to already-open Python buffers (e.g. opened via CLI)
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
