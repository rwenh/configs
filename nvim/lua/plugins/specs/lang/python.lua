-- lua/plugins/specs/lang/python.lua - Python development
--
-- FIX (v2.2.3):
--   • iron send_motion and visual_send: both were bound to <leader>pyrc.
--     iron uses separate keys for motion vs visual; binding both to the same
--     key meant visual_send in normal mode was a silent no-op that polluted
--     the keymap. Fixed: visual_send → <leader>pyrv.
--   • dap-python async race: the FileType autocmd for PythonDapKeymaps fired
--     immediately on FileType, but dap-python setup() is async (vim.system
--     callback). On slow machines keymaps registered before the adapter was
--     ready. Fixed: PythonDapKeymaps autocmd now fires inside the vim.system
--     success callback, guaranteeing adapter is set up first.

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

      -- FIX: register DAP keymaps INSIDE the success callback so the adapter
      -- is guaranteed to exist before any keymap fires.
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

        -- Apply keymaps to any already-open python buffers
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
              if ok then
                -- FIX: keymaps registered only after adapter is confirmed ready
                register_dap_keymaps()
              end
            else
              try_setup(index + 1)
            end
          end)
        end)
      end

      try_setup(1)
    end,
  },

  {
    "danymat/neogen",
    ft           = "python",
    dependencies = "nvim-treesitter/nvim-treesitter",
    opts = {
      languages = {
        python = { template = { annotation_convention = "google_docstrings" } },
      },
    },
    keys = {
      { "<leader>pyg",
        function() pcall(function() require("neogen").generate() end) end,
        desc = "Python Generate Docstring" },
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
            repl_open_cmd = require("iron.view").bottom(20),
          },
          keymaps = {
            send_motion       = "<leader>pyrc",
            -- FIX: visual_send moved off <leader>pyrc to avoid collision.
            -- Both pointing to the same key caused visual_send in normal
            -- mode to shadow send_motion silently.
            visual_send       = "<leader>pyrv",
            send_line         = "<leader>pyrl",
            send_until_cursor = "<leader>pyru",
            exit              = "<leader>pyrq",
            clear             = "<leader>pyrx",
          },
        })
      end)
    end,
    keys = {
      { "<leader>pyrs", "<cmd>IronRepl<cr>",    desc = "Python REPL Start" },
      { "<leader>pyrr", "<cmd>IronRestart<cr>", desc = "Python REPL Restart" },
      { "<leader>pyri", "<cmd>IronInterrupt<cr>", desc = "Python REPL Interrupt" },
    },
  },

  { "Vimjas/vim-python-pep8-indent", ft = "python" },
}
