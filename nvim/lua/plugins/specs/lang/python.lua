-- lua/plugins/specs/lang/python.lua - Python development
--
-- FIX (v2.2.4):
--   • iron repl_open_cmd: require("iron.view").bottom(20) was called eagerly
--     at config() execution time. If iron.nvim hadn't finished loading yet
--     (e.g. first open of a .py file on a slow machine), the require errored
--     inside the pcall, setup() was skipped entirely, and iron silently never
--     configured. Wrapped in a function so evaluation is deferred to the
--     moment iron actually opens a REPL — guaranteed to be after load.
--   • iron send_motion and visual_send: both were bound to <leader>pyrc.
--     visual_send moved to <leader>pyrv (unchanged from v2.2.3 fix).
--   • dap-python async race: PythonDapKeymaps autocmd fires inside the
--     vim.system success callback (unchanged from v2.2.3 fix).

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
              if ok then
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
            -- FIX: iron.view.bottom(20) wrapped in a function so it is
            -- evaluated lazily when iron opens a REPL, not at config time.
            -- Eager evaluation at setup() called require("iron.view") before
            -- the module was guaranteed loaded, causing the entire iron setup
            -- to silently fail inside pcall on the first .py open.
            repl_open_cmd = function()
              return require("iron.view").bottom(20)
            end,
          },
          keymaps = {
            send_motion       = "<leader>pyrc",
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
      { "<leader>pyrs", "<cmd>IronRepl<cr>",      desc = "Python REPL Start" },
      { "<leader>pyrr", "<cmd>IronRestart<cr>",   desc = "Python REPL Restart" },
      { "<leader>pyri", "<cmd>IronInterrupt<cr>", desc = "Python REPL Interrupt" },
    },
  },

  { "Vimjas/vim-python-pep8-indent", ft = "python" },
}
