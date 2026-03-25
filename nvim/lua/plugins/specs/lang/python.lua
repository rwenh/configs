-- lua/plugins/specs/lang/python.lua - Python development
-- All Python keymaps use <leader>py* prefix consistently.

return {
  -- Virtual environment selector
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

  -- DAP: debugpy
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

      local function try_setup(index)
        if index > #candidates then
          vim.notify("debugpy not found. Install with: pip install debugpy", vim.log.levels.WARN)
          return
        end
        local python = candidates[index]
        if python == "" then return try_setup(index + 1) end
        vim.system({ python, "-c", "import debugpy" }, {}, function(result)
          vim.schedule(function()
            if result.code == 0 then
              require("dap-python").setup(python)
            else
              try_setup(index + 1)
            end
          end)
        end)
      end

      try_setup(1)

      -- FIX #6: Keymaps scoped to Python buffers via FileType autocmd.
      -- The original set them globally — active in every buffer type.
      vim.api.nvim_create_autocmd("FileType", {
        pattern  = "python",
        group    = vim.api.nvim_create_augroup("PythonDapKeymaps", { clear = true }),
        callback = function(e)
          local buf = e.buf
          vim.keymap.set("n", "<leader>pydm",
            function() require("dap-python").test_method() end,
            { buffer = buf, desc = "Python Debug Method" })
          vim.keymap.set("n", "<leader>pydc",
            function() require("dap-python").test_class() end,
            { buffer = buf, desc = "Python Debug Class" })
          vim.keymap.set({ "n", "v" }, "<leader>pyds",
            function() require("dap-python").debug_selection() end,
            { buffer = buf, desc = "Python Debug Selection" })
        end,
      })
    end,
  },

  -- Docstring generator
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
      -- FIX #8: Renamed <leader>pyd → <leader>pyg (generate) to avoid being
      -- a prefix of <leader>pydm/pydc/pyds (debug subgroup). which-key would
      -- wait for a third key after pyd, making the docstring binding unreachable
      -- without timeout.
      { "<leader>pyg", function() require("neogen").generate() end, desc = "Python Generate Docstring" },
    },
  },

  -- REPL via iron.nvim (ipython)
  {
    "Vigemus/iron.nvim",
    ft = "python",
    config = function()
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
          send_motion  = "<leader>pyrc",
          visual_send  = "<leader>pyrc",
          send_line    = "<leader>pyrl",
          -- FIX #9: Removed cr = "<leader>pyr<cr>" — iron's cr keymap expects
          -- a simple key (e.g. "<CR>") not a compound leader sequence. The
          -- string was syntactically invalid as a keymap.
          interrupt    = "<leader>pyri",
          exit         = "<leader>pyrq",
          clear        = "<leader>pyrx",
        },
      })
    end,
    keys = {
      { "<leader>pyrs", "<cmd>IronRepl<cr>",    desc = "Python REPL Start" },
      { "<leader>pyrr", "<cmd>IronRestart<cr>", desc = "Python REPL Restart" },
    },
  },

  -- Better indentation
  { "Vimjas/vim-python-pep8-indent", ft = "python" },

  -- NOTE (Fix #10): Neotest optional spec removed — it was a no-op that only
  -- set opts.adapters to itself. test.lua handles neotest-python registration
  -- centrally. No entry needed here.

}
