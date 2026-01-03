-- lua/plugins/specs/lang/python.lua - Python development

return {
  -- Virtual environment selector
  {
    "linux-cultist/venv-selector.nvim",
    ft = "python",
    cmd = "VenvSelect",
    opts = {
      name = { "venv", ". venv", "env", ". env" },
    },
    keys = {
      { "<leader>vs", "<cmd>VenvSelect<cr>", desc = "Select VirtualEnv", ft = "python" },
    },
  },

  -- Python-specific DAP configuration
  {
    "mfussenegger/nvim-dap-python",
    ft = "python",
    dependencies = "mfussenegger/nvim-dap",
    config = function()
      local dap_python = require("dap-python")
      -- Try to find python in common locations
      local python_path = vim.fn.exepath("python3") or vim.fn.exepath("python")
      if python_path ~= "" then
        dap_python. setup(python_path)
      end
      
      -- Add keymaps
      vim.keymap.set("n", "<leader>dpr", function() dap_python.test_method() end, { desc = "Debug Python Method" })
      vim.keymap.set("n", "<leader>dpc", function() dap_python.test_class() end, { desc = "Debug Python Class" })
    end,
  },

  -- Docstring generator
  {
    "danymat/neogen",
    ft = "python",
    dependencies = "nvim-treesitter/nvim-treesitter",
    opts = {
      languages = {
        python = {
          template = {
            annotation_convention = "google_docstrings",
          },
        },
      },
    },
    keys = {
      { "<leader>nf", function() require("neogen"). generate() end, desc = "Generate Docstring", ft = "python" },
    },
  },

  -- Python REPL
  {
    "Vigemus/iron.nvim",
    ft = "python",
    config = function()
      local iron = require("iron.core")
      iron.setup({
        config = {
          scratch_repl = true,
          repl_definition = {
            python = {
              command = { "ipython", "--no-autoindent" },
              format = require("iron.fts.common"). bracketed_paste,
            },
          },
          repl_open_cmd = require("iron.view").bottom(20),
        },
        keymaps = {
          send_motion = "<space>rc",
          visual_send = "<space>rc",
          send_line = "<space>rl",
          cr = "<space>r<cr>",
          interrupt = "<space>r<space>",
          exit = "<space>rq",
          clear = "<space>rx",
        },
      })
    end,
    keys = {
      { "<leader>rs", "<cmd>IronRepl<cr>", desc = "Toggle REPL", ft = "python" },
      { "<leader>rr", "<cmd>IronRestart<cr>", desc = "Restart REPL", ft = "python" },
    },
  },

  -- Better indentation handling
  {
    "Vimjas/vim-python-pep8-indent",
    ft = "python",
  },
}
