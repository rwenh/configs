-- lua/plugins/specs/lang/python.lua - Python development

return {
  -- Virtual environment selector
  {
    "linux-cultist/venv-selector.nvim",
    ft = "python",
    cmd = "VenvSelect",
    opts = {
      name = { "venv", ".venv", "env", ".env" },
      auto_refresh = true,
    },
    keys = {
      { "<leader>vs", "<cmd>VenvSelect<cr>", desc = "Select VirtualEnv", ft = "python" },
    },
  },

  -- Python debugpy installation helper
  {
    "mfussenegger/nvim-dap-python",
    ft = "python",
    dependencies = "mfussenegger/nvim-dap",
    config = function()
      -- Ensure debugpy is available
      local python_path = vim.fn.exepath("python3") or vim.fn.exepath("python")
      
      if python_path ~= "" then
        -- Check if debugpy is installed
        local handle = io.popen(python_path .. " -c 'import debugpy' 2>&1")
        local result = handle:read("*a")
        handle:close()
        
        if result:match("ModuleNotFoundError") or result:match("No module named") then
          vim.notify(
            "debugpy not found. Install with: pip install debugpy",
            vim.log.levels.WARN
          )
        end
      end

      -- Additional Python-specific debug keymaps
      vim.keymap.set("n", "<leader>dpm", function()
        require("dap-python").test_method()
      end, { desc = "Debug Python Test Method" })
      
      vim.keymap.set("n", "<leader>dpc", function()
        require("dap-python").test_class()
      end, { desc = "Debug Python Test Class" })
      
      vim.keymap.set({ "n", "v" }, "<leader>dps", function()
        require("dap-python").debug_selection()
      end, { desc = "Debug Selection" })
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
      { "<leader>nf", function() require("neogen").generate() end, desc = "Generate Docstring", ft = "python" },
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
              format = require("iron.fts.common").bracketed_paste,
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