-- lua/plugins/specs/lang/python.lua - Python development (SAFE KEYMAPS)

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
      { "<leader>pyv", "<cmd>VenvSelect<cr>", desc = "Select VirtualEnv", ft = "python" },
    },
  },

  -- Python debugpy installation helper
  {
    "mfussenegger/nvim-dap-python",
    ft = "python",
    dependencies = "mfussenegger/nvim-dap",
    config = function()
      local python_path = vim.fn.exepath("python3") or vim.fn.exepath("python")
      
      if python_path ~= "" then
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

      -- Python-specific debug keymaps (using ;py prefix)
      vim.keymap.set("n", "<leader>;pm", function()
        require("dap-python").test_method()
      end, { desc = "Debug Python Test Method" })
      
      vim.keymap.set("n", "<leader>;pc", function()
        require("dap-python").test_class()
      end, { desc = "Debug Python Test Class" })
      
      vim.keymap.set({ "n", "v" }, "<leader>;ps", function()
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
      { "<leader>pyd", function() require("neogen").generate() end, desc = "Generate Docstring", ft = "python" },
    },
  },

  -- Python REPL (using \py for REPL operations - backslash is safe)
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
          send_motion = "<leader>\\pc",
          visual_send = "<leader>\\pc",
          send_line = "<leader>\\pl",
          cr = "<leader>\\p<cr>",
          interrupt = "<leader>\\pi",
          exit = "<leader>\\pq",
          clear = "<leader>\\px",
        },
      })
    end,
    keys = {
      { "<leader>\\ps", "<cmd>IronRepl<cr>", desc = "Python REPL Start", ft = "python" },
      { "<leader>\\pr", "<cmd>IronRestart<cr>", desc = "Python REPL Restart", ft = "python" },
    },
  },

  -- Better indentation handling
  {
    "Vimjas/vim-python-pep8-indent",
    ft = "python",
  },
}
