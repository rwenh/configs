-- lua/plugins/specs/lang/python.lua - Python development

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
      { "<leader>pyv", "<cmd>VenvSelect<cr>", desc = "Select VirtualEnv" },
    },
  },

  -- Python debug adapter
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

      -- Non-blocking debugpy check via vim.system (Nvim 0.10+)
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

      -- Python-specific debug keymaps
      vim.keymap.set("n", "<leader>;pm", function() require("dap-python").test_method() end,    { desc = "Debug Python Method" })
      vim.keymap.set("n", "<leader>;pc", function() require("dap-python").test_class() end,     { desc = "Debug Python Class" })
      vim.keymap.set({ "n", "v" }, "<leader>;ps", function() require("dap-python").debug_selection() end, { desc = "Debug Selection" })
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
      { "<leader>pyd", function() require("neogen").generate() end, desc = "Generate Docstring" },
    },
  },

  -- Python REPL (ipython via iron.nvim)
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
          send_motion  = "<leader>\\pc",
          visual_send  = "<leader>\\pc",
          send_line    = "<leader>\\pl",
          cr           = "<leader>\\p<cr>",
          interrupt    = "<leader>\\pi",
          exit         = "<leader>\\pq",
          clear        = "<leader>\\px",
        },
      })
    end,
    keys = {
      { "<leader>\\ps", "<cmd>IronRepl<cr>",    desc = "Python REPL Start" },
      { "<leader>\\pr", "<cmd>IronRestart<cr>", desc = "Python REPL Restart" },
    },
  },

  -- Better Python indentation
  { "Vimjas/vim-python-pep8-indent", ft = "python" },
}
