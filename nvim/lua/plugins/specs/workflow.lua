-- lua/plugins/specs/workflow.lua
--
-- FIX (v2.2.3):
--   • overseer-community-tasks removed — that repo does not exist. All
--     templates (cargo, cmake, go, npm, make, mix, rake, tox, python) are
--     built into overseer.nvim and auto-discovered from the working directory.
--     The `templates` key simply controls which built-in modules are active.
--     No extra dependency is needed.

return {

  {
    "stevearc/overseer.nvim",
    cmd  = { "OverseerRun", "OverseerToggle", "OverseerBuild", "OverseerTaskAction" },
    keys = {
      { "<leader>ot", "<cmd>OverseerToggle<cr>",     desc = "Overseer: task list" },
      { "<leader>or", "<cmd>OverseerRun<cr>",        desc = "Overseer: run task" },
      { "<leader>ob", "<cmd>OverseerBuild<cr>",      desc = "Overseer: build" },
      { "<leader>oa", "<cmd>OverseerTaskAction<cr>", desc = "Overseer: task action" },
      { "<leader>oc", "<cmd>OverseerClearCache<cr>", desc = "Overseer: clear cache" },
      { "<leader>os", function()
          require("overseer").run_template({ name = "shell" })
        end, desc = "Overseer: shell command" },
    },
    opts = {
      strategy = {
        "toggleterm",
        direction     = "float",
        close_on_exit = false,
        open_on_start = true,
      },
      -- All entries are built-in to overseer.nvim — no extra plugin needed.
      -- "builtin" covers: shell, vscode tasks.json, make, npm, cargo, cmake,
      -- go, mix, rake, tox, python, just, mise — auto-discovered per project.
      templates = { "builtin" },
      auto_scroll = true,
      task_list = {
        direction      = "bottom",
        min_height     = 10,
        max_height     = 25,
        default_detail = 1,
        bindings = {
          ["<CR>"]       = "RunAction",
          ["<C-e>"]      = "Edit",
          ["o"]          = "Open",
          ["p"]          = "TogglePreview",
          ["<C-f>"]      = "ScrollOutputDown",
          ["<C-b>"]      = "ScrollOutputUp",
          ["?"]          = "ShowHelp",
          ["<leader>ot"] = "Close",
        },
      },
      form     = { border = "rounded", zindex = 40, min_width = 80,
                   win_opts = { winblend = 5 } },
      confirm  = { border = "rounded", zindex = 40, min_width = 80,
                   win_opts = { winblend = 5 } },
      task_win = { border = "rounded", padding = 2,
                   win_opts = { winblend = 5 } },
      log      = { { type = "echo", level = vim.log.levels.WARN } },
    },
    config = function(_, opts)
      local ok = pcall(function() require("overseer").setup(opts) end)
      if not ok then
        vim.notify("overseer.nvim setup failed", vim.log.levels.WARN)
      end
    end,
  },

  {
    "nvim-neotest/neotest",
    optional = true,
    keys = {
      { "<leader>'w", function()
          pcall(function()
            require("neotest").watch.toggle(vim.fn.expand("%"))
          end)
        end, desc = "Neotest: watch file" },
      { "<leader>'W", function()
          pcall(function() require("neotest").watch.toggle() end)
        end, desc = "Neotest: watch nearest" },
    },
  },
}
