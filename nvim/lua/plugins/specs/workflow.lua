-- lua/plugins/specs/workflow.lua
--
-- FIX (v2.3.1):
--   • overseer.run_template({ name = "build" }) throws an unhandled error when
--     no template matching "build" exists in the current project (e.g. a plain
--     Python project with no Makefile or CMakeLists.txt). The error propagated
--     to the user as a red stacktrace. Fixed: wrapped in pcall; on failure
--     falls back to OverseerRun (the full picker) with a brief notify so the
--     user understands why the direct build shortcut didn't fire.
--     Same fix applied to the shell template shortcut for consistency.

return {

  {
    "stevearc/overseer.nvim",
    cmd  = { "OverseerRun", "OverseerToggle", "OverseerTaskAction", "OverseerClearCache" },
    keys = {
      { "<leader>ot", "<cmd>OverseerToggle<cr>",     desc = "Overseer: task list" },
      { "<leader>or", "<cmd>OverseerRun<cr>",        desc = "Overseer: run task" },
      {
        "<leader>ob",
        function()
          -- FIX: pcall + fallback — run_template throws if no "build" template
          -- matches the current project. Fall back to the full picker.
          local ok = pcall(function()
            require("overseer").run_template({ name = "build" })
          end)
          if not ok then
            vim.notify("[overseer] No 'build' template found — opening task picker",
              vim.log.levels.INFO)
            vim.cmd("OverseerRun")
          end
        end,
        desc = "Overseer: build",
      },
      { "<leader>oa", "<cmd>OverseerTaskAction<cr>", desc = "Overseer: task action" },
      { "<leader>oc", "<cmd>OverseerClearCache<cr>", desc = "Overseer: clear cache" },
      {
        "<leader>os",
        function()
          -- FIX: same pcall guard for shell template
          local ok = pcall(function()
            require("overseer").run_template({ name = "shell" })
          end)
          if not ok then
            vim.notify("[overseer] No 'shell' template found — opening task picker",
              vim.log.levels.INFO)
            vim.cmd("OverseerRun")
          end
        end,
        desc = "Overseer: shell command",
      },
    },
    opts = {
      strategy = {
        "toggleterm",
        direction     = "float",
        close_on_exit = false,
        open_on_start = true,
      },
      templates  = { "builtin" },
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
