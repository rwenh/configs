-- lua/plugins/specs/workflow.lua — Overseer task runner
--

-- ── Shared win opts ────────────────────────────────────────────────────────

local WIN_OPTS = { border = "rounded", win_opts = { winblend = 5 } }

return {
  {
    "stevearc/overseer.nvim",
    cmd  = { "OverseerRun", "OverseerToggle", "OverseerTaskAction", "OverseerClearCache" },
    keys = {
      { "<leader>ot", "<cmd>OverseerToggle<cr>",     desc = "Overseer: task list"   },
      { "<leader>or", "<cmd>OverseerRun<cr>",        desc = "Overseer: run task"    },
      { "<leader>oa", "<cmd>OverseerTaskAction<cr>", desc = "Overseer: task action" },
      { "<leader>oc", "<cmd>OverseerClearCache<cr>", desc = "Overseer: clear cache" },
      {
        "<leader>ob",
        function()
          local ok_ov, overseer = pcall(require, "overseer")
          if not ok_ov then
            vim.notify("[overseer] plugin not loaded", vim.log.levels.WARN)
            return
          end
          local ok = pcall(function() overseer.run_template({ name = "build" }) end)
          if not ok then
            vim.notify("[overseer] No 'build' template — opening task picker",
              vim.log.levels.INFO)
            vim.cmd("OverseerRun")
          end
        end,
        desc = "Overseer: build",
      },
      {
        "<leader>os",
        function()
          local ok_ov, overseer = pcall(require, "overseer")
          if not ok_ov then
            vim.notify("[overseer] plugin not loaded", vim.log.levels.WARN)
            return
          end
          local ok = pcall(function() overseer.run_template({ name = "shell" }) end)
          if not ok then
            vim.notify("[overseer] No 'shell' template — opening task picker",
              vim.log.levels.INFO)
            vim.cmd("OverseerRun")
          end
        end,
        desc = "Overseer: shell command",
      },
    },
    opts = function()
      -- Evaluated at setup time (not spec-parse time), so toggleterm is
      -- guaranteed to have been loaded if it was triggered before overseer.
      local strategy_name = (pcall(require, "toggleterm")) and "toggleterm" or "terminal"
      return {
        strategy = {
          strategy_name,
          direction     = "float",
          close_on_exit = false,
          open_on_start = true,
        },

        templates   = { "builtin" },

        auto_scroll = vim.g.overseer_auto_scroll ~= false,

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

        form     = vim.tbl_extend("keep", { min_width = 80, zindex = 40 }, WIN_OPTS),
        confirm  = vim.tbl_extend("keep", { min_width = 80, zindex = 40 }, WIN_OPTS),
        task_win = vim.tbl_extend("keep", { padding = 2               }, WIN_OPTS),

        log = { { type = "echo", level = vim.log.levels.WARN } },
      }
    end,
    config = function(_, opts)
      local ok, err = pcall(function() require("overseer").setup(opts) end)
      if not ok then
        vim.notify(
          "overseer.nvim setup failed: " .. tostring(err)
          .. "\nRun :Lazy update overseer.nvim",
          vim.log.levels.WARN
        )
      end
    end,
  },

  -- ── Neotest watch keys ─────────────────────────────────────────────────────
  -- (Keys are registered in test.lua keys= to keep all neotest keys together.)
}
