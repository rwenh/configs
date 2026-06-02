-- lua/plugins/specs/workflow.lua — Overseer task runner
--

-- ── Shared win opts ────────────────────────────────────────────────────────

local WIN_OPTS = { border = "rounded", win_opts = { winblend = 5 } }

-- ── Per-project template loader ────────────────────────────────────────────
--
--   return {
--     {
--       name    = "dev server",
--       builder = function() return { cmd = { "npm", "run", "dev" } } end,
--     },
--   }

local _project_templates_loaded = {}

local function load_project_templates()
  local ok_path, path_util = pcall(require, "core.util.path")
  if not ok_path then return end

  local root = path_util.find_root()
  if not root or root == "" then return end
  if _project_templates_loaded[root] then return end

  local tmpl_file = root .. "/.overseer.lua"
  if vim.fn.filereadable(tmpl_file) ~= 1 then
    _project_templates_loaded[root] = true
    return
  end

  local ok_chunk, chunk = pcall(loadfile, tmpl_file)
  if not ok_chunk or type(chunk) ~= "function" then
    vim.notify(
      "[workflow] .overseer.lua parse error in: " .. root,
      vim.log.levels.WARN
    )
    _project_templates_loaded[root] = true
    return
  end

  local ok_run, templates = pcall(chunk)
  if not ok_run or type(templates) ~= "table" then
    _project_templates_loaded[root] = true
    return
  end

  local ok_ov, overseer = pcall(require, "overseer")
  if not ok_ov then return end

  local registered = 0
  for _, tmpl in ipairs(templates) do
    if type(tmpl) == "table" and type(tmpl.name) == "string" then
      local ok_reg = pcall(function() overseer.register_template(tmpl) end)
      if ok_reg then registered = registered + 1 end
    end
  end

  if registered > 0 then
    vim.notify(
      string.format("[workflow] loaded %d project template(s) from .overseer.lua", registered),
      vim.log.levels.INFO
    )
  end

  _project_templates_loaded[root] = true
end

-- ── Inline shell-command runner ────────────────────────────────────────────
--

local function run_shell_command_interactive()
  local ok_ov, overseer = pcall(require, "overseer")
  if not ok_ov then
    vim.notify("[workflow] overseer not loaded", vim.log.levels.WARN)
    return
  end

  vim.ui.input({ prompt = "Shell command: ", completion = "shellcmd" }, function(input)
    if not input or vim.trim(input) == "" then return end

    local ok_path, path_util = pcall(require, "core.util.path")
    local cwd = (ok_path and path_util.find_root()) or vim.fn.getcwd()

    local ok_task = pcall(function()
      overseer.run_template({
        name  = "shell",
        params = {
          cmd = input,
          cwd = cwd,
        },
      })
    end)

    if not ok_task then
      -- Fallback: wrap in a one-shot task directly.
      pcall(function()
        overseer.new_task({
          name    = input,
          cmd     = { "sh", "-c", input },
          cwd     = cwd,
          components = { "default" },
        }):start()
      end)
    end
  end)
end

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
            vim.notify("[overseer] no 'build' template — opening task picker",
              vim.log.levels.INFO)
            vim.cmd("OverseerRun")
          end
        end,
        desc = "Overseer: build",
      },
      {
        "<leader>os",
        function() run_shell_command_interactive() end,
        desc = "Overseer: shell command (prompted)",
      },
    },

    opts = function()
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
        return
      end

      -- Load per-project templates after DirChanged or on first load.
      load_project_templates()
      vim.api.nvim_create_autocmd({ "DirChanged", "BufEnter" }, {
        group    = vim.api.nvim_create_augroup("OverseerProjectTemplates", { clear = true }),
        once     = false,
        callback = function()
          if vim.bo.buftype == "" then
            vim.schedule(load_project_templates)
          end
        end,
        desc = "Load per-project .overseer.lua templates on directory change",
      })
    end,
  },
}
