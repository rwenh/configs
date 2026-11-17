-- lua/plugins/specs/workflow.lua — Overseer task runner and project templates
--
-- Project templates from .overseer.lua are registered once per file signature. Each carries a condition
-- that matches the current buffer's project root. They stay registered, but are offered only inside their
-- own project, so another project's tasks never appear. The file still needs `:trust`.

local cache = require("core.util.cache")
local path = require("core.util.path")
local projconf = require("core.util.projconf")

local WIN_OPTS = { border = "rounded", win_opts = { winblend = 5 } }

local _applied = {} -- root -> file signature last registered

--- A copy of `tmpl` that is offered only while the current buffer's project root is `root`.
---@param tmpl table
---@param root string
---@return table
local function scoped(tmpl, root)
  local copy = vim.deepcopy(tmpl)
  local user = copy.condition and copy.condition.callback
  copy.condition = vim.tbl_extend("force", copy.condition or {}, {
    callback = function(search)
      if path.find_root() ~= root then
        return false
      end
      return user == nil or user(search)
    end,
  })
  return copy
end

local function load_project_templates()
  local root = path.find_root()
  if not root or root == "" then
    return
  end

  local sig = cache.file_sig(root .. "/.overseer.lua")
  if _applied[root] == sig then
    return
  end
  _applied[root] = sig

  local res = projconf.load(root, ".overseer.lua")
  if not res.ok then
    if not res.absent then
      vim.notify("[workflow] .overseer.lua not loaded in " .. root .. ": " .. res.reason, vim.log.levels.WARN)
    end
    return
  end

  local ok_ov, overseer = pcall(require, "overseer")
  if not ok_ov then
    return
  end

  local registered = 0
  for _, tmpl in ipairs(res.value) do
    if type(tmpl) == "table" and type(tmpl.name) == "string" then
      if pcall(overseer.register_template, scoped(tmpl, root)) then
        registered = registered + 1
      end
    end
  end
  if registered > 0 then
    vim.notify(
      ("[workflow] %d project template(s) for %s"):format(registered, vim.fn.fnamemodify(root, ":~")),
      vim.log.levels.INFO
    )
  end
end

-- ── Debounced loader ──────────────────────────────────────────────────────────

local DEBOUNCE_MS = (type(vim.g.workflow_template_debounce_ms) == "number" and vim.g.workflow_template_debounce_ms > 0)
    and vim.g.workflow_template_debounce_ms
  or 300

local _timer

local function stop_timer()
  if _timer then
    pcall(function()
      _timer:stop()
      _timer:close()
    end)
    _timer = nil
  end
end

local function schedule_load()
  stop_timer()
  local t = vim.uv.new_timer()
  _timer = t
  t:start(
    DEBOUNCE_MS,
    0,
    vim.schedule_wrap(function()
      if _timer == t then
        stop_timer()
        load_project_templates()
      end
    end)
  )
end

-- ── Inline shell-command runner ───────────────────────────────────────────────

local function run_shell_command_interactive()
  vim.ui.input({ prompt = "Shell command: ", completion = "shellcmd" }, function(input)
    if not input or vim.trim(input) == "" then
      return
    end
    local ok_task, task = pcall(require, "core.util.task")
    if not ok_task then
      vim.notify("[workflow] core.util.task not loaded", vim.log.levels.WARN)
      return
    end
    task.run({ cmd = input, interactive = false })
  end)
end

return {
  {
    "stevearc/overseer.nvim",
    cmd = { "OverseerRun", "OverseerToggle", "OverseerOpen", "OverseerClose", "OverseerShell", "OverseerTaskAction" },
    keys = {
      { "<leader>ot", "<cmd>OverseerToggle<cr>", desc = "Overseer: task list" },
      { "<leader>or", "<cmd>OverseerRun<cr>", desc = "Overseer: run task" },
      { "<leader>oa", "<cmd>OverseerTaskAction<cr>", desc = "Overseer: task action" },
      { "<leader>oc", "<cmd>OverseerClose<cr>", desc = "Overseer: close task list" },
      {
        "<leader>ob",
        function()
          local ok_ov, overseer = pcall(require, "overseer")
          if not ok_ov then
            return
          end

          local function open_picker_fallback()
            vim.notify("[overseer] no 'build' template — opening task picker", vim.log.levels.INFO)
            vim.cmd("OverseerRun")
          end

          local ok = pcall(function()
            overseer.run_template({ name = "build" }, function(task)
              if not task then
                vim.schedule(open_picker_fallback)
              end
            end)
          end)
          if not ok then
            open_picker_fallback()
          end
        end,
        desc = "Overseer: build",
      },
      {
        "<leader>os",
        run_shell_command_interactive,
        desc = "Overseer: shell command (prompted)",
      },
    },

    opts = function()
      return {
        templates = { "builtin" },
        auto_scroll = vim.g.overseer_auto_scroll ~= false,
        task_list = {
          direction = "bottom",
          min_height = 10,
          max_height = 25,
          default_detail = 1,
          bindings = {
            ["<CR>"] = "RunAction",
            ["<C-e>"] = "Edit",
            ["o"] = "Open",
            ["p"] = "TogglePreview",
            ["<C-f>"] = "ScrollOutputDown",
            ["<C-b>"] = "ScrollOutputUp",
            ["?"] = "ShowHelp",
            ["<leader>ot"] = "Close",
          },
        },
        form = vim.tbl_extend("keep", { min_width = 80, zindex = 40 }, WIN_OPTS),
        confirm = vim.tbl_extend("keep", { min_width = 80, zindex = 40 }, WIN_OPTS),
        task_win = vim.tbl_extend("keep", { padding = 2 }, WIN_OPTS),
        log = { { type = "echo", level = vim.log.levels.WARN } },
      }
    end,

    config = function(_, opts)
      local overseer = require("overseer")
      local ok, err = pcall(function()
        overseer.setup(opts)
      end)
      if not ok then
        vim.notify("overseer.nvim setup failed: " .. tostring(err), vim.log.levels.WARN)
        return
      end

      -- Built-in template: run the current file with the runner.
      overseer.register_template({
        name = "run current file",
        builder = function()
          local file = vim.fn.expand("%:p")
          local runner = require("core.util.runner")
          local cmd = runner.resolve_run(file)
          return {
            cmd = cmd,
            name = "Run: " .. vim.fn.fnamemodify(file, ":t"),
            components = { "default" },
          }
        end,
        condition = {
          callback = function()
            local file = vim.fn.expand("%:p")
            if file == "" then
              return false
            end
            local ok_r, runner = pcall(require, "core.util.runner")
            if not ok_r then
              return false
            end
            local cmd = runner.resolve_run(file)
            return cmd ~= nil
          end,
        },
      })

      load_project_templates()

      vim.api.nvim_create_autocmd("DirChanged", {
        group = vim.api.nvim_create_augroup("OverseerDirChanged", { clear = true }),
        callback = schedule_load,
      })

      vim.api.nvim_create_autocmd("BufEnter", {
        group = vim.api.nvim_create_augroup("OverseerBufEnter", { clear = true }),
        callback = function()
          if vim.bo.buftype == "" then
            schedule_load()
          end
        end,
      })

      vim.api.nvim_create_autocmd("VimLeavePre", {
        group = vim.api.nvim_create_augroup("OverseerTimerCleanup", { clear = true }),
        once = true,
        callback = stop_timer,
      })
    end,
  },
}
