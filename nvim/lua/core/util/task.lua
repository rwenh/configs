-- lua/core/util/task.lua

local path = require("core.util.path")

local M = {}

local STRATEGY = {
  interactive = { "toggleterm", direction = "float", open_on_start = true, close_on_exit = false },
  batch = "jobstart",
}

---@param opts { cmd: string|table, cwd: string?, name: string?, interactive: boolean?, on_done: function? }
---@return table|nil task
---@return string?   reason
function M.run(opts)
  opts = opts or {}
  if not opts.cmd or (type(opts.cmd) == "string" and opts.cmd == "") or (type(opts.cmd) == "table" and #opts.cmd == 0) then
    return nil, "task.run: cmd must be a non-empty string or table"
  end

  local ok_ov, overseer = pcall(require, "overseer")
  if not ok_ov then
    local why = "overseer.nvim is not loadable (run :Lazy)"
    vim.notify("[task] " .. why, vim.log.levels.ERROR)
    return nil, why
  end

  local name = opts.name
  if not name then
    name = type(opts.cmd) == "table" and table.concat(opts.cmd, " ") or opts.cmd
  end

  local ok_new, t = pcall(overseer.new_task, {
    name = name,
    cmd = opts.cmd,
    cwd = opts.cwd or path.find_root() or vim.fn.getcwd(),
    strategy = opts.interactive and STRATEGY.interactive or STRATEGY.batch,
    components = { "default" },
  })
  if not ok_new then
    vim.notify("[task] could not create task", vim.log.levels.ERROR)
    return nil, "creation failed"
  end

  -- Hook the on_done callback if provided
  if type(opts.on_done) == "function" then
    t:subscribe("on_complete", function(status, result)
      -- Optional: vim.schedule ensures it runs safely in Neovim's main event loop
      vim.schedule(function()
        opts.on_done(status, result)
      end)
    end)
  end

  local ok_start, err = pcall(t.start, t)
  if not ok_start then
    vim.notify("[task] could not start task: " .. tostring(err), vim.log.levels.ERROR)
    return nil, tostring(err)
  end

  if not opts.interactive then
    pcall(overseer.open, { enter = false })
  end
  return t
end

return M
