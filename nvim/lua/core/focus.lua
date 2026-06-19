-- lua/core/focus.lua — deep focus mode
--

local M = {}

local SPEC = {
  { "o",  "laststatus",     0,     3       },
  { "o",  "showtabline",    0,     1       },
  { "o",  "ruler",          false, false   },
  { "wo", "signcolumn",     "no",  "yes:1" },
  { "wo", "number",         false, true    },
  { "wo", "relativenumber", false, true    },
  { "wo", "cursorline",     false, true    },
}

local _active = false
local _snap   = {}

local function apply_spec(active)
  local applied = {}   -- track which keys were successfully set this pass

  for _, s in ipairs(SPEC) do
    local scope, key, off_value, default = s[1], s[2], s[3], s[4]

    local ok, err = pcall(function()
      if active then
        _snap[key]      = vim[scope][key]
        vim[scope][key] = off_value
      else
        vim[scope][key] = (_snap[key] ~= nil) and _snap[key] or default
      end
    end)

    if ok then
      table.insert(applied, { scope = scope, key = key })
    else
      -- Roll back the options already applied in this pass.
      vim.notify(
        string.format(
          "[focus] apply_spec() failed on %s.%s: %s\n"
          .. "Rolling back %d already-applied option(s).",
          scope, key, tostring(err), #applied
        ),
        vim.log.levels.WARN
      )
      for i = #applied, 1, -1 do
        local a = applied[i]
        local prev = active and _snap[a.key] or nil
        -- On activate-rollback restore the snapshotted value;
        -- on deactivate-rollback restore the focus value (off_value for this key).
        pcall(function()
          if active then
            -- Restore from snapshot (which we already set above).
            if prev ~= nil then vim[a.scope][a.key] = prev end
          else
            -- Restore to focus-off value (we tried to restore but failed partway).
            for _, ss in ipairs(SPEC) do
              if ss[2] == a.key then vim[a.scope][a.key] = ss[3]; break end
            end
          end
        end)
      end
      -- Clear the partial snapshot so future calls don't use stale values.
      if active then _snap = {} end
      return false
    end
  end

  return true
end

local function set_zen(want_on)
  local ok, zm = pcall(require, "zen-mode")
  if not ok then return end
  if want_on then pcall(zm.open) else pcall(zm.close) end
end

local function set_twilight(want_on)
  if want_on then
    local ok, err = pcall(vim.cmd, "TwilightEnable")
    if not ok then vim.notify("[focus] TwilightEnable failed: " .. tostring(err), vim.log.levels.DEBUG) end
  else
    local ok, err = pcall(vim.cmd, "TwilightDisable")
    if not ok then vim.notify("[focus] TwilightDisable failed: " .. tostring(err), vim.log.levels.DEBUG) end
  end
end

local function fire_event(pattern)
  pcall(function()
    vim.api.nvim_exec_autocmds("User", { pattern = pattern, modeline = false })
  end)
end

---@param active boolean
function M.set(active)
  if active == _active then return end
  _active = active
  local ok = apply_spec(active)
  if not ok then
    -- Partial failure: stay in the previous state to avoid a half-applied mode.
    _active = not active
    vim.notify(
      "[focus] Mode change aborted due to option-set failure. State unchanged.",
      vim.log.levels.WARN
    )
    return
  end
  set_twilight(active)
  set_zen(active)
  fire_event(active and "FocusEnter" or "FocusLeave")
  vim.notify(active and "Focus mode" or "Focus off", vim.log.levels.INFO)
end

function M.enter()  M.set(true)  end
function M.exit()   M.set(false) end
function M.toggle() M.set(not _active) end

---@return boolean
function M.is_active() return _active end

vim.api.nvim_create_autocmd("VimLeavePre", {
  group    = vim.api.nvim_create_augroup("FocusModeCleanup", { clear = true }),
  callback = function()
    if _active then
      _active = false
      pcall(apply_spec, false)
      fire_event("FocusLeave")
    end
  end,
  desc = "Restore focus-mode options before Neovim exits",
})

return M
