-- lua/core/focus.lua — deep focus mode
--
-- Fires User events:
--   User FocusEnter  — after focus mode is activated
--   User FocusLeave  — after focus mode is deactivated
--
-- Other modules can hook these:
--   vim.api.nvim_create_autocmd("User", {
--     pattern = "FocusEnter", once = false,
--     callback = function() ... end,
--   })
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
  for _, s in ipairs(SPEC) do
    local scope, key, off_value, default = s[1], s[2], s[3], s[4]
    if active then
      _snap[key]      = vim[scope][key]
      vim[scope][key] = off_value
    else
      vim[scope][key] = (_snap[key] ~= nil) and _snap[key] or default
    end
  end
end

local function set_zen(want_on)
  local ok, zm = pcall(require, "zen-mode")
  if not ok then return end
  if want_on then pcall(zm.open) else pcall(zm.close) end
end

local function set_twilight(want_on)
  if want_on then pcall(vim.cmd, "TwilightEnable")
  else            pcall(vim.cmd, "TwilightDisable") end
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
  apply_spec(active)
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
