-- lua/core/focus.lua — deep focus mode
--

local M = {}

-- ── Option spec ───────────────────────────────────────────────────────────────

-- { scope, key, off_value, default }
--   scope     : "o" = vim.o   "wo" = vim.wo
--   off_value : value applied when focus is active
--   default   : restore fallback when snapshot is nil
local SPEC = {
  { "o",  "laststatus",     0,     3       },
  { "o",  "showtabline",    0,     1       },
  { "o",  "ruler",          false, false   },
  { "wo", "signcolumn",     "no",  "yes:1" },
  { "wo", "number",         false, true    },
  { "wo", "relativenumber", false, true    },
  { "wo", "cursorline",     false, true    },
}

local _active     = false
local _snap       = {}
local _zen_active = false

-- ── Unified spec applicator ───────────────────────────────────────────────────

local function apply_spec(active)
  for _, s in ipairs(SPEC) do
    local scope, key, off_value, default = s[1], s[2], s[3], s[4]
    if active then
      _snap[key]      = vim[scope][key]
      vim[scope][key] = off_value
    else
      if _snap[key] ~= nil then
        vim[scope][key] = _snap[key]
      else
        vim[scope][key] = default
      end
    end
  end
end

-- ── Plugin coordination helpers ───────────────────────────────────────────────

local function set_zen(want_on)
  if want_on == _zen_active then return end
  _zen_active = want_on

  local ok, zm = pcall(require, "zen-mode")
  if not ok then return end

  if want_on then
    pcall(function() zm.open() end)
  else
    pcall(function() zm.close() end)
  end
end

local function set_twilight(want_on)
  if want_on then
    pcall(vim.cmd, "TwilightEnable")
  else
    pcall(vim.cmd, "TwilightDisable")
  end
end

-- ── Public API ────────────────────────────────────────────────────────────────

---@param active boolean
function M.set(active)
  if active == _active then return end
  _active = active
  apply_spec(active)
  set_twilight(active)
  set_zen(active)
  vim.notify(active and "Focus mode" or "Focus off", vim.log.levels.INFO)
end

--- Activate focus mode.
function M.enter()  M.set(true)  end

--- Deactivate focus mode.
function M.exit()   M.set(false) end

--- Toggle focus mode.
function M.toggle() M.set(not _active) end

--- Return true when focus mode is currently active.
---@return boolean
function M.is_active() return _active end

-- ── VimLeavePre cleanup ───────────────────────────────────────────────────────

vim.api.nvim_create_autocmd("VimLeavePre", {
  group    = vim.api.nvim_create_augroup("FocusModeCleanup", { clear = true }),
  callback = function()
    if _active then
      _active     = false
      _zen_active = false
      pcall(apply_spec, false)
    end
  end,
  desc = "Restore focus-mode options before Neovim exits",
})

return M
