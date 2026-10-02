-- lua/core/focus.lua — deep focus mode
--
-- Fixes vs v2.5.0:
--   * window-local options are snapshotted/restored on the ORIGIN window (not
--     "whatever window is current when you exit")
--   * ZenMode is closed BEFORE options are restored, then restored again after a
--     short defer — ZenMode restores the origin window from a snapshot taken AFTER
--     we had already stripped it, which used to leave the window stripped
--   * state lives in vim.g, so `:Reload core.focus` while active is safe
--   * apply is transactional: any failure rolls back what was already set

local M = {}

local STATE_KEY = "_nvim_focus_state"

local GLOBAL_SPEC = {   -- { option, focused value }
  { "laststatus",  0     },
  { "showtabline", 0     },
  { "ruler",       false },
}
local WINDOW_SPEC = {
  { "signcolumn",     "no"  },
  { "number",         false },
  { "relativenumber", false },
  { "cursorline",     false },
}
local FALLBACK = {
  laststatus = 3, showtabline = 1, ruler = false,
  signcolumn = "yes:1", number = true, relativenumber = true, cursorline = true,
}

local function load_state()
  local s = vim.g[STATE_KEY]
  if type(s) == "table" and s.active then return s end
  return nil
end

local function save_state(s) vim.g[STATE_KEY] = s end   -- nil clears

local function set_wo(win, key, val)
  vim.api.nvim_set_option_value(key, val, { win = win, scope = "local" })
end

local function snapshot(win)
  local s = { active = true, win = win, global = {}, wo = {} }
  for _, e in ipairs(GLOBAL_SPEC) do s.global[e[1]] = vim.o[e[1]] end
  for _, e in ipairs(WINDOW_SPEC) do s.wo[e[1]] = vim.api.nvim_get_option_value(e[1], { win = win }) end
  return s
end

local function restore(s)
  local win = (s.win and vim.api.nvim_win_is_valid(s.win)) and s.win or vim.api.nvim_get_current_win()
  for _, e in ipairs(GLOBAL_SPEC) do
    local v = s.global and s.global[e[1]]
    if v == nil then v = FALLBACK[e[1]] end
    pcall(function() vim.o[e[1]] = v end)
  end
  for _, e in ipairs(WINDOW_SPEC) do
    local v = s.wo and s.wo[e[1]]
    if v == nil then v = FALLBACK[e[1]] end
    pcall(set_wo, win, e[1], v)
  end
end

--- Apply the focus values; on any failure, undo what was applied.
---@return boolean ok
---@return string? err
local function apply(s)
  local undo = {}
  local function try(key, setter, restore_val)
    local ok, err = pcall(setter)
    if not ok then
      for i = #undo, 1, -1 do pcall(undo[i]) end
      return false, string.format("%s: %s", key, tostring(err))
    end
    table.insert(undo, function() setter(restore_val) end)
    return true
  end

  for _, e in ipairs(GLOBAL_SPEC) do
    local key, want, prev = e[1], e[2], s.global[e[1]]
    -- NB: no `(v == nil) and want or v` here — that idiom yields nil when want == false.
    local ok, err = try(key, function(v) if v == nil then v = want end; vim.o[key] = v end, prev)
    if not ok then return false, err end
  end
  for _, e in ipairs(WINDOW_SPEC) do
    local key, want, prev = e[1], e[2], s.wo[e[1]]
    local ok, err = try(key, function(v) if v == nil then v = want end; set_wo(s.win, key, v) end, prev)
    if not ok then return false, err end
  end
  return true
end

local function set_zen(want_on)
  local ok, zm = pcall(require, "zen-mode")
  if not ok then return end
  if want_on then pcall(zm.open) else pcall(zm.close) end
end

local function set_twilight(want_on)
  local ok, err = pcall(vim.cmd, want_on and "TwilightEnable" or "TwilightDisable")
  if not ok then vim.notify("[focus] Twilight toggle failed: " .. tostring(err), vim.log.levels.DEBUG) end
end

local function fire_event(pattern)
  pcall(vim.api.nvim_exec_autocmds, "User", { pattern = pattern, modeline = false })
end

---@param active boolean
function M.set(active)
  local state = load_state()
  if active == (state ~= nil) then return end

  if active then
    local s = snapshot(vim.api.nvim_get_current_win())
    local ok, err = apply(s)
    if not ok then
      vim.notify("[focus] aborted, state unchanged — " .. tostring(err), vim.log.levels.WARN)
      return
    end
    save_state(s)
    set_twilight(true)
    set_zen(true)
    fire_event("FocusEnter")
    vim.notify("Focus mode", vim.log.levels.INFO)
  else
    set_zen(false)            -- close ZenMode first …
    set_twilight(false)
    restore(state)            -- … then restore our snapshot …
    save_state(nil)
    vim.defer_fn(function()   -- … and once more after ZenMode's own deferred restore
      if not load_state() then restore(state) end
    end, 80)
    fire_event("FocusLeave")
    vim.notify("Focus off", vim.log.levels.INFO)
  end
end

function M.enter()  M.set(true)  end
function M.exit()   M.set(false) end
function M.toggle() M.set(load_state() == nil) end

---@return boolean
function M.is_active() return load_state() ~= nil end

vim.api.nvim_create_autocmd("VimLeavePre", {
  group    = vim.api.nvim_create_augroup("FocusModeCleanup", { clear = true }),
  callback = function()
    local s = load_state()
    if s then restore(s); save_state(nil); fire_event("FocusLeave") end
  end,
  desc = "Restore focus-mode options before Neovim exits",
})

return M
