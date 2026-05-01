-- lua/core/focus.lua — deep focus mode
-- Toggle with <leader>uF.  Goes deeper than ZenMode alone by also stripping
-- the status line, sign column, line numbers, tab bar, and ruler.
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

local _active = false
local _snap   = {}

-- ── Unified spec applicator ───────────────────────────────────────────────────

local function apply_spec(active)
  for _, s in ipairs(SPEC) do
    local scope, key, off_value, default = s[1], s[2], s[3], s[4]
    if active then
      _snap[key]      = vim[scope][key]
      vim[scope][key] = off_value
    else
      -- Explicit if/else — the `a and b or c` ternary fails when b is false.
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
  -- ZenMode has no "is active" API; we rely on our own _active flag.
  -- The pcall means ZenMode loading/unloading is handled gracefully.
  pcall(vim.cmd, "ZenMode")   -- toggles; called only when transition is needed
  _ = want_on                 -- suppress unused-var lint; state tracked by _active
end

local function set_twilight(want_on)
  if want_on then
    pcall(vim.cmd, "TwilightEnable")
  else
    pcall(vim.cmd, "TwilightDisable")
  end
end

-- ── Public API ────────────────────────────────────────────────────────────────

--- Set focus mode to *active* (true = on, false = off).
--- This is the single entry point; enter/exit/toggle delegate here.
---@param active boolean
function M.set(active)
  if active == _active then return end   -- no-op if already in desired state
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
      _active = false
      pcall(apply_spec, false)   -- FIX B2: pcall added
    end
  end,
  desc = "Restore focus-mode options before Neovim exits",
})

return M
