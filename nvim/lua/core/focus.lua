-- lua/core/focus.lua
-- Focus mode: collapse all chrome, keep only the text.
-- Toggle with <leader>uF — goes deeper than ZenMode alone.
--
-- OPT (v2.3.14):
--   • apply_spec(active) replaces the two independent SPEC iterations in
--     enter() and exit(). A single traversal handles both directions,
--     eliminating the duplicated loop and the boolean-edge-case guard that
--     only existed in exit().
--
-- FIX (v2.3.15):
--   • apply_spec(active) boolean restore used the classic Lua false-value
--     ternary anti-pattern: `(saved ~= nil) and saved or default`.
--     When saved=false (user had number/relativenumber/cursorline disabled),
--     `true and false or default` evaluates to `default` (true), so focus
--     exit forcibly re-enabled those options. Fixed with an explicit
--     if/else so a legitimately-false snapshot is preserved correctly.

local M = {}

-- ── Option spec ──────────────────────────────────────────────────────────
-- { scope, key, off_value, default }
--   scope      : "o" = vim.o   "wo" = vim.wo
--   off_value  : value applied in focus mode
--   default    : fallback when snapshot is nil (i.e. enter() was never called)
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

-- ── Unified spec applicator ──────────────────────────────────────────────
-- active=true  → snapshot current values, then apply off_values (focus on)
-- active=false → restore from snapshot, fall back to defaults (focus off)
local function apply_spec(active)
  for _, s in ipairs(SPEC) do
    local scope, key, off_value, default = s[1], s[2], s[3], s[4]
    if active then
      _snap[key]      = vim[scope][key]
      vim[scope][key] = off_value
    else
      local saved = _snap[key]
      -- FIX (v2.3.15): use explicit if/else instead of `a and b or c`.
      -- The ternary form fails when b is false: `true and false or default`
      -- returns `default`, discarding a legitimately-false saved value.
      -- This affected number, relativenumber, and cursorline (default=true):
      -- if the user had any of them off before entering focus mode, exit
      -- would wrongly re-enable them.
      if saved ~= nil then
        vim[scope][key] = saved
      else
        vim[scope][key] = default
      end
    end
  end
end

function M.enter()
  if _active then return end
  _active = true
  apply_spec(true)
  pcall(vim.cmd, "Twilight")
  pcall(vim.cmd, "ZenMode")
  vim.notify("Focus mode", vim.log.levels.INFO)
end

function M.exit()
  if not _active then return end
  _active = false
  apply_spec(false)
  pcall(vim.cmd, "Twilight")
  pcall(vim.cmd, "ZenMode")
  vim.notify("Focus off", vim.log.levels.INFO)
end

function M.toggle()
  if _active then M.exit() else M.enter() end
end

return M
