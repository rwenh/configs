-- lua/core/focus.lua
-- Focus mode: collapse all chrome, keep only the text.
-- Toggle with <leader>uF — goes deeper than ZenMode alone.
--
-- OPT (v2.3.14):
--   • apply_spec(active) replaces the two independent SPEC iterations in
--     enter() and exit(). A single traversal handles both directions,
--     eliminating the duplicated loop and the boolean-edge-case guard that
--     only existed in exit().

local M = {}

-- ── Option spec ──────────────────────────────────────────────────────────
-- { scope, key, off_value, default }
--   scope      : "o" = vim.o   "wo" = vim.wo
--   off_value  : value applied in focus mode
--   default    : fallback when snapshot is nil
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
      -- Boolean options: nil-safe (snapshot could legitimately be false).
      if type(default) == "boolean" then
        vim[scope][key] = (saved ~= nil) and saved or default
      else
        vim[scope][key] = saved or default
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
