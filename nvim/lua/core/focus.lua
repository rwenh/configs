-- lua/core/focus.lua
-- Focus mode: collapse all chrome, keep only the text.
-- Toggle with <leader>uF — goes deeper than ZenMode alone.
--
-- OPT (v2.3.13):
--   • Snapshot / restore driven by a declarative spec table instead of
--     8 manual field pairs.  Adding a new option is a single table row.
--   • Restore defaults moved into the spec so they are co-located with
--     the option they guard.

local M = {}

-- ── Option spec ─────────────────────────────────────────────────────────────
-- { scope, key, off_value, default }
--   scope      : "o" = vim.o   "wo" = vim.wo
--   off_value  : value applied in focus mode
--   default    : fallback when the snapshot is nil / boolean edge-case
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

function M.enter()
  if _active then return end
  _active = true

  for _, s in ipairs(SPEC) do
    local scope, key = s[1], s[2]
    _snap[key] = vim[scope][key]
    vim[scope][key] = s[3]
  end

  pcall(vim.cmd, "Twilight")
  pcall(vim.cmd, "ZenMode")
  vim.notify("Focus mode", vim.log.levels.INFO)
end

function M.exit()
  if not _active then return end
  _active = false

  for _, s in ipairs(SPEC) do
    local scope, key, _, default = s[1], s[2], s[3], s[4]
    local val = _snap[key]
    -- boolean options: nil-safe guard (snapshot could be false legitimately)
    if type(default) == "boolean" then
      vim[scope][key] = (val ~= nil) and val or default
    else
      vim[scope][key] = val or default
    end
  end

  pcall(vim.cmd, "Twilight")
  pcall(vim.cmd, "ZenMode")
  vim.notify("Focus off", vim.log.levels.INFO)
end

function M.toggle()
  if _active then M.exit() else M.enter() end
end

return M
