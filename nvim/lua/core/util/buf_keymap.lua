-- lua/core/util/buf_keymap.lua — buffer-local keymap registration helper
--
-- Eliminates the local `map`/`imap`/`mapv` factory functions that were
-- independently defined in python.lua, java.lua, kotlin.lua, and elsewhere.
--
-- Usage:
--
--   local bkm = require("core.util.buf_keymap")
--
--   -- Single map:
--   bkm.set(buf, "n", "<leader>pydm", function() ... end, "Python Debug Method")
--
--   -- Batch (most common pattern):
--   bkm.batch(buf, {
--     { "n",        "<leader>pydm", function() ... end, "Python Debug Method" },
--     { "n",        "<leader>pydc", function() ... end, "Python Debug Class"  },
--     { {"n","v"},  "<leader>pyds", function() ... end, "Python Debug Selection" },
--   })
--
--   -- Guard: apply at most once per buffer lifetime.
--   -- Pass a unique flag_name per feature to prevent re-registration on
--   -- :luafile reloads (mirrors python.lua's python_dap_keymaps_registered flag).
--   bkm.batch(buf, maps, "python_dap_keymaps_registered")

local M = {}

--- Register a single buffer-local keymap.
---
---@param buf       integer       buffer handle
---@param mode      string|table  vim mode(s)
---@param lhs       string        left-hand side key sequence
---@param rhs       function|string
---@param desc      string        description shown in which-key / :map
function M.set(buf, mode, lhs, rhs, desc)
  vim.keymap.set(mode, lhs, rhs, {
    buffer  = buf,
    silent  = true,
    noremap = true,
    desc    = desc,
  })
end

--- Register a batch of buffer-local keymaps.
--- Each entry: { mode, lhs, rhs, desc }
---
--- If *flag_name* is provided the maps are applied at most once per buffer —
--- subsequent calls with the same flag_name on the same buffer are no-ops.
--- This replaces the ad-hoc `vim.b[buf].X_registered` pattern used in
--- python.lua and elsewhere.
---
---@param buf        integer    buffer handle
---@param maps       table      list of { mode, lhs, rhs, desc }
---@param flag_name  string?    optional deduplication key stored in vim.b[buf]
function M.batch(buf, maps, flag_name)
  if flag_name then
    local ok, already = pcall(function() return vim.b[buf][flag_name] end)
    if ok and already then return end
    pcall(function() vim.b[buf][flag_name] = true end)
  end

  for _, spec in ipairs(maps) do
    local mode, lhs, rhs, desc = spec[1], spec[2], spec[3], spec[4]
    M.set(buf, mode, lhs, rhs, desc)
  end
end

--- Convenience: apply *maps* to every currently-loaded buffer whose filetype
--- matches *ft*, then register a FileType autocmd to catch future buffers.
--- This replaces the dual "retroactive loop + FileType autocmd" pattern that
--- appears in python.lua (DAP keymaps) and iron.nvim keymaps.
---
---@param ft         string     filetype to match (e.g. "python")
---@param maps       table      list of { mode, lhs, rhs, desc }
---@param flag_name  string?    deduplication key (recommended)
---@param group      string?    augroup name (defaults to "BufKeymap_<ft>_<flag>")
function M.on_ft(ft, maps, flag_name, group)
  local augroup_name = group
    or ("BufKeymap_" .. ft .. (flag_name and ("_" .. flag_name) or ""))

  -- Retroactively apply to already-open buffers of this filetype.
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_loaded(buf)
    and vim.bo[buf].filetype == ft then
      M.batch(buf, maps, flag_name)
    end
  end

  -- Future buffers.
  vim.api.nvim_create_autocmd("FileType", {
    pattern  = ft,
    group    = vim.api.nvim_create_augroup(augroup_name, { clear = true }),
    callback = function(e) M.batch(e.buf, maps, flag_name) end,
    desc     = "Register " .. ft .. " buffer-local keymaps",
  })
end

return M
