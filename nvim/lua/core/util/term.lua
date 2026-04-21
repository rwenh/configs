-- lua/core/util/term.lua - Shared toggleterm launch helper
--
-- Eliminates the ~24 copy-pasted toggleterm boilerplate blocks spread across
-- every lang spec. All callers reduce to a single M.float(cmd) call.
--
-- Usage:
--   local term = require("core.util.term")
--   term.float("gfortran -Wall -o /tmp/a.out " .. file .. " && /tmp/a.out")
--   term.float(cmd, { close_on_exit = true })   -- override any default

local M = {}

local DEFAULTS = {
  direction     = "float",
  close_on_exit = false,
}

--- Launch cmd in a floating toggleterm window.
-- @param cmd     string   Shell command to run.
-- @param opts    table?   Any Terminal:new() opts to merge over DEFAULTS.
function M.float(cmd, opts)
  assert(type(cmd) == "string" and cmd ~= "", "[term] cmd must be a non-empty string")

  local ok, terminal = pcall(require, "toggleterm.terminal")
  if not ok then
    vim.notify("[term] toggleterm not available", vim.log.levels.ERROR)
    return
  end

  local cfg = vim.tbl_extend("force", DEFAULTS, opts or {}, { cmd = cmd })
  pcall(function() terminal.Terminal:new(cfg):toggle() end)
end

--- Convenience: run cmd after cd-ing to project root.
-- @param cmd     string   Bare command (no cd prefix needed).
-- @param opts    table?   Forwarded to M.float.
function M.float_at_root(cmd, opts)
  assert(type(cmd) == "string" and cmd ~= "", "[term] cmd must be a non-empty string")

  local ok_path, path = pcall(require, "core.util.path")
  local root = (ok_path and path.find_root()) or vim.fn.getcwd()
  M.float("cd " .. vim.fn.shellescape(root) .. " && " .. cmd, opts)
end

return M
