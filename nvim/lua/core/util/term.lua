-- lua/core/util/term.lua — shared toggleterm launch helper
--
-- All lang specs and runner.lua reduce terminal launches to a single
-- M.float(cmd) or M.float_at_root(cmd) call.
--

local M = {}

-- Accepted keys mirror toggleterm.terminal.Terminal:new() options.
-- direction, close_on_exit, on_exit, and cmd are the most commonly overridden.
local DEFAULTS = {
  direction     = "float",
  close_on_exit = false,
}

-- ── Public helpers ─────────────────────────────────────────────────────────────

--- Return a "cd <root> && " shell prefix with proper escaping.
--- Extracted from float_at_root so callers building composite commands can
--- reuse it without duplicating the shellescape logic.
---
---@param root string  absolute path to the target directory
---@return string      shell fragment including trailing space, or "" on blank root
function M.cd_prefix(root)
  if not root or root == "" then return "" end
  return "cd " .. vim.fn.shellescape(root) .. " && "
end

--- Launch *cmd* in a floating toggleterm window.
---
---@param cmd  string        shell command to run (must be non-empty)
---@param opts table?        any Terminal:new() opts merged over DEFAULTS
function M.float(cmd, opts)
  if type(cmd) ~= "string" or cmd == "" then
    vim.notify("[term] cmd must be a non-empty string", vim.log.levels.ERROR)
    return
  end

  local ok_req, terminal = pcall(require, "toggleterm.terminal")
  if not ok_req then
    vim.notify("[term] toggleterm not available — cannot launch terminal",
      vim.log.levels.ERROR)
    return
  end

  local cfg = vim.tbl_extend("force", DEFAULTS, opts or {}, { cmd = cmd })

  local ok_toggle, err = pcall(function()
    terminal.Terminal:new(cfg):toggle()
  end)
  if not ok_toggle then
    vim.notify("[term] failed to open terminal: " .. tostring(err),
      vim.log.levels.ERROR)
  end
end

--- Run *cmd* after cd-ing to the project root.
--- Uses M.cd_prefix() so the escaping logic lives in exactly one place.
---
---@param cmd  string   bare shell command (no cd prefix needed)
---@param opts table?   forwarded to M.float
function M.float_at_root(cmd, opts)
  if type(cmd) ~= "string" or cmd == "" then
    vim.notify("[term] cmd must be a non-empty string", vim.log.levels.ERROR)
    return
  end

  local ok_path, path = pcall(require, "core.util.path")
  local root = (ok_path and path.find_root()) or vim.fn.getcwd()

  if not root or root == "" then
    vim.notify("[term] could not resolve project root — command not launched",
      vim.log.levels.WARN)
    return
  end

  M.float(M.cd_prefix(root) .. cmd, opts)
end

return M
