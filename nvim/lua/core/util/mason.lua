-- lua/core/util/mason.lua — Mason path resolution helpers
--
-- Eliminates the local mason_bin() / mason_pkg() helpers that were
-- independently defined inside dap.lua's config() closure and referenced
-- implicitly in several lang specs.
--
-- Single source-of-truth for Mason binary / package path construction.
-- All callers (dap.lua, lang specs) should use these instead of inline
-- stdpath("data") .. "/mason/..." concatenation.

local M = {}

--- Return the resolved path to a Mason-managed binary.
--- Checks the system PATH first, then falls back to the Mason bin directory.
---
---@param name string  binary name (e.g. "codelldb", "dlv", "debugpy")
---@return string      absolute path (may not exist — callers must check)
function M.bin(name)
  local p = vim.fn.exepath(name)
  if p ~= "" then return p end
  return vim.fn.stdpath("data") .. "/mason/bin/" .. name
end

--- Return the absolute path to a file inside a Mason package directory.
---
---@param rel string  path relative to mason/packages/ (e.g. "js-debug-adapter/js-debug/src/dapDebugServer.js")
---@return string
function M.pkg(rel)
  return vim.fn.stdpath("data") .. "/mason/packages/" .. rel
end

--- Return the Mason packages root directory.
---@return string
function M.packages_root()
  return vim.fn.stdpath("data") .. "/mason/packages"
end

--- Return true only when the named Mason binary is executable.
--- Convenience wrapper used by DAP adapter guards.
---
---@param name string
---@return boolean
function M.bin_ok(name)
  local p = M.bin(name)
  return vim.fn.executable(p) == 1 or vim.fn.filereadable(p) == 1
end

return M
