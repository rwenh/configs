-- lua/core/util/mason.lua — Mason path resolution helpers
--
-- Eliminates the local mason_bin() / mason_pkg() helpers that were
-- independently defined inside dap.lua's config() closure and referenced
-- implicitly in several lang specs.
--

local M = {}

---@param name string  binary name (e.g. "codelldb", "dlv", "debugpy")
---@return string      absolute path (may not exist — callers must check)
function M.bin(name)
  local p = vim.fn.exepath(name)
  if p ~= "" then return p end
  return vim.fn.stdpath("data") .. "/mason/bin/" .. name
end

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

--- Uses executable() only — filereadable() without the executable bit
--- can match non-runnable files and produce a silent adapter failure.
--- For shell scripts (e.g. debugger.sh) callers should use
--- vim.fn.filereadable() directly and check the path themselves.
---
---@param name string
---@return boolean
function M.bin_ok(name)
  local p = M.bin(name)
  return vim.fn.executable(p) == 1
end

return M
