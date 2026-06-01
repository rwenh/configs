-- lua/core/util/mason.lua — Mason path resolution helpers
--

local M = {}

---@param name string  binary name (e.g. "codelldb", "dlv", "debugpy")
---@return string      absolute path (may not exist — callers must check)
function M.bin(name)
  local p = vim.fn.exepath(name)
  if p ~= "" then return p end
  return vim.fn.stdpath("data") .. "/mason/bin/" .. name
end

---@param rel string  path relative to mason/packages/
---                   (e.g. "js-debug-adapter/js-debug/src/dapDebugServer.js")
---@return string
function M.pkg(rel)
  return vim.fn.stdpath("data") .. "/mason/packages/" .. rel
end

--- Return the Mason packages root directory.
---@return string
function M.packages_root()
  return vim.fn.stdpath("data") .. "/mason/packages"
end

---@param name string  binary name passed to M.bin()
---@return boolean
function M.bin_ok(name)
  local p = M.bin(name)
  return vim.fn.executable(p) == 1
end

---@param rel string  path relative to mason/packages/
---                   (e.g. "elixir-ls/debugger.sh")
---@return boolean
function M.script_ok(rel)
  return vim.fn.filereadable(M.pkg(rel)) == 1
end

return M
