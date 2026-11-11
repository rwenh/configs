-- lua/core/util/safe_setup.lua — run one plugin setup; report a failure once, with the error
--
local M = {}
local _reported = {}

---@param name string   plugin or feature name, used as the notice tag
---@param fn   fun()    the setup call
---@param fix  string?  what the user can do about a failure
---@return boolean ok
function M.run(name, fn, fix)
  local ok, err = pcall(fn)
  if ok then
    return true
  end
  if not _reported[name] then
    _reported[name] = true
    vim.notify(
      ("[%s] setup failed: %s%s"):format(name, tostring(err), fix and ("\n  fix: " .. fix) or ""),
      vim.log.levels.WARN
    )
  end
  return false
end

return M
