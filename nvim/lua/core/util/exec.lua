-- lua/core/util/exec.lua — executable presence guards and version detection
--

local M = {}

--
---@param name  string   binary name (e.g. "gcc", "python3", "ghdl")
---@param hint  string?  optional install hint — shown after "not found" message
---@return boolean
function M.require_bin(name, hint)
  if vim.fn.executable(name) == 1 then return true end
  local msg = string.format("[exec] '%s' not found.", name)
  if hint and hint ~= "" then
    msg = msg .. "\nInstall: " .. hint
  end
  vim.notify(msg, vim.log.levels.ERROR)
  return false
end

--
---@param name  string
---@param hint  string?
---@return boolean
function M.warn_bin(name, hint)
  if vim.fn.executable(name) == 1 then return true end
  local msg = string.format("[exec] '%s' not found (optional).", name)
  if hint and hint ~= "" then
    msg = msg .. "\nInstall: " .. hint
  end
  vim.notify(msg, vim.log.levels.WARN)
  return false
end

--
---@param  names string[]
---@return string|nil   absolute path via exepath(), or nil if none found
function M.find_first(names)
  for _, name in ipairs(names) do
    if vim.fn.executable(name) == 1 then
      local p = vim.fn.exepath(name)
      if p and p ~= "" then return p end
    end
  end
  return nil
end

--
---@param bin         string   binary name to check first with executable()
---@param version_cmd string   full shell command (e.g. "python3 --version")
---@param pattern     string?  Lua pattern to extract version from output
---                            defaults to "%d+%.%d+%.?%d*"
---@return string|nil          version string, "installed" (no version parseable), or nil
function M.version(bin, version_cmd, pattern)
  if vim.fn.executable(bin) ~= 1 then return nil end
  local out = vim.trim(vim.fn.system(version_cmd .. " 2>/dev/null"))
  if vim.v.shell_error ~= 0 or out == "" then
    return "installed"   -- binary exists but version command failed
  end
  return out:match(pattern or "%d+%.%d+%.?%d*") or "installed"
end

--
---@param  names  string[]
---@param  hint   string?   shown if any binary is missing
---@return boolean
function M.require_all(names, hint)
  for _, name in ipairs(names) do
    if vim.fn.executable(name) ~= 1 then
      local msg = string.format("[exec] '%s' not found.", name)
      if hint and hint ~= "" then msg = msg .. "\nInstall: " .. hint end
      vim.notify(msg, vim.log.levels.ERROR)
      return false
    end
  end
  return true
end

return M
