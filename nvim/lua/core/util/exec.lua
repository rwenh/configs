-- lua/core/util/exec.lua — executable presence guards and version detection
--

local M = {}

-- ── Version cache ─────────────────────────────────────────────────────────────
local _version_cache = {}

-- ── Synchronous guards ────────────────────────────────────────────────────────

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

---@param  names  string[]
---@param  hint   string?
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

-- ── Synchronous version detection (cached) ────────────────────────────────────

---@param bin         string   binary name to check first with executable()
---@param version_cmd string   full shell command (e.g. "python3 --version")
---@param pattern     string?  Lua pattern to extract version from output
---@return string|nil          version string, "installed", or nil
function M.version(bin, version_cmd, pattern)
  if _version_cache[bin] ~= nil then return _version_cache[bin] end

  if vim.fn.executable(bin) ~= 1 then
    _version_cache[bin] = nil
    return nil
  end

  local out = vim.trim(vim.fn.system(version_cmd .. " 2>/dev/null"))
  local ver
  if vim.v.shell_error ~= 0 or out == "" then
    ver = "installed"
  else
    ver = out:match(pattern or "%d+%.%d+%.?%d*") or "installed"
  end

  _version_cache[bin] = ver
  return ver
end

--- Clear the version cache (call after :MasonUpdate or tool upgrades).
function M.clear_version_cache()
  _version_cache = {}
end

-- ── Async version detection (vim.system on Nvim 0.10+) ───────────────────────
--
-- Usage:
--   exec.version_async("node", { "node", "--version" }, function(ver)
--     if ver then vim.notify("Node: " .. ver) end
--   end)
--
---@param bin      string    binary name (checked with executable() first)
---@param cmd      string[]  argv table (e.g. { "node", "--version" })
---@param callback fun(ver: string|nil)  called on the main loop
---@param pattern  string?   Lua pattern to extract version (default: "%d+%.%d+%.?%d*")
function M.version_async(bin, cmd, callback, pattern)
  if type(callback) ~= "function" then return end

  -- Fast path: already cached.
  if _version_cache[bin] ~= nil then
    vim.schedule(function() callback(_version_cache[bin]) end)
    return
  end

  if vim.fn.executable(bin) ~= 1 then
    _version_cache[bin] = nil
    vim.schedule(function() callback(nil) end)
    return
  end

  -- vim.system available on Nvim 0.10+.
  if vim.system then
    vim.system(cmd, { text = true }, function(result)
      local ver
      if result.code == 0 and result.stdout and result.stdout ~= "" then
        local out = vim.trim(result.stdout:match("([^\n]+)") or "")
        ver = out:match(pattern or "%d+%.%d+%.?%d*") or "installed"
      else
        ver = "installed"
      end
      _version_cache[bin] = ver
      vim.schedule(function() callback(ver) end)
    end)
  else
    -- Synchronous fallback for Neovim < 0.10.
    local ver = M.version(bin, table.concat(cmd, " "), pattern)
    vim.schedule(function() callback(ver) end)
  end
end

return M
