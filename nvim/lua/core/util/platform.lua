-- lua/core/util/platform.lua — OS and package-manager detection for install hints
--
-- Detection runs once per session. On Linux the native managers are probed before brew, so
-- Linuxbrew does not shadow the distribution's own manager. On macOS brew is probed first.

local M = {}

local unpack = unpack or table.unpack

local NATIVE = { "zypper", "dnf", "apt-get", "pacman", "apk" }
local PM_CMD = {
  brew = "brew install",
  zypper = "sudo zypper install",
  dnf = "sudo dnf install",
  apt = "sudo apt install",
  pacman = "sudo pacman -S",
  apk = "sudo apk add",
}

local _pm -- nil: not probed yet; false: none found

local function probe()
  local order = vim.fn.has("mac") == 1 and { "brew", unpack(NATIVE) } or { unpack(NATIVE), "brew" }
  for _, bin in ipairs(order) do
    if vim.fn.executable(bin) == 1 then
      return bin == "apt-get" and "apt" or bin
    end
  end
  return nil
end

--- Package-manager key ("apt", "dnf", "zypper", "pacman", "apk", "brew"), or nil.
---@return string|nil
function M.pm()
  if _pm == nil then
    _pm = probe() or false
  end
  return _pm or nil
end

--- "macos", "linux" or "other".
---@return string
function M.os()
  if vim.fn.has("mac") == 1 then
    return "macos"
  end
  if vim.fn.has("linux") == 1 then
    return "linux"
  end
  return "other"
end

--- The ID from /etc/os-release (for example "ubuntu" or "opensuse-leap"), or nil.
---@return string|nil
function M.distro()
  if M.os() ~= "linux" then
    return nil
  end
  local ok, lines = pcall(vim.fn.readfile, "/etc/os-release")
  if not ok then
    return nil
  end
  for _, line in ipairs(lines) do
    local id = line:match('^ID="?([^"]+)"?$')
    if id then
      return id
    end
  end
  return nil
end

--- Command that installs `key` here, from core.util.system_pkgs.
--- A key with no row, or no column for this manager, gets the row's fallback, else "install <key>".
---@param key string
---@return string
function M.install_hint(key)
  local entry = require("core.util.system_pkgs")[key]
  if not entry then
    return "install " .. key
  end
  local pm = M.pm()
  local pkg = pm and entry[pm]
  if pkg then
    return PM_CMD[pm] .. " " .. pkg
  end
  return entry.fallback or ("install " .. key)
end

return M
