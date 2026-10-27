-- lua/core/util/path.lua — project-root detection
--
-- Root rule. Walking up from the start directory:
--   1. A PRIMARY marker (a build system or a project boundary) wins at any distance.
--      The nearest primary marker is the root.
--   2. Otherwise the nearest SECONDARY marker (package.json, Makefile, Gemfile, a VCS
--      directory) is the root.
-- So a Phoenix assets/package.json or a Rails app's nested package.json no longer becomes the
-- root above the mix.exs or config/application.rb that owns it. The earlier rule took the
-- nearest marker of any kind.
--
-- vim.g.path_prefer_vcs = true puts the VCS root ahead of both tiers (the earlier opt-in).
-- find_package_root keeps the nearest-package rule: it answers "which package am I in",
-- not "which project am I in".
--
-- Results are cached per start directory (core.util.cache) and cleared on :cd.

local cache = require("core.util.cache")
local flags = require("core.flags")

local M = {}
local uv = vim.uv or vim.loop

local function positive(name, default)
  local v = flags.get(name, default)
  return (type(v) == "number" and v > 0) and v or default
end

local MAX_WALK_DEPTH = positive("path_max_walk_depth", 20)
local CACHE_TTL = positive("path_cache_ttl", 30)

local VCS = { ".git", ".hg", ".svn" }

local PRIMARY = {
  "mix.exs",
  "rebar.config",
  "Cargo.toml",
  "build.zig",
  "go.mod",
  "go.work",
  "pyproject.toml",
  "setup.py",
  "setup.cfg",
  "pom.xml",
  "build.gradle",
  "build.gradle.kts",
  "settings.gradle",
  "settings.gradle.kts",
  "CMakeLists.txt",
  "config/application.rb",
  "vhdl_ls.toml",
  ".nvim.lua",
}

local SECONDARY = {
  ".git",
  ".hg",
  ".svn",
  "package.json",
  "Makefile",
  "makefile",
  "GNUmakefile",
  "Gemfile",
}

local PACKAGE_MARKERS = {
  "package.json",
  "Cargo.toml",
  "go.mod",
  "pyproject.toml",
  "setup.py",
  "setup.cfg",
  "pom.xml",
  "build.gradle",
  "build.gradle.kts",
  "mix.exs",
  "rebar.config",
}

local IGNORE_DIRS = (function()
  local t = { ".cache", "__pycache__" }
  local extra = flags.get("path_ignore_dirs", nil)
  if type(extra) == "table" then
    vim.list_extend(t, extra)
  end
  return t
end)()

local _roots = cache.new({ name = "path.root", ttl = CACHE_TTL })
local _pkgs = cache.new({ name = "path.package", ttl = CACHE_TTL })

local function normalize(p)
  local n = vim.fn.fnamemodify(p, ":p"):gsub("/$", "")
  return n == "" and "/" or n
end

local function exists(path)
  return uv.fs_stat(path) ~= nil
end

local function is_ignored(dir)
  local base = dir:match("([^/\\]+)$") or dir
  for _, pat in ipairs(IGNORE_DIRS) do
    if base == pat then
      return true
    end
  end
  return false
end

local function has_any(dir, markers)
  for _, m in ipairs(markers) do
    if exists(dir .. "/" .. m) then
      return true
    end
  end
  return false
end

--- Nearest directory holding any of `markers`, walking up from `start_key`.
---@param start_key string  normalised start directory
---@param markers   string[]
---@return string|nil
local function walk_for(start_key, markers)
  local current = start_key
  for _ = 1, MAX_WALK_DEPTH do
    if not is_ignored(current) and has_any(current, markers) then
      return current
    end
    local parent = vim.fn.fnamemodify(current, ":h")
    if parent == current or parent == "" then
      break
    end
    current = parent
  end
  return nil
end

--- One walk for the root rule: the nearest primary directory (the walk stops there), and the
--- nearest secondary directory passed on the way.
---@return string|nil primary
---@return string|nil secondary
local function scan(start_key)
  local secondary
  local current = start_key
  for _ = 1, MAX_WALK_DEPTH do
    if not is_ignored(current) then
      if has_any(current, PRIMARY) then
        return current, secondary
      end
      if not secondary and has_any(current, SECONDARY) then
        secondary = current
      end
    end
    local parent = vim.fn.fnamemodify(current, ":h")
    if parent == current or parent == "" then
      break
    end
    current = parent
  end
  return nil, secondary
end

local function cwd_fallback(start_path)
  local ok, cwd = pcall(vim.fn.getcwd)
  if not (ok and cwd and cwd ~= "") then
    return nil
  end
  if flags.get("path_debug", false) == true then
    vim.schedule(function()
      vim.notify(
        "[path] no root markers found walking from: " .. start_path .. "\n  falling back to cwd: " .. cwd,
        vim.log.levels.DEBUG
      )
    end)
  end
  return cwd
end

---@param start_path string?
---@return string|nil
function M.find_root(start_path)
  start_path = start_path or vim.fn.expand("%:p:h")
  local key = normalize(start_path)

  local hit, cached = _roots:lookup(key)
  if hit then
    return cached
  end

  local found
  if flags.get("path_prefer_vcs", false) == true then
    found = walk_for(key, VCS)
  end
  if not found then
    local primary, secondary = scan(key)
    found = primary or secondary
  end
  found = found or cwd_fallback(start_path)
  _roots:set(key, found)
  return found
end

---@param start_path string?
---@param callback   fun(root: string|nil)
function M.find_root_async(start_path, callback)
  if type(callback) ~= "function" then
    return
  end
  vim.schedule(function()
    callback(M.find_root(start_path))
  end)
end

--- Nearest ancestor directory containing any of `markers` (uncached, no cwd fallback).
---@param markers    string[]
---@param start_path string?
---@return string|nil
function M.find_up(markers, start_path)
  return walk_for(normalize(start_path or vim.fn.expand("%:p:h")), markers)
end

---@param start_path string?
---@return string|nil
function M.find_package_root(start_path)
  start_path = start_path or vim.fn.expand("%:p:h")
  local key = normalize(start_path)
  local hit, root = _pkgs:lookup(key)
  if hit then
    return root
  end
  root = walk_for(key, PACKAGE_MARKERS)
  _pkgs:set(key, root)
  return root
end

function M.clear_cache()
  _roots:clear_all()
  _pkgs:clear_all()
end

return M
