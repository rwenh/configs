-- lua/core/util/path.lua — project-root detection with caching
--
-- Fixes vs v2.5.0:
--   * is_ignored() matched by SUBSTRING ("build" also skipped "rebuild-tools"); now exact
--   * fs_stat instead of two vim.fn calls per probe
--   * "/" no longer normalises to ""
--   * opt-in vim.g.path_prefer_vcs: pick the VCS root over a nearer Makefile/package.json
--   * new find_up(markers, start): nearest dir containing any marker (e.g. a Makefile)
--   * find_root_async keeps its API but is a scheduled sync call (the walk is ~1 ms)

local M = {}
local uv = vim.uv or vim.loop

local MAX_WALK_DEPTH = (type(vim.g.path_max_walk_depth) == "number" and vim.g.path_max_walk_depth > 0)
  and vim.g.path_max_walk_depth or 20

local CACHE_TTL = (type(vim.g.path_cache_ttl) == "number" and vim.g.path_cache_ttl > 0)
  and vim.g.path_cache_ttl or 30

local VCS_MARKERS = { ".git", ".hg", ".svn" }

local ROOT_MARKERS = {
  ".git", ".hg", ".svn",
  "Cargo.toml", "package.json", "go.mod", "pyproject.toml",
  "Makefile", "CMakeLists.txt", ".nvim.lua",
  "pom.xml", "build.gradle", "build.gradle.kts",
  "mix.exs", "rebar.config",
  "setup.py", "setup.cfg",
}

local PACKAGE_MARKERS = {
  "package.json", "Cargo.toml", "go.mod", "pyproject.toml",
  "setup.py", "setup.cfg", "pom.xml", "build.gradle",
  "build.gradle.kts", "mix.exs", "rebar.config",
}

local IGNORE_DIRS = (function()
  local t = { ".cache", "__pycache__" }
  if type(vim.g.path_ignore_dirs) == "table" then vim.list_extend(t, vim.g.path_ignore_dirs) end
  return t
end)()

local _cache     = {}
local _pkg_cache = {}

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
    if base == pat then return true end
  end
  return false
end

--- Walk upward from `start_key`, returning the first directory holding any marker.
---@param start_key string  normalised start directory
---@param markers   string[]
---@return string|nil
local function walk_for_marker(start_key, markers)
  local current = start_key
  for _ = 1, MAX_WALK_DEPTH do
    if not is_ignored(current) then
      for _, marker in ipairs(markers) do
        if exists(current .. "/" .. marker) then return current end
      end
    end
    local parent = vim.fn.fnamemodify(current, ":h")
    if parent == current or parent == "" then break end
    current = parent
  end
  return nil
end

local function cwd_fallback(start_path, tag)
  local ok, cwd = pcall(vim.fn.getcwd)
  if not (ok and cwd and cwd ~= "") then return nil end
  if vim.g.path_debug then
    vim.schedule(function()
      vim.notify(
        "[path] " .. tag .. "no root markers found walking from: " .. start_path
        .. "\n  falling back to cwd: " .. cwd,
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

  local entry = _cache[key]
  if entry and (os.time() - entry.time) < CACHE_TTL then return entry.root end
  _cache[key] = nil

  local found
  if vim.g.path_prefer_vcs == true then found = walk_for_marker(key, VCS_MARKERS) end
  found = found or walk_for_marker(key, ROOT_MARKERS) or cwd_fallback(start_path, "")
  if found then _cache[key] = { root = found, time = os.time() } end
  return found
end

---@param start_path string?
---@param callback   fun(root: string|nil)
function M.find_root_async(start_path, callback)
  if type(callback) ~= "function" then return end
  vim.schedule(function() callback(M.find_root(start_path)) end)
end

--- Nearest ancestor directory containing any of `markers` (uncached, no cwd fallback).
---@param markers    string[]
---@param start_path string?
---@return string|nil
function M.find_up(markers, start_path)
  return walk_for_marker(normalize(start_path or vim.fn.expand("%:p:h")), markers)
end

function M.clear_cache() _cache = {}; _pkg_cache = {} end

---@param start_path string?
---@return string|nil
function M.find_package_root(start_path)
  start_path = start_path or vim.fn.expand("%:p:h")
  local key = normalize(start_path)

  local entry = _pkg_cache[key]
  if entry and (os.time() - entry.time) < CACHE_TTL then return entry.root end

  -- Cache misses (nil) too, or buffers with no package marker re-walk every call.
  local found = walk_for_marker(key, PACKAGE_MARKERS)
  _pkg_cache[key] = { root = found, time = os.time() }
  return found
end

vim.api.nvim_create_autocmd({ "DirChangedPre", "DirChanged" }, {
  group    = vim.api.nvim_create_augroup("PathCacheClear", { clear = true }),
  pattern  = "*",
  callback = function() pcall(M.clear_cache) end,
  desc     = "Invalidate path.lua root cache on directory change",
})

return M
