-- lua/core/util/path.lua - Path utilities with caching
--
-- FIX (v2.2.3):
--   • Cache TTL reduced to 30s.
--   • DirChangedPre added alongside DirChanged.
--
-- FIX (v2.3.2):
--   • Cache key was the raw start_path argument. When find_root() is called
--     with no argument it defaults to vim.fn.expand("%:p:h"), which may produce
--     slightly different strings for the same physical directory depending on
--     symlinks or trailing slashes (e.g. "/proj/src" vs "/proj/src/"). Two
--     calls from buffers in the same directory could create duplicate cache
--     entries and bypass the TTL on one of them.
--     Fix: normalise start_path with fnamemodify(":p") and strip any trailing
--     slash before using it as the cache key. fnamemodify(":p") resolves
--     symlinks and canonicalises the path on all platforms.

local M = {}

local root_markers = {
  ".git", ".hg", ".svn",
  "Cargo.toml", "package.json", "go.mod", "pyproject.toml",
  "Makefile", "CMakeLists.txt", ".nvim.lua",
  "pom.xml", "build.gradle", "build.gradle.kts",
  "mix.exs", "rebar.config",
  "setup.py", "setup.cfg",
}

local cache         = {}
local cache_timeout = 30  -- seconds

-- Normalise a directory path to a canonical cache key.
-- fnamemodify(":p") adds a trailing slash on directories; strip it so that
-- "/proj/src/" and "/proj/src" hash to the same key.
local function normalise(p)
  local n = vim.fn.fnamemodify(p, ":p")
  return (n:gsub("/$", ""))
end

function M.find_root(start_path)
  start_path = start_path or vim.fn.expand("%:p:h")

  -- FIX: normalise before cache lookup so different representations of the
  -- same directory always hit the same cache slot.
  local cache_key = normalise(start_path)

  -- Cache hit (with TTL)
  if cache[cache_key] then
    local age = os.time() - cache[cache_key].time
    if age < cache_timeout then
      return cache[cache_key].root
    end
    cache[cache_key] = nil
  end

  local now     = os.time()
  local current = cache_key   -- already normalised; walk from here

  for _ = 1, 20 do
    for _, marker in ipairs(root_markers) do
      local path = current .. "/" .. marker

      local ok_dir,  is_dir  = pcall(function() return vim.fn.isdirectory(path) == 1 end)
      local ok_file, is_file = pcall(function() return vim.fn.filereadable(path)  == 1 end)

      if (ok_dir and is_dir) or (ok_file and is_file) then
        cache[cache_key] = { root = current, time = now }
        return current
      end
    end

    local parent = vim.fn.fnamemodify(current, ":h")
    if parent == current or parent == "" then break end
    current = parent
  end

  -- No marker found — return cwd but do NOT cache.
  return vim.fn.getcwd()
end

function M.clear_cache()
  cache = {}
end

local augroup = vim.api.nvim_create_augroup("PathCacheClear", { clear = true })

-- Clear before AND after directory change.
vim.api.nvim_create_autocmd({ "DirChangedPre", "DirChanged" }, {
  group    = augroup,
  pattern  = "*",
  callback = function() pcall(M.clear_cache) end,
  desc     = "Clear path.lua root cache on directory change",
})

return M
