-- lua/core/util/path.lua — project-root detection with caching
--
-- Cache shape: { [normalized_path: string]: { root: string, time: number } }
--

local M = {}

-- ── Constants ─────────────────────────────────────────────────────────────────
local MAX_WALK_DEPTH = 20   -- max directory levels walked toward filesystem root
local CACHE_TTL      = 30   -- seconds before a cached root entry is considered stale

local ROOT_MARKERS = {
  ".git", ".hg", ".svn",
  "Cargo.toml", "package.json", "go.mod", "pyproject.toml",
  "Makefile", "CMakeLists.txt", ".nvim.lua",
  "pom.xml", "build.gradle", "build.gradle.kts",
  "mix.exs", "rebar.config",
  "setup.py", "setup.cfg",
}

-- ── Internal state ─────────────────────────────────────────────────────────────
-- Shape: { [normalized_path]: { root = string, time = number } }
local _cache = {}

-- ── Helpers ────────────────────────────────────────────────────────────────────

--- Normalize a directory path to a canonical cache key.
--- Strips trailing slash so "/proj/src/" and "/proj/src" hash to the same slot.
local function normalize(p)
  local n = vim.fn.fnamemodify(p, ":p")
  return (n:gsub("/$", ""))
end

--- Return true if *path* exists as a directory OR a readable file.
local function exists(path)
  local ok_d, is_dir  = pcall(function() return vim.fn.isdirectory(path) == 1 end)
  local ok_f, is_file = pcall(function() return vim.fn.filereadable(path)  == 1 end)
  return (ok_d and is_dir) or (ok_f and is_file)
end

-- ── Public API ─────────────────────────────────────────────────────────────────

--- Find the project root for *start_path* (defaults to the directory of the
--- current file).  Returns the root directory string, or nil when detection
--- fails entirely (headless contexts, no markers found, getcwd() throws).
---
---@param start_path string?  directory from which to begin the upward walk
---@return string|nil
function M.find_root(start_path)
  start_path = start_path or vim.fn.expand("%:p:h")

  local cache_key = normalize(start_path)

  -- Cache hit (TTL-gated)
  local entry = _cache[cache_key]
  if entry and (os.time() - entry.time) < CACHE_TTL then
    return entry.root
  end
  _cache[cache_key] = nil   -- discard stale entry before fresh walk

  local current = cache_key   -- already normalized; walk upward from here

  for _ = 1, MAX_WALK_DEPTH do
    for _, marker in ipairs(ROOT_MARKERS) do
      if exists(current .. "/" .. marker) then
        _cache[cache_key] = { root = current, time = os.time() }
        return current
      end
    end

    local parent = vim.fn.fnamemodify(current, ":h")
    if parent == current or parent == "" then break end
    current = parent
  end

  -- No marker found — return cwd as a best-effort fallback but do NOT cache
  -- (the user may open a different project in the same session).
  -- pcall: getcwd() can throw in headless / embedded contexts (v2.3.16).
  local ok_cwd, cwd = pcall(vim.fn.getcwd)
  return (ok_cwd and cwd and cwd ~= "") and cwd or nil
end

--- Invalidate the entire root cache.
function M.clear_cache()
  _cache = {}
end

-- ── Autocmds ───────────────────────────────────────────────────────────────────

vim.api.nvim_create_autocmd({ "DirChangedPre", "DirChanged" }, {
  group    = vim.api.nvim_create_augroup("PathCacheClear", { clear = true }),
  pattern  = "*",
  callback = function() pcall(M.clear_cache) end,
  desc     = "Invalidate path.lua root cache on directory change",
})

return M
