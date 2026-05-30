-- lua/core/util/path.lua — project-root detection with caching
--

local M = {}

-- ── Constants ─────────────────────────────────────────────────────────────────
-- Override at the top of init.lua before plugins load, e.g.:
--   vim.g.path_max_walk_depth = 30   -- deeper monorepo structures
--   vim.g.path_cache_ttl      = 60   -- less aggressive cache invalidation
local MAX_WALK_DEPTH = (type(vim.g.path_max_walk_depth) == "number"
  and vim.g.path_max_walk_depth > 0)
  and vim.g.path_max_walk_depth or 20

local CACHE_TTL = (type(vim.g.path_cache_ttl) == "number"
  and vim.g.path_cache_ttl > 0)
  and vim.g.path_cache_ttl or 30

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

---@param start_path string?  directory from which to begin the upward walk
---@return string|nil
function M.find_root(start_path)
  start_path = start_path or vim.fn.expand("%:p:h")

  local cache_key = normalize(start_path)

  local entry = _cache[cache_key]
  if entry and (os.time() - entry.time) < CACHE_TTL then
    return entry.root
  end
  _cache[cache_key] = nil

  local current = cache_key

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

  local ok_cwd, cwd = pcall(vim.fn.getcwd)
  if ok_cwd and cwd and cwd ~= "" then
    if vim.g.path_debug then
      vim.schedule(function()
        vim.notify(
          "[path] no root markers found walking from: " .. start_path
          .. "\n  falling back to cwd: " .. cwd
          .. "\n  (set vim.g.path_debug = false to silence this)",
          vim.log.levels.DEBUG
        )
      end)
    end
    return cwd
  end

  return nil
end

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
