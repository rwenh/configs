-- lua/core/util/path.lua — project-root detection with caching
--

local M = {}

local MAX_WALK_DEPTH = (type(vim.g.path_max_walk_depth) == "number" and vim.g.path_max_walk_depth > 0)
  and vim.g.path_max_walk_depth or 20

local CACHE_TTL = (type(vim.g.path_cache_ttl) == "number" and vim.g.path_cache_ttl > 0)
  and vim.g.path_cache_ttl or 30

local ROOT_MARKERS = {
  ".git", ".hg", ".svn",
  "Cargo.toml", "package.json", "go.mod", "pyproject.toml",
  "Makefile", "CMakeLists.txt", ".nvim.lua",
  "pom.xml", "build.gradle", "build.gradle.kts",
  "mix.exs", "rebar.config",
  "setup.py", "setup.cfg",
}

-- ── Per-project ignore patterns ───────────────────────────────────────────────
--
-- Set via vim.g.path_ignore_dirs in init.lua:
--
--   vim.g.path_ignore_dirs = { "vendor", "node_modules", "third_party" }
--
local IGNORE_DIRS = (function()
  local t = { ".cache", "__pycache__" }
  if type(vim.g.path_ignore_dirs) == "table" then
    vim.list_extend(t, vim.g.path_ignore_dirs)
  end
  return t
end)()

local _cache = {}

local function normalize(p)
  local n = vim.fn.fnamemodify(p, ":p")
  return (n:gsub("/$", ""))
end

local function exists(path)
  local ok_d, is_dir  = pcall(function() return vim.fn.isdirectory(path) == 1 end)
  local ok_f, is_file = pcall(function() return vim.fn.filereadable(path) == 1 end)
  return (ok_d and is_dir) or (ok_f and is_file)
end

local function basename(path)
  return path:match("([^/\\]+)$") or path
end

local function is_ignored(dir)
  local base = basename(dir)
  for _, pat in ipairs(IGNORE_DIRS) do
    if base == pat or base:find(pat, 1, true) then return true end
  end
  return false
end

-- ── Synchronous find_root ─────────────────────────────────────────────────────

---@param start_path string?
---@return string|nil
function M.find_root(start_path)
  start_path = start_path or vim.fn.expand("%:p:h")
  local cache_key = normalize(start_path)

  local entry = _cache[cache_key]
  if entry and (os.time() - entry.time) < CACHE_TTL then return entry.root end
  _cache[cache_key] = nil

  local current = cache_key
  for _ = 1, MAX_WALK_DEPTH do
    if not is_ignored(current) then
      for _, marker in ipairs(ROOT_MARKERS) do
        if exists(current .. "/" .. marker) then
          _cache[cache_key] = { root = current, time = os.time() }
          return current
        end
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
          .. "\n  falling back to cwd: " .. cwd,
          vim.log.levels.DEBUG
        )
      end)
    end
    return cwd
  end
  return nil
end

-- ── Async find_root ───────────────────────────────────────────────────────────
--
---@param start_path string?
---@param callback   fun(root: string|nil)
function M.find_root_async(start_path, callback)
  if type(callback) ~= "function" then return end

  -- Fast path: synchronous result available from cache.
  start_path = start_path or vim.fn.expand("%:p:h")
  local cache_key = normalize(start_path)
  local entry = _cache[cache_key]
  if entry and (os.time() - entry.time) < CACHE_TTL then
    vim.schedule(function() callback(entry.root) end)
    return
  end

  -- If vim.uv async fs_stat is available, use it.
  if not (vim.uv and vim.uv.fs_stat) then
    vim.schedule(function() callback(M.find_root(start_path)) end)
    return
  end

  local current    = cache_key
  local depth      = 0
  local marker_idx = 1

  local function check_next()
    if depth > MAX_WALK_DEPTH then
      vim.schedule(function()
        callback(vim.fn.getcwd())
      end)
      return
    end

    if marker_idx > #ROOT_MARKERS then
      -- All markers exhausted for this directory; walk up.
      local parent = vim.fn.fnamemodify(current, ":h")
      if parent == current or parent == "" then
        vim.schedule(function() callback(vim.fn.getcwd()) end)
        return
      end
      current    = parent
      marker_idx = 1
      depth      = depth + 1
    end

    if is_ignored(current) then
      local parent = vim.fn.fnamemodify(current, ":h")
      if parent == current then
        vim.schedule(function() callback(vim.fn.getcwd()) end)
        return
      end
      current    = parent
      marker_idx = 1
      depth      = depth + 1
    end

    local path = current .. "/" .. ROOT_MARKERS[marker_idx]
    vim.uv.fs_stat(path, function(err, stat)
      if not err and stat then
        -- Found a root marker.
        _cache[cache_key] = { root = current, time = os.time() }
        vim.schedule(function() callback(current) end)
      else
        marker_idx = marker_idx + 1
        check_next()
      end
    end)
  end

  check_next()
end

function M.clear_cache() _cache = {} end

vim.api.nvim_create_autocmd({ "DirChangedPre", "DirChanged" }, {
  group    = vim.api.nvim_create_augroup("PathCacheClear", { clear = true }),
  pattern  = "*",
  callback = function() pcall(M.clear_cache) end,
  desc     = "Invalidate path.lua root cache on directory change",
})

return M
