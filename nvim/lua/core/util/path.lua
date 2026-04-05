-- lua/core/util/path.lua - Path utilities with caching
--
-- FIX (v2.2.3):
--   • Cache TTL reduced to 30s (was 300s). A monorepo switching branches
--     within 5 minutes would serve a stale root for the entire TTL window.
--     30s is still fast enough to avoid repeated filesystem walks during
--     normal editing, while being short enough that branch switches (which
--     take >1s) always get a fresh lookup on the next BufEnter.
--   • DirChangedPre added alongside DirChanged — the cache is cleared
--     *before* the directory changes so the first BufEnter in the new dir
--     never gets the old root from cache.
--   • No-cache fallback unchanged: getcwd() result is never cached.

local M = {}

local root_markers = {
  ".git", ".hg", ".svn",
  "Cargo.toml", "package.json", "go.mod", "pyproject.toml",
  "Makefile", "CMakeLists.txt", ".nvim.lua",
  "pom.xml", "build.gradle", "build.gradle.kts",
  "mix.exs", "rebar.config",
  "setup.py", "setup.cfg",
}

local cache        = {}
local cache_timeout = 30  -- seconds (reduced from 300 — see FIX above)

function M.find_root(start_path)
  start_path = start_path or vim.fn.expand("%:p:h")

  -- Cache hit (with TTL)
  if cache[start_path] then
    local age = os.time() - cache[start_path].time
    if age < cache_timeout then
      return cache[start_path].root
    end
    cache[start_path] = nil
  end

  local now     = os.time()
  local current = start_path

  for _ = 1, 20 do
    for _, marker in ipairs(root_markers) do
      local path = current .. "/" .. marker

      local ok_dir,  is_dir  = pcall(function() return vim.fn.isdirectory(path) == 1 end)
      local ok_file, is_file = pcall(function() return vim.fn.filereadable(path)  == 1 end)

      if (ok_dir and is_dir) or (ok_file and is_file) then
        cache[start_path] = { root = current, time = now }
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

-- FIX: clear before AND after directory change.
-- DirChangedPre fires before cwd changes → first lookup in new dir is fresh.
-- DirChanged fires after → handles programmatic cd() that skips Pre.
vim.api.nvim_create_autocmd({ "DirChangedPre", "DirChanged" }, {
  group    = augroup,
  pattern  = "*",
  callback = function() pcall(M.clear_cache) end,
  desc     = "Clear path.lua root cache on directory change",
})

return M
