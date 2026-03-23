-- lua/core/util/path.lua - Path utilities

local M = {}

-- FIX #4: Removed ".git/config" — it is always co-located with ".git" so it
-- was a redundant marker adding no extra detection coverage.
local root_markers = {
  ".git", ".hg", ".svn",
  "Cargo.toml", "package.json", "go.mod", "pyproject.toml",
  "Makefile", "CMakeLists.txt", ".nvim.lua",
}

local cache = {}
local cache_timeout = 300

function M.find_root(start_path)
  start_path = start_path or vim.fn.expand("%:p:h")

  if cache[start_path] then
    local age = os.time() - cache[start_path].time
    if age < cache_timeout then
      return cache[start_path].root
    end
  end

  -- FIX #1: Capture os.time() once so both cache-write sites use the same
  -- timestamp and can't diverge if execution crosses a second boundary.
  local now     = os.time()
  local current = start_path

  for _ = 1, 20 do
    for _, marker in ipairs(root_markers) do
      local path = current .. "/" .. marker
      if vim.fn.isdirectory(path) == 1 or vim.fn.filereadable(path) == 1 then
        cache[start_path] = { root = current, time = now }
        return current
      end
    end

    local parent = vim.fn.fnamemodify(current, ":h")
    if parent == current or parent == "" then break end
    current = parent
  end

  local cwd = vim.fn.getcwd()
  cache[start_path] = { root = cwd, time = now }
  return cwd
end

function M.clear_cache()
  cache = {}
end

-- FIX #2: Invalidate the cache whenever the user changes working directory
-- (:cd, :lcd, :tcd) so find_root doesn't return a stale project root within
-- the 300-second TTL window after switching projects.
vim.api.nvim_create_autocmd("DirChanged", {
  group    = vim.api.nvim_create_augroup("PathCacheClear", { clear = true }),
  pattern  = "*",
  callback = M.clear_cache,
  desc     = "Clear path.lua root cache on directory change",
})

return M
