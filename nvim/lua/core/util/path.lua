-- lua/core/util/path.lua - Path utilities with caching

local M = {}

-- RECALIBRATION: Safe root marker detection with comprehensive coverage
local root_markers = {
  ".git", ".hg", ".svn",
  "Cargo.toml", "package.json", "go.mod", "pyproject.toml",
  "Makefile", "CMakeLists.txt", ".nvim.lua",
  "pom.xml", "build.gradle", "build.gradle.kts",  -- Java/Kotlin
  "mix.exs", "rebar.config",                        -- Elixir/Erlang
  "setup.py", "setup.cfg", "pyproject.toml",       -- Python (duplicated for safety)
}

local cache = {}
local cache_timeout = 300

function M.find_root(start_path)
  start_path = start_path or vim.fn.expand("%:p:h")

  -- Check cache first
  if cache[start_path] then
    local age = os.time() - cache[start_path].time
    if age < cache_timeout then
      return cache[start_path].root
    end
  end

  -- Capture timestamp once for consistency
  local now = os.time()
  local current = start_path

  -- Walk up directory tree (max 20 levels to prevent infinite loops)
  for _ = 1, 20 do
    for _, marker in ipairs(root_markers) do
      local path = current .. "/" .. marker

      -- RECALIBRATION: Safe filesystem checks with error handling
      local ok_dir, is_dir = pcall(function()
        return vim.fn.isdirectory(path) == 1
      end)
      local ok_file, is_file = pcall(function()
        return vim.fn.filereadable(path) == 1
      end)

      if (ok_dir and is_dir) or (ok_file and is_file) then
        cache[start_path] = { root = current, time = now }
        return current
      end
    end

    local parent = vim.fn.fnamemodify(current, ":h")
    if parent == current or parent == "" then break end
    current = parent
  end

  -- Fallback to current working directory
  local cwd = vim.fn.getcwd()
  cache[start_path] = { root = cwd, time = now }
  return cwd
end

function M.clear_cache()
  cache = {}
end

-- RECALIBRATION: Safe DirChanged autocmd with error handling
vim.api.nvim_create_autocmd("DirChanged", {
  group    = vim.api.nvim_create_augroup("PathCacheClear", { clear = true }),
  pattern  = "*",
  callback = function()
    pcall(function() M.clear_cache() end)
  end,
  desc     = "Clear path.lua root cache on directory change",
})

return M
