-- lua/core/util/path.lua - Path utilities

local M = {}

local root_markers = {
  ".git", ".git/config", ".hg", ".svn",
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

  local current = start_path
  for _ = 1, 20 do
    for _, marker in ipairs(root_markers) do
      local path = current .. "/" .. marker
      if vim.fn.isdirectory(path) == 1 or vim.fn.filereadable(path) == 1 then
        cache[start_path] = { root = current, time = os.time() }
        return current
      end
    end

    local parent = vim.fn.fnamemodify(current, ":h")
    if parent == current or parent == "" then break end
    current = parent
  end

  local cwd = vim.fn.getcwd()
  cache[start_path] = { root = cwd, time = os.time() }
  return cwd
end

function M.clear_cache()
  cache = {}
end

return M