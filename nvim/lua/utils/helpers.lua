-- ~/.config/nvim/lua/utils/helpers.lua

local M = {}

-- Safe module require with caching
local module_cache = {}
function M.safe_require(module, opts)
  opts = opts or {}

  if module_cache[module] then
    return module_cache[module]
  end

  local ok, result = pcall(require, module)
  if ok then
    module_cache[module] = result
    return result
  end

  _G.nvim_ide.failed[module] = { error = tostring(result), timestamp = os.time() }

  if opts.fallback then
    return opts.fallback
  end

  if not opts.silent then
    vim.schedule(function()
      vim.notify("Module '" .. module .. "' failed to load", vim.log.levels.WARN)
    end)
  end
  return nil
end

-- Cross-platform path joining
function M.join_path(...)
  if vim.fs and vim.fs.joinpath then
    return vim.fs.joinpath(...)
  end
  return table.concat({...}, vim.fn.has('win32') == 1 and '\\' or '/')
end

-- Check if command exists
function M.command_exists(cmd)
  if not cmd or cmd == "" then return false end
  return vim.fn.executable(cmd) == 1
end

-- Find project root with caching
local root_cache = {}
local root_cache_time = {}
local CACHE_TIMEOUT = 300  -- 5 minutes

function M.find_project_root()
  local start = vim.fn.expand("%:p:h")

  -- Check cache validity
  if root_cache[start] then
    local cache_age = os.time() - (root_cache_time[start] or 0)
    if cache_age < CACHE_TIMEOUT then
      return root_cache[start]
    end
  end

  local root_markers = {
    ".git", ".hg", ".svn",
    "Makefile", "CMakeLists.txt", "build.gradle", "pom.xml",
    "package.json", "Cargo.toml", "go.mod", "pyproject.toml",
    "setup.py", "requirements.txt", "Pipfile",
    ".nvim.lua", ".nvimrc", ".vscode",
    "configure.ac", "modelsim.ini",
    "deno.json", "deno.jsonc",
  }

  local current = start
  local iterations = 0
  local max_iterations = 20

  while current ~= "/" and current ~= "" and current ~= "." and iterations < max_iterations do
    for _, marker in ipairs(root_markers) do
      local path = M.join_path(current, marker)
      if vim.fn.isdirectory(path) == 1 or vim.fn.filereadable(path) == 1 then
        root_cache[start] = current
        root_cache_time[start] = os.time()
        _G.nvim_ide.project_root = current
        return current
      end
    end
    local parent = vim.fn.fnamemodify(current, ":h")
    if parent == current then break end  -- Prevent infinite loop
    current = parent
    iterations = iterations + 1
  end

  local cwd = vim.fn.getcwd()
  root_cache[start] = cwd
  root_cache_time[start] = os.time()
  _G.nvim_ide.project_root = cwd
  return cwd
end

-- Clear root cache (useful for project switching)
function M.clear_root_cache()
  root_cache = {}
  root_cache_time = {}
end

-- Check if project has file
function M.project_has_file(name)
  local root = M.find_project_root()
  local path = M.join_path(root, name)
  return vim.fn.filereadable(path) == 1
end

-- Smart buffer delete
function M.smart_buf_delete()
  local buf = vim.api.nvim_get_current_buf()

  -- Don't delete special buffers
  local buftype = vim.bo[buf].buftype
  if buftype ~= "" and buftype ~= "acwrite" then
    vim.cmd("close")
    return
  end

  if vim.bo[buf].modified then
    local choice = vim.fn.confirm("Save changes?", "&Yes\n&No\n&Cancel", 1)
    if choice == 1 then
      vim.cmd("write")
    elseif choice == 3 then
      return
    end
  end

  local buffers = vim.fn.getbufinfo({buflisted = 1})
  if #buffers <= 1 then
    vim.cmd("enew")
  end

  vim.cmd("bdelete! " .. buf)
end

-- Get language context
function M.get_language_context()
  local ft = vim.bo.filetype
  local filename = vim.fn.expand("%:t")

  return {
    filetype = ft,
    filename = filename,
    is_source_file = vim.tbl_contains({
      "python", "rust", "fortran", "sql", "vhdl", "html", "css",
      "javascript", "typescript", "java", "lua", "c", "cpp", "go"
    }, ft),
    project_root = M.find_project_root(),
  }
end

-- FIXED: Memory check using correct global variable
function M.check_memory()
  local memory_kb = collectgarbage("count")
  local memory_mb = memory_kb / 1024
  local limit = _G.nvim_ide.memory_limit_mb or 1024  -- FIXED: Use _G.nvim_ide

  if memory_mb > limit then
    vim.schedule(function()
      vim.notify(
        string.format("Memory usage high: %.1fMB (limit: %dMB) - Running GC", memory_mb, limit),
        vim.log.levels.WARN
      )
    end)
    collectgarbage("collect")

    local new_memory = collectgarbage("count") / 1024
    if new_memory > limit * 0.9 then
      module_cache = {}
      collectgarbage("collect")
      vim.schedule(function()
        vim.notify("Cleared module cache due to high memory", vim.log.levels.WARN)
      end)
    end
    return false
  end
  return true
end

-- Get memory stats
function M.get_memory_stats()
  local memory_kb = collectgarbage("count")
  local memory_mb = memory_kb / 1024
  local limit = _G.nvim_ide.memory_limit_mb or 1024

  return {
    current_mb = memory_mb,
    limit_mb = limit,
    percentage = (memory_mb / limit) * 100,
    is_healthy = memory_mb < limit * 0.8,
  }
end

-- Setup plugin helper
function M.setup_plugin(plugin_name, setup_fn)
  local plugin = M.safe_require(plugin_name, { silent = true })
  if not plugin then return false end

  local ok, err = pcall(setup_fn, plugin)
  if not ok then
    vim.schedule(function()
      vim.notify(
        "Failed to setup " .. plugin_name .. ": " .. tostring(err),
        vim.log.levels.ERROR
      )
    end)
    return false
  end
  return true
end

-- File watcher for auto-reload
function M.watch_file(filepath, callback)
  local w = vim.uv.new_fs_event()
  if not w then return nil end

  w:start(filepath, {}, vim.schedule_wrap(function(err, filename, events)
    if err then
      vim.notify("File watch error: " .. err, vim.log.levels.ERROR)
      return
    end
    callback(filename, events)
  end))

  return w
end

return M
