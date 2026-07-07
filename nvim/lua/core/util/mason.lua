-- lua/core/util/mason.lua — Mason path resolution helpers
--

local M = {}

local function normalize(p)
  local n = vim.fn.exepath(p)
  if n ~= "" then return n end
  return vim.fn.stdpath("data") .. "/mason/bin/" .. p
end

---@param name string
---@return string
function M.bin(name)
  return normalize(name)
end

---@param rel string
---@return string
function M.pkg(rel)
  return vim.fn.stdpath("data") .. "/mason/packages/" .. rel
end

---@return string
function M.packages_root()
  return vim.fn.stdpath("data") .. "/mason/packages"
end

---@param name string
---@return boolean
function M.bin_ok(name)
  local p = M.bin(name)
  return vim.fn.executable(p) == 1
end

---@param rel string
---@return boolean
function M.script_ok(rel)
  return vim.fn.filereadable(M.pkg(rel)) == 1
end

M.bin_exists  = M.bin_ok
M.pkg_exists  = M.script_ok

-- ── Version cache ─────────────────────────────────────────────────────────────

local _version_cache = {}

local VERSION_FLAGS = {
  stylua          = { "--version" },
  black           = { "--version" },
  prettier        = { "--version" },
  ruff            = { "--version" },
  eslint_d        = { "--version" },
  shfmt           = { "--version" },
  gofumpt         = { "--version" },
  rubocop         = { "--version" },
  ktlint          = { "--version" },
  ["clang-format"]= { "--version" },
  fprettify       = { "--version" },
  sqlfmt          = { "--version" },
  vsg             = { "--version" },
  isort           = { "--version" },
}

---@param name string
---@return string|nil
function M.version(name)
  if _version_cache[name] ~= nil then return _version_cache[name] end

  local bin = M.bin(name)
  if vim.fn.executable(bin) ~= 1 then
    _version_cache[name] = nil
    return nil
  end

  local flags = VERSION_FLAGS[name] or { "--version" }
  local cmd   = { bin, unpack(flags) }
  local out   = ""

  local ok = pcall(function()
    local result = vim.fn.systemlist(cmd)
    if vim.v.shell_error == 0 and #result > 0 then
      out = vim.trim(result[1])
    end
  end)

  local ver = (ok and out ~= "") and (out:match("%d+%.%d+%.?%d*") or "installed")
    or "installed"

  _version_cache[name] = ver
  return ver
end

---@param name     string
---@param callback fun(ver: string|nil)
function M.version_async(name, callback)
  if type(callback) ~= "function" then return end

  if _version_cache[name] ~= nil then
    vim.schedule(function() callback(_version_cache[name]) end)
    return
  end

  local bin = M.bin(name)
  if vim.fn.executable(bin) ~= 1 then
    _version_cache[name] = nil
    vim.schedule(function() callback(nil) end)
    return
  end

  local flags = VERSION_FLAGS[name] or { "--version" }
  local cmd   = { bin, unpack(flags) }

  if vim.system then
    vim.system(cmd, { text = true }, function(result)
      local ver
      if result.code == 0 and result.stdout and result.stdout ~= "" then
        local out = vim.trim(result.stdout:match("([^\n]+)") or "")
        ver = out:match("%d+%.%d+%.?%d*") or "installed"
      else
        ver = "installed"
      end
      _version_cache[name] = ver
      vim.schedule(function() callback(ver) end)
    end)
  else
    -- Synchronous fallback for Neovim < 0.10 (no vim.system).
    local ver = M.version(name)
    vim.schedule(function() callback(ver) end)
  end
end

--- Clear the version cache (call after :MasonUpdate or tool upgrades).
function M.clear_version_cache()
  _version_cache = {}
end

vim.api.nvim_create_autocmd("User", {
  pattern  = "MasonUpdateCompleted",
  group    = vim.api.nvim_create_augroup("MasonVersionCacheClear", { clear = true }),
  callback = function()
    M.clear_version_cache()
    vim.notify("[mason] Version cache cleared after :MasonUpdate.", vim.log.levels.DEBUG)
  end,
  desc = "Auto-clear mason.lua version cache after :MasonUpdate completes",
})

return M
