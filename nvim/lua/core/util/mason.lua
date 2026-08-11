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

-- ── js-debug-adapter path resolution ──────────────────────────────────────────
--
local JS_DEBUG_CANDIDATES = {
  "js-debug-adapter/js-debug/src/dapDebugServer.js",   -- current Mason layout
  "js-debug-adapter/extension/src/dapDebugServer.js",  -- some older builds
  "js-debug-adapter/out/src/dapDebugServer.js",        -- compiled variants
  "js-debug-adapter/dist/src/dapDebugServer.js",
}

---@return string|nil  absolute path to dapDebugServer.js, or nil if not found in any known layout
---@return boolean?     true if the found path was NOT the primary/expected layout
function M.js_debug_script()
  for i, rel in ipairs(JS_DEBUG_CANDIDATES) do
    local path = M.pkg(rel)
    if vim.fn.filereadable(path) == 1 then
      return path, (i ~= 1)
    end
  end
  return nil
end

-- ── Version cache ─────────────────────────────────────────────────────────────

local _version_cache = {}

local NOT_INSTALLED = false

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
  local cached = _version_cache[name]
  if cached ~= nil then
    return cached ~= NOT_INSTALLED and cached or nil
  end

  local bin = M.bin(name)
  if vim.fn.executable(bin) ~= 1 then
    _version_cache[name] = NOT_INSTALLED
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

  local cached = _version_cache[name]
  if cached ~= nil then
    vim.schedule(function() callback(cached ~= NOT_INSTALLED and cached or nil) end)
    return
  end

  local bin = M.bin(name)
  if vim.fn.executable(bin) ~= 1 then
    _version_cache[name] = NOT_INSTALLED
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

local function register_registry_hooks()
  local ok_registry, registry = pcall(require, "mason-registry")
  if not ok_registry then return false end

  local function clear_and_notify(reason)
    return function()
      M.clear_version_cache()
      vim.notify("[mason] Version cache cleared " .. reason .. ".", vim.log.levels.DEBUG)
    end
  end

  registry:on("update:success",            clear_and_notify("after :MasonUpdate"))
  registry:on("package:install:success",   clear_and_notify("after a package install"))
  registry:on("package:uninstall:success", clear_and_notify("after a package uninstall"))
  return true
end

if not register_registry_hooks() then
  vim.api.nvim_create_autocmd("User", {
    pattern  = "LazyDone",
    once     = true,
    group    = vim.api.nvim_create_augroup("MasonVersionCacheClear", { clear = true }),
    callback = function()
      if not register_registry_hooks() then
        vim.notify(
          "[mason] mason-registry still not available after LazyDone -- "
          .. "version cache will not auto-clear after installs/updates. Call "
          .. "require('core.util.mason').clear_version_cache() manually if a "
          .. "version looks stale.",
          vim.log.levels.DEBUG
        )
      end
    end,
    desc = "Retry hooking mason-registry's events once plugins have finished loading",
  })
end

return M
