-- lua/core/util/mason.lua — Mason path resolution helpers
--

local M = {}

-- ── Path resolution ───────────────────────────────────────────────────────────

---@param name string  binary name (e.g. "codelldb", "dlv", "debugpy")
---@return string      absolute path (may not exist — callers must check)
function M.bin(name)
  local p = vim.fn.exepath(name)
  if p ~= "" then return p end
  return vim.fn.stdpath("data") .. "/mason/bin/" .. name
end

---@param rel string  path relative to mason/packages/
---                   (e.g. "js-debug-adapter/js-debug/src/dapDebugServer.js")
---@return string
function M.pkg(rel)
  return vim.fn.stdpath("data") .. "/mason/packages/" .. rel
end

--- Return the Mason packages root directory.
---@return string
function M.packages_root()
  return vim.fn.stdpath("data") .. "/mason/packages"
end

-- ── Existence guards ──────────────────────────────────────────────────────────

---@param name string  binary name passed to M.bin()
---@return boolean
function M.bin_ok(name)
  local p = M.bin(name)
  return vim.fn.executable(p) == 1
end

---@param rel string  path relative to mason/packages/
---@return boolean
function M.script_ok(rel)
  return vim.fn.filereadable(M.pkg(rel)) == 1
end

---@param name string
---@return boolean
function M.bin_exists(name)
  return M.bin_ok(name)
end

---@param rel string
---@return boolean
function M.pkg_exists(rel)
  return M.script_ok(rel)
end

-- ── Version query ─────────────────────────────────────────────────────────────
-- Cached results live for the session — binaries do not upgrade mid-session.

local _version_cache = {}

-- Known version flags per binary name.
-- Unlisted binaries fall back to "--version".
local VERSION_FLAGS = {
  stylua     = { "--version" },
  black      = { "--version" },
  prettier   = { "--version" },
  ruff       = { "--version" },
  eslint_d   = { "--version" },
  shfmt      = { "--version" },
  gofumpt    = { "--version" },
  rubocop    = { "--version" },
  ktlint     = { "--version" },
  ["clang-format"] = { "--version" },
  fprettify  = { "--version" },
  sqlfmt     = { "--version" },
  vsg        = { "--version" },
  isort      = { "--version" },
}

---@param name string   binary name (e.g. "stylua", "black", "prettier")
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

--- Clear the version cache (useful after :MasonUpdate).
function M.clear_version_cache()
  _version_cache = {}
end

return M
