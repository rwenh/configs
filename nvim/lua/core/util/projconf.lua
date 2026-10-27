-- lua/core/util/projconf.lua — trust-gated loader for per-project Lua files
--
-- One loader for .runner.lua, .lspconfig.lua, .nvim-dap.lua and .overseer.lua, which were
-- four copies with four caches. Each file runs through vim.secure, so the user is asked once
-- per content hash, and the result must pass a shape check. The outcome is cached against the
-- file signature:
--   * a trusted, valid file is kept until the file or the working directory changes;
--   * a declined or invalid file is re-checked after FAIL_TTL seconds, so a later :trust is seen.
-- Nothing is read when vim.g.disable_project_configs is set.
--
-- The cache is keyed by file path, so each file is always loaded with the same validator.
-- Callers decide what to tell the user: load() never notifies.

local cache = require("core.util.cache")
local secure = require("core.util.secure")

local M = {}

local OK_TTL = math.huge
local FAIL_TTL = 30

local _store = cache.new({ name = "projconf", ttl = FAIL_TTL })

---@class ProjConf
---@field ok     boolean  true when `value` is set and passed validation
---@field absent boolean  nothing to load: no file, or project configs are disabled
---@field value  any      the validated result, when ok
---@field reason string?  why the file was not used, when neither ok nor absent

local ABSENT = { ok = false, absent = true }

--- Default shape check: the file must return a table.
---@param v any
---@return boolean ok
---@return string? reason
local function must_be_table(v)
  if type(v) == "table" then
    return true
  end
  return false, "returns " .. type(v) .. " instead of a table"
end

---@param file     string
---@param validate fun(v: any): boolean, string?
---@return ProjConf
local function evaluate(file, validate)
  local ok, value = secure.load(file)
  if not ok then
    return { ok = false, absent = false, reason = tostring(value) }
  end
  local vok, why = validate(value)
  if not vok then
    return { ok = false, absent = false, reason = why or "failed validation" }
  end
  return { ok = true, absent = false, value = value }
end

--- Load `<root>/<name>` through the trust gate. Never raises.
---@param root     string
---@param name     string
---@param validate (fun(v: any): boolean, string?)?  defaults to "must be a table"
---@return ProjConf
function M.load(root, name, validate)
  if not secure.enabled() then
    return ABSENT
  end
  local file = root .. "/" .. name
  local sig = cache.file_sig(file)
  if sig == "absent" then
    return ABSENT
  end
  local hit, res = _store:lookup(file, sig)
  if hit then
    return res
  end
  res = evaluate(file, validate or must_be_table)
  _store:set(file, res, sig, res.ok and OK_TTL or FAIL_TTL)
  return res
end

--- Forget the outcome for one project file (the next load re-checks it).
---@param root string
---@param name string
function M.forget(root, name)
  _store:clear(root .. "/" .. name)
end

return M
