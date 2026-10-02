-- lua/core/util/reload.lua — real module hot-swap (:Reload)
--
-- `:luafile module.lua` does NOT hot-swap a module: it re-runs the file but throws
-- away its return value, so every require() caller keeps the OLD table. A real
-- reload clears package.loaded[name] and requires it again. This does that
-- atomically: if any module fails to load, every module is rolled back.

local M = {}

local RESTART_ONLY = {
  ["core.bootstrap"] = "leader keys / lazy.nvim bootstrap must precede plugin loading",
  ["core.options"]   = "some options only take effect before plugins load",
}

-- Post-reload hooks for modules whose effect is a side-effect of calling them.
local AFTER = {
  ["core.highlights"] = function(m) if m.apply then m.apply() end end,
}

local function notify(msg, level) vim.notify("[reload] " .. msg, level or vim.log.levels.INFO) end

---@param name  string   module name, or "prefix.*" for a whole subtree
---@param force boolean? bypass the restart-only / plugin-spec guards
---@return boolean ok
function M.reload(name, force)
  name = type(name) == "string" and vim.trim(name) or ""
  if name == "" then notify("usage: :Reload core.util.runner  |  :Reload core.util.*", vim.log.levels.WARN); return false end

  local wildcard = name:sub(-2) == ".*"
  local base     = wildcard and name:sub(1, -3) or name

  if not force then
    if RESTART_ONLY[base] then
      notify(base .. " needs a full restart: " .. RESTART_ONLY[base] .. " (:Reload! to force)", vim.log.levels.WARN)
      return false
    end
    if base == "plugins" or vim.startswith(base, "plugins.") then
      notify("plugin specs hold lazy.nvim state — use :Lazy reload <plugin> (or :Reload! to force)", vim.log.levels.WARN)
      return false
    end
  end

  local targets = {}
  if wildcard then
    for k in pairs(package.loaded) do
      if k == base or vim.startswith(k, base .. ".") then table.insert(targets, k) end
    end
    table.sort(targets)
  else
    targets = { base }
  end
  if #targets == 0 then notify("no loaded modules match '" .. name .. "'", vim.log.levels.WARN); return false end

  local old = {}
  for _, t in ipairs(targets) do old[t] = package.loaded[t]; package.loaded[t] = nil end

  local loaded, failure = {}, nil
  for _, t in ipairs(targets) do
    local ok, res = pcall(require, t)
    if not ok then failure = { mod = t, err = res }; break end
    table.insert(loaded, t)
  end

  if failure then
    for t, m in pairs(old) do package.loaded[t] = m end   -- atomic rollback
    notify(string.format("%s failed — rolled back %d module(s):\n%s", failure.mod, #targets, tostring(failure.err)), vim.log.levels.ERROR)
    return false
  end

  for _, t in ipairs(loaded) do
    local hook = AFTER[t]
    if hook then pcall(hook, package.loaded[t]) end
  end
  notify(string.format("reloaded %d module(s): %s", #loaded, table.concat(loaded, ", ")))
  return true
end

--- Completion for :Reload
---@param lead string
---@return string[]
function M.complete(lead)
  local seen, out = {}, {}
  local function add(s)
    if not seen[s] and vim.startswith(s, lead or "") then seen[s] = true; table.insert(out, s) end
  end
  for k in pairs(package.loaded) do
    if type(k) == "string" and vim.startswith(k, "core.") then
      add(k)
      local parent = k:match("^(.*)%.[^.]+$")
      if parent then add(parent .. ".*") end
    end
  end
  table.sort(out)
  return out
end

return M
