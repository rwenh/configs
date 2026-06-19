-- lua/core/util/term.lua — shared toggleterm launch helper
--

local M = {}

-- ── Defaults ──────────────────────────────────────────────────────────────────

local DEFAULTS = {
  direction     = "float",
  close_on_exit = false,
}

-- ── Named terminal registry ───────────────────────────────────────────────────
local _registry = {}

-- ── Public helpers ─────────────────────────────────────────────────────────────

---@param root string  absolute path to the target directory
---@return string      shell fragment including trailing space, or "" on blank root
function M.cd_prefix(root)
  if not root or root == "" then return "" end
  return "cd " .. vim.fn.shellescape(root) .. " && "
end

---@param cmd  string        shell command to run (must be non-empty)
---@param opts table?        any Terminal:new() opts merged over DEFAULTS
function M.float(cmd, opts)
  if type(cmd) ~= "string" or cmd == "" then
    vim.notify("[term] cmd must be a non-empty string", vim.log.levels.ERROR)
    return
  end

  local ok_req, terminal = pcall(require, "toggleterm.terminal")
  if not ok_req then
    vim.notify("[term] toggleterm not available — cannot launch terminal",
      vim.log.levels.ERROR)
    return
  end

  local cfg = vim.tbl_extend("force", DEFAULTS, opts or {}, { cmd = cmd })

  local ok_toggle, err = pcall(function()
    terminal.Terminal:new(cfg):toggle()
  end)
  if not ok_toggle then
    vim.notify("[term] failed to open terminal: " .. tostring(err),
      vim.log.levels.ERROR)
  end
end

---@param cmd  string   bare shell command (no cd prefix needed)
---@param opts table?   forwarded to M.float
function M.float_at_root(cmd, opts)
  if type(cmd) ~= "string" or cmd == "" then
    vim.notify("[term] cmd must be a non-empty string", vim.log.levels.ERROR)
    return
  end

  local ok_path, path = pcall(require, "core.util.path")
  local root = (ok_path and path.find_root()) or vim.fn.getcwd()

  if not root or root == "" then
    vim.notify(
      "[term] float_at_root(): could not resolve project root.\n"
      .. "The command was not launched. Possible causes:\n"
      .. "  • No root markers (Cargo.toml, package.json, .git, …) found above the current file.\n"
      .. "  • vim.g.path_debug = true  shows the detection trace in :messages.",
      vim.log.levels.WARN
    )
    return
  end

  M.float(M.cd_prefix(root) .. cmd, opts)
end

-- ── Named terminal API ────────────────────────────────────────────────────────

---@param name string   registry key — any non-empty string
---@param cmd  string?  shell command; required on first call, optional thereafter
---@param opts table?   Terminal:new() overrides (merged over DEFAULTS)
function M.open(name, cmd, opts)
  if type(name) ~= "string" or name == "" then
    vim.notify("[term] open(): name must be a non-empty string", vim.log.levels.ERROR)
    return
  end

  local ok_req, terminal = pcall(require, "toggleterm.terminal")
  if not ok_req then
    vim.notify("[term] toggleterm not available", vim.log.levels.ERROR)
    return
  end

  if _registry[name] then
    local ok = pcall(function() _registry[name]:toggle() end)
    if not ok then
      _registry[name] = nil

      if type(cmd) ~= "string" or cmd == "" then
        vim.notify(
          "[term] open('" .. name .. "'): terminal became stale.\n"
          .. "Re-call M.open('" .. name .. "', '<your-command>') to restart it.",
          vim.log.levels.WARN
        )
        return
      end

      -- Safe to re-enter: cmd is now guaranteed to be a non-empty string.
      M.open(name, cmd, opts)
    end
    return
  end

  if type(cmd) ~= "string" or cmd == "" then
    vim.notify(
      "[term] open('" .. name .. "'): cmd is required for the first invocation",
      vim.log.levels.ERROR
    )
    return
  end

  local cfg = vim.tbl_extend("force", DEFAULTS, opts or {}, {
    cmd          = cmd,
    display_name = name,
    on_close     = function()
    end,
  })

  local ok_new, err = pcall(function()
    local t = terminal.Terminal:new(cfg)
    _registry[name] = t
    t:toggle()
  end)

  if not ok_new then
    vim.notify("[term] open('" .. name .. "'): " .. tostring(err), vim.log.levels.ERROR)
    _registry[name] = nil
  end
end

---Send text to a named terminal that is already running.
---@param name string   registry key of an existing named terminal
---@param text string   text to send
---@param nl   boolean? append newline (default true)
function M.send(name, text, nl)
  if type(name) ~= "string" or name == "" then
    vim.notify("[term] send(): name must be a non-empty string", vim.log.levels.ERROR)
    return
  end
  if type(text) ~= "string" then
    vim.notify("[term] send(): text must be a string", vim.log.levels.ERROR)
    return
  end

  local t = _registry[name]
  if not t then
    vim.notify(
      "[term] send(): no terminal named '" .. name .. "' — open it first with M.open()",
      vim.log.levels.WARN
    )
    return
  end

  local payload = (nl ~= false) and (text .. "\n") or text
  local ok, err = pcall(function() t:send(payload) end)
  if not ok then
    vim.notify("[term] send('" .. name .. "'): " .. tostring(err), vim.log.levels.WARN)
  end
end

---@param  name string
---@return table|nil
function M.get(name)
  return _registry[name]
end

---@param name string
function M.close(name)
  local t = _registry[name]
  if t then
    pcall(function() t:shutdown() end)
    _registry[name] = nil
  end
end

---@return string[]
function M.list()
  local names = {}
  for k in pairs(_registry) do table.insert(names, k) end
  table.sort(names)
  return names
end

return M
