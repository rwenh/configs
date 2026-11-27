-- scripts/check_graph.lua — structural checks that stylua and the parser cannot see.
--
--   nvim --headless -u NONE -l scripts/check_graph.lua
--
-- 1. Every require of a local module (core.*, plugins.*) resolves to a file.
-- 2. Every spec file under lua/plugins/specs is imported by the plugin registry, and every registry import
--    exists. An unregistered language file is never loaded, so nothing else would notice.
-- 3. Every vim.g.<name> read is registered in lua/core/flags.lua, or is a plugin or internal name.
-- 4. Every registered flag is read somewhere. A flag with no reader is documentation without a feature.
-- Exits 1 when any check fails.

local root = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p:h:h")
local failures = {}

local function fail(msg)
  failures[#failures + 1] = msg
end

local function read(p)
  local f = io.open(p, "r")
  if not f then
    return nil
  end
  local s = f:read("*a")
  f:close()
  return s
end

local function rel(p)
  return p:sub(#root + 2)
end

local function exists(p)
  return vim.uv.fs_stat(p) ~= nil
end

-- Every Lua file the repository ships, with its text.
local files = {}
for _, p in ipairs(vim.fn.globpath(root, "**/*.lua", false, true)) do
  if not p:find("/%.git/") then
    files[#files + 1] = { path = p, text = read(p) or "" }
  end
end

local LOCAL = { core = true, plugins = true }

local function module_file(mod)
  local stem = root .. "/lua/" .. mod:gsub("%.", "/")
  if exists(stem .. ".lua") then
    return stem .. ".lua"
  end
  if exists(stem .. "/init.lua") then
    return stem .. "/init.lua"
  end
  return nil
end

-- ── 1. requires resolve ───────────────────────────────────────────────────────

for _, f in ipairs(files) do
  for mod in f.text:gmatch("require%s*[%(,]?%s*[\"']([%w_%.%-]+)[\"']") do
    local first = mod:match("^([^%.]+)")
    -- A trailing dot is a computed name, as in require("plugins.specs.lang." .. stem); it cannot be checked here.
    if LOCAL[first] and mod:sub(-1) ~= "." and not module_file(mod) then
      fail(("require(%q) in %s does not resolve to a file"):format(mod, rel(f.path)))
    end
  end
end

-- ── 2. spec files and the registry agree ──────────────────────────────────────

local registry = read(root .. "/lua/plugins/specs/init.lua") or ""
local imported = {}
for m in registry:gmatch('import%s*=%s*"([%w_%.]+)"') do
  imported[m] = true
  if not module_file(m) then
    fail(("registry imports %s, which has no file"):format(m))
  end
end

for _, p in ipairs(vim.fn.globpath(root .. "/lua/plugins/specs", "**/*.lua", false, true)) do
  local mod = rel(p):gsub("^lua/", ""):gsub("%.lua$", ""):gsub("/", ".")
  if mod ~= "plugins.specs.init" and mod ~= "plugins.specs.lang.shared" and not imported[mod] then
    fail(("%s is never imported by plugins/specs/init.lua, so it is never loaded"):format(rel(p)))
  end
end

-- ── 3 and 4. vim.g reads are registered, and registered flags are read ───────

local flags_text = read(root .. "/lua/core/flags.lua") or ""
local registered = {}
for name in flags_text:gmatch('name%s*=%s*"([%w_]+)"') do
  registered[name] = true
end

-- Names owned by plugins, by Neovim, or by this config's internals. They are not escape hatches.
local EXTERNAL_EXACT =
  { mapleader = true, maplocalleader = true, db = true, rustaceanvim = true, neovide = true, goneovim = true }
local EXTERNAL_PREFIX = {
  "loaded_",
  "_",
  "nvim_ide_",
  "nvim_session_",
  "user_emmet_",
  "matchup_",
  "tmux_navigator_",
  "undotree_",
  "gruvbox_material_",
  "vim_markdown_",
  "db_ui_",
}

local function external(name)
  if EXTERNAL_EXACT[name] then
    return true
  end
  for _, p in ipairs(EXTERNAL_PREFIX) do
    if name:sub(1, #p) == p then
      return true
    end
  end
  return false
end

local read_anywhere = {}
for _, f in ipairs(files) do
  local is_flags = f.path:sub(-#"core/flags.lua") == "core/flags.lua"
  for name in f.text:gmatch("vim%.g%.([%a_][%w_]*)") do
    if not is_flags and not external(name) and not registered[name] then
      fail(("vim.g.%s is read in %s but not registered in core/flags.lua"):format(name, rel(f.path)))
    end
    read_anywhere[name] = true
  end
  if not is_flags then
    for name in pairs(registered) do
      if f.text:find('"' .. name .. '"', 1, true) or f.text:find("vim.g." .. name, 1, true) then
        read_anywhere[name] = true
      end
    end
  end
end

for name in pairs(registered) do
  if not read_anywhere[name] then
    fail(("flag %s is registered in core/flags.lua but nothing reads it"):format(name))
  end
end

-- ── report ────────────────────────────────────────────────────────────────────

if #failures > 0 then
  table.sort(failures)
  print(("check_graph: %d problem(s)"):format(#failures))
  for _, msg in ipairs(failures) do
    print("  " .. msg)
  end
  os.exit(1)
end
print("check_graph: requires resolve, every spec is registered, every vim.g read is registered")
