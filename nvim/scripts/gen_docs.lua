-- scripts/gen_docs.lua — regenerate the code-derived tables in MANIFEST.md so they cannot drift.
--
--   write: nvim --headless -u NONE "+luafile scripts/gen_docs.lua"
--   check: GEN_DOCS=check nvim --headless -u NONE "+luafile scripts/gen_docs.lua"   (exit 1 if stale)
--
-- Reads the sources directly: lua/core/flags.lua for escape hatches and the plugin spec files for
-- plugins. Nothing is installed, loaded by lazy.nvim or checked for updates, so it runs on a fresh
-- checkout. Versions come from lazy-lock.json and are not part of these tables.
--
-- Regions in MANIFEST.md, between <!-- BEGIN GENERATED:name --> and <!-- END GENERATED:name -->:
--   flags    every escape hatch in lua/core/flags.lua
--   plugins  every plugin spec, grouped by phase, with its lazy.nvim trigger
--
-- Only MANIFEST.md is written. README, KEYMAPS and CHANGELOG are maintained by hand.

local root = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p:h:h")
vim.opt.rtp:prepend(root)
local target = root .. "/MANIFEST.md"
local check_only = vim.env.GEN_DOCS == "check"

local function fail(msg)
  io.stderr:write("gen_docs: " .. msg .. "\n")
  vim.cmd("cquit 2")
end

local function esc(s)
  return (tostring(s):gsub("|", "\\|"):gsub("\n", " "))
end

-- ── Flags ─────────────────────────────────────────────────────────────────────

local function render_flags()
  local flags = require("core.flags")
  local out =
    { ('%d flags — set at the top of `init.lua`, before `require("core.bootstrap")`.'):format(#flags.list), "" }
  for _, group in ipairs(flags.groups) do
    table.insert(out, "**" .. group .. "**")
    table.insert(out, "")
    table.insert(out, "| Flag | Type | Default | Effect |")
    table.insert(out, "|------|------|---------|--------|")
    for _, f in ipairs(flags.list) do
      if f.group == group then
        table.insert(out, ("| `vim.g.%s` | %s | `%s` | %s |"):format(f.name, f.type, esc(f.default), esc(f.effect)))
      end
    end
    table.insert(out, "")
  end
  return table.concat(out, "\n")
end

-- ── Plugins (from the spec sources) ───────────────────────────────────────────

-- Every module that declares plugin specs: the registry's imports, plus any language spec file the
-- registry does not import (the same rule lazy.nvim's loader applies in core/lazy.lua).
local function spec_modules()
  local mods, seen = {}, {}
  local function add(m)
    if not seen[m] then
      seen[m] = true
      table.insert(mods, m)
    end
  end
  local ok, registry = pcall(require, "plugins.specs")
  if not ok then
    fail("cannot load the spec registry: " .. tostring(registry))
  end
  for _, e in ipairs(registry) do
    if type(e) == "table" and type(e.import) == "string" then
      add(e.import)
    end
  end
  local files = vim.fn.readdir(root .. "/lua/plugins/specs/lang")
  table.sort(files)
  for _, f in ipairs(files) do
    local stem = f:match("^(.-)%.lua$")
    if stem and stem ~= "shared" then
      add("plugins.specs.lang." .. stem)
    end
  end
  return mods
end

local function short(repo)
  return repo:match("[^/]+$")
end

local TRIGGER_KEYS = { "event", "cmd", "ft", "keys" }

local function render_trigger(triggers, gated)
  local parts = {}
  if triggers.startup then
    parts = { "startup" }
  else
    for _, k in ipairs(TRIGGER_KEYS) do
      if triggers[k] then
        table.insert(parts, k)
      end
    end
    if #parts == 0 then
      parts = { "on-require" }
    end
  end
  local s = table.concat(parts, "+")
  if gated then
    s = s .. " (gated)"
  end
  return s
end

local function collect_plugins()
  local top, dep_repo = {}, {}
  for _, m in ipairs(spec_modules()) do
    local phase = m:match("^plugins%.specs%.lang%.") and "lang" or "core"
    local label = m:gsub("^plugins%.specs%.", ""):gsub("%.", "/")
    local ok, entries = pcall(require, m)
    if not ok then
      fail("cannot load " .. m .. ": " .. tostring(entries))
    end
    for _, e in ipairs(entries) do
      if type(e) == "table" and type(e[1]) == "string" then
        -- lazy names a plugin by its `name` field when one is given (the colorschemes), else by repo basename
        local name = e.name or short(e[1])
        local rec = top[name]
        if not rec then
          rec = { name = name, repo = e[1], phase = phase, spec = label, triggers = {}, gated = e.cond ~= nil }
          top[name] = rec
        else
          rec.gated = rec.gated and e.cond ~= nil
          if phase == "core" and rec.phase == "lang" then
            rec.phase, rec.spec = "core", label
          end
        end
        if e.lazy == false then
          rec.triggers.startup = true
        end
        for _, k in ipairs(TRIGGER_KEYS) do
          if e[k] ~= nil then
            rec.triggers[k] = true
          end
        end
        local deps_field = e.dependencies
        local deps_list = type(deps_field) == "string" and { deps_field } or deps_field or {}
        for _, d in ipairs(deps_list) do
          local dn = type(d) == "string" and d or (type(d) == "table" and d[1])
          if type(dn) == "string" then
            dep_repo[short(dn)] = dn
          end
        end
      end
    end
  end
  local deps = {}
  for name, repo in pairs(dep_repo) do
    if not top[name] then
      table.insert(
        deps,
        { name = name, repo = repo, phase = "dep", spec = "—", triggers = { ["on-require"] = true }, gated = false }
      )
    end
  end
  return top, deps
end

local function render_plugins()
  local top, deps = collect_plugins()
  local groups = { core = {}, lang = {}, dep = deps }
  for _, rec in pairs(top) do
    table.insert(groups[rec.phase], rec)
  end
  local titles = {
    { "core", "Core (Waves 1–2): UI, editor, completion, LSP, treesitter, git, DAP, test, workflow" },
    { "lang", "Languages (Wave 3): per-language specs" },
    { "dep", "Dependencies (pulled in by the plugins above)" },
  }
  local total = #deps
  for _, rec in pairs(top) do
    total = total + 1
  end
  local out = {}
  for _, t in ipairs(titles) do
    local rows = groups[t[1]]
    table.sort(rows, function(a, b)
      return a.name < b.name
    end)
    table.insert(out, ("**%s** — %d"):format(t[2], #rows))
    table.insert(out, "")
    table.insert(out, "| Plugin | Repo | Spec | Loads on |")
    table.insert(out, "|--------|------|------|----------|")
    for _, r in ipairs(rows) do
      table.insert(
        out,
        ("| %s | `%s` | %s | %s |"):format(esc(r.name), esc(r.repo), r.spec, render_trigger(r.triggers, r.gated))
      )
    end
    table.insert(out, "")
  end
  table.insert(
    out,
    1,
    ("%d plugins. `Loads on` = lazy.nvim trigger (startup / event / cmd / ft / keys / on-require); `gated` = installed only when its `cond` holds. lazy.nvim itself is installed by core/bootstrap.lua and is not listed."):format(
      total
    )
  )
  table.insert(out, 2, "")
  return table.concat(out, "\n")
end

-- ── Write or check ────────────────────────────────────────────────────────────

local function read(p)
  local f = io.open(p, "r")
  if not f then
    return nil
  end
  local s = f:read("*a")
  f:close()
  return s
end

local function replace_region(text, name, body)
  local b, e = ("<!-- BEGIN GENERATED:%s -->"):format(name), ("<!-- END GENERATED:%s -->"):format(name)
  local i, j = text:find(b, 1, true), text:find(e, 1, true)
  if not i or not j or j < i then
    return nil, "markers for '" .. name .. "' not found in MANIFEST.md"
  end
  return text:sub(1, i + #b - 1) .. "\n" .. body .. "\n" .. text:sub(j)
end

local function main()
  local text = read(target)
  if not text then
    fail("cannot read " .. target)
  end

  local new = text
  for _, item in ipairs({ { "flags", render_flags() }, { "plugins", render_plugins() } }) do
    local replaced, why = replace_region(new, item[1], item[2])
    if not replaced then
      fail(why)
    end
    new = replaced
  end

  if check_only then
    if new == text then
      print("gen_docs: MANIFEST.md generated sections are up to date")
      return 0
    end
    print("gen_docs: STALE: MANIFEST.md (run scripts/gen_docs.lua)")
    return 1
  end
  local f = assert(io.open(target, "w"))
  f:write(new)
  f:close()
  print("gen_docs: MANIFEST.md updated")
  return 0
end

-- Any error must end the process: a headless editor otherwise waits forever after an error in +luafile.
local ok, code = pcall(main)
if not ok then
  io.stderr:write("gen_docs: " .. tostring(code) .. "\n")
  vim.cmd("cquit 2")
end
vim.cmd(code == 0 and "qa!" or ("cquit " .. tostring(code)))
