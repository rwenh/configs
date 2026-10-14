-- scripts/gen_docs.lua — regenerate the code-derived sections of CHANGELOG.md so they cannot drift.
--   write: nvim --headless "+luafile scripts/gen_docs.lua"
--   check: GEN_DOCS=check nvim --headless "+luafile scripts/gen_docs.lua"   (exit 1 if stale)
-- Regions (between <!-- BEGIN GENERATED:name --> / <!-- END GENERATED:name -->):
--   flags    — escape-hatch tables from lua/core/flags.lua
--   plugins  — every plugin, grouped by phase, from the live lazy.nvim spec
local cfgdir = vim.fn.stdpath("config")
local check = vim.env.GEN_DOCS == "check"
local target = cfgdir .. "/CHANGELOG.md"

local function esc(s)
  return (tostring(s):gsub("|", "\\|"):gsub("\n", " "))
end

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

-- Phase = the wave that owns the spec file (core specs: Waves 1–2, lang specs: Wave 3).
local CORE_SPECS = {
  ui = true,
  editor = true,
  completion = true,
  treesitter = true,
  lsp = true,
  git = true,
  dap = true,
  test = true,
  advanced = true,
  hud = true,
  workflow = true,
}

local function spec_owner()
  local owner = {} -- repo -> { phase = "core"|"lang", spec = "lsp" | "lang/python" }
  local root = cfgdir .. "/lua/"
  local files = vim.fn.glob(root .. "plugins/specs/**/*.lua", false, true)
  table.sort(files)
  for _, path in ipairs(files) do
    local rel = path:sub(#root + #"plugins/specs/" + 1):gsub("%.lua$", "")
    if rel ~= "init" and rel ~= "lang/shared" then
      local ok, spec = pcall(require, "plugins.specs." .. rel:gsub("/", "."))
      if ok and type(spec) == "table" then
        local phase = CORE_SPECS[rel] and "core" or (rel:find("^lang/") and "lang" or "other")
        for _, e in ipairs(spec) do
          if type(e) == "table" and type(e[1]) == "string" then
            local prev = owner[e[1]]
            if not prev or (prev.phase == "lang" and phase == "core") then
              owner[e[1]] = { phase = phase, spec = rel }
            end
          end
        end
      end
    end
  end
  return owner
end

local function render_plugins()
  local cfg = require("lazy.core.config")
  local owner = spec_owner()
  local groups = { core = {}, lang = {}, dep = {} }
  for name, p in pairs(cfg.plugins) do
    local trig = {}
    if p.event then
      table.insert(trig, "event")
    end
    if p.cmd then
      table.insert(trig, "cmd")
    end
    if p.ft then
      table.insert(trig, "ft")
    end
    if p.keys then
      table.insert(trig, "keys")
    end
    if p.lazy == false then
      trig = { "startup" }
    end
    local o = owner[p[1] or ""]
    local row = {
      name = name,
      repo = p[1] or "",
      spec = o and o.spec or "—",
      trig = #trig > 0 and table.concat(trig, "+") or "on-require",
    }
    table.insert(groups[o and o.phase == "core" and "core" or (o and o.phase == "lang" and "lang" or "dep")], row)
  end
  local out, total = {}, 0
  local titles = {
    { "core", "Core (Waves 1–2): UI, editor, completion, LSP, treesitter, git, DAP, test, workflow" },
    { "lang", "Languages (Wave 3): per-language specs" },
    { "dep", "Dependencies (pulled in by the plugins above)" },
  }
  for _, t in ipairs(titles) do
    local rows = groups[t[1]]
    table.sort(rows, function(a, b)
      return a.name < b.name
    end)
    total = total + #rows
    table.insert(out, ("**%s** — %d"):format(t[2], #rows))
    table.insert(out, "")
    table.insert(out, "| Plugin | Repo | Spec | Loads on |")
    table.insert(out, "|--------|------|------|----------|")
    for _, r in ipairs(rows) do
      table.insert(out, ("| %s | `%s` | %s | %s |"):format(esc(r.name), esc(r.repo), r.spec, r.trig))
    end
    table.insert(out, "")
  end
  table.insert(
    out,
    1,
    ("%d plugins. `Loads on` = lazy.nvim trigger (startup / event / cmd / ft / keys / on-require)."):format(total)
  )
  table.insert(out, 2, "")
  return table.concat(out, "\n")
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

local function replace_region(text, name, body)
  local b, e = ("<!-- BEGIN GENERATED:%s -->"):format(name), ("<!-- END GENERATED:%s -->"):format(name)
  local i = text:find(b, 1, true)
  local j = text:find(e, 1, true)
  if not i or not j or j < i then
    return nil, "markers for '" .. name .. "' not found"
  end
  return text:sub(1, i + #b - 1) .. "\n" .. body .. "\n" .. text:sub(j)
end

vim.defer_fn(function()
  local text = read(target)
  if not text then
    print("gen_docs: cannot read " .. target)
    vim.cmd("cquit 2")
    return
  end
  local new = text
  for _, item in ipairs({ { "flags", render_flags() }, { "plugins", render_plugins() } }) do
    local replaced, why = replace_region(new, item[1], item[2])
    if not replaced then
      print("gen_docs: " .. why)
      vim.cmd("cquit 2")
      return
    end
    new = replaced
  end
  if check then
    print(
      new == text and "gen_docs: CHANGELOG.md generated sections up to date"
        or "  STALE: CHANGELOG.md (run scripts/gen_docs.lua)"
    )
    vim.cmd(new == text and "qa!" or "cquit 1")
  else
    local f = assert(io.open(target, "w"))
    f:write(new)
    f:close()
    print("gen_docs: CHANGELOG.md updated")
    vim.cmd("qa!")
  end
end, 1500)
