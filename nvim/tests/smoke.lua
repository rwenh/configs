-- tests/smoke.lua — headless integrity test
-- Run:  nvim --headless "+luafile tests/smoke.lua"      (exit 1 on any FAULT)
-- Classifies notifications: ENV (missing external binary — fix by installing) vs FAULT (config bug).
local faults, env = {}, {}
local ENV_PAT = {
  "not found",
  "not installed",
  "MasonInstall",
  "deno",
  "fd binary",
  "fd_binary",
  "Install:",
  "Install with",
  "optional",
  "no network",
  "Could not resolve host",
  "internet connection",
  "tree%-sitter",
  "ENOENT",
}
local FAULT_PAT = {
  "failed to load",
  "setup failed",
  "not ready",
  "Installation not possible",
  "deprecated",
  "Too many rounds",
  "module '.-' not found",
  "attempt to",
  "E%d%d+:",
  "loop or previous error",
  "FIRST REAL ERROR",
  "Error detected",
}
local function classify(m)
  m = tostring(m)
  -- vim-dadbod-ui runs `silent! call remove(s:dbui_icons, 'collapsed')` (plugin/db_ui.vim:120); the swallowed
  -- E716 still lands in v:errmsg. Benign — behaviour.lua proves :DBUIToggle opens its drawer.
  if m:find('Key not present in Dictionary: "collapsed"', 1, true) then
    return "ignore"
  end
  for _, p in ipairs(FAULT_PAT) do
    if m:find(p) then
      return "fault"
    end
  end
  for _, p in ipairs(ENV_PAT) do
    if m:find(p, 1, true) then
      return "env"
    end
  end
  return "fault"
end
local function hook()
  vim.notify = function(m, l)
    if (l or vim.log.levels.INFO) < vim.log.levels.WARN then
      return
    end
    local kind = classify(m)
    if kind ~= "ignore" then
      table.insert(kind == "env" and env or faults, (tostring(m):gsub("\n", " ⏎ ")))
    end
  end
end
hook()

local function drain_errmsg(tag)
  if vim.v.errmsg and vim.v.errmsg ~= "" then
    local m = vim.v.errmsg
    vim.v.errmsg = ""
    local kind = classify(m)
    if kind ~= "ignore" then
      table.insert(kind == "env" and env or faults, ("[" .. tag .. "] " .. m:gsub("\n", " ⏎ ")))
    end
  end
end

local function finish()
  local seen = {}
  local function dump(title, t)
    print(("== %s (%d) =="):format(title, #t))
    for _, m in ipairs(t) do
      if not seen[m] then
        seen[m] = true
        print("  " .. m:sub(1, 300))
      end
    end
  end
  dump("ENV (install the tool)", env)
  dump("FAULT (config bug)", faults)
  vim.cmd(#faults == 0 and "qa!" or "cquit 1")
end

vim.defer_fn(function()
  -- 1. treesitter API + parser names
  local ok_c = pcall(require, "nvim-treesitter.configs")
  if not ok_c then
    table.insert(faults, "nvim-treesitter.configs missing — plugin not pinned to branch=master")
  end
  local ok_p, parsers = pcall(require, "nvim-treesitter.parsers")
  if ok_p then
    local avail = parsers.get_parser_configs()
    local ts = require("lazy.core.config").plugins["nvim-treesitter"]
    local opts = ts and require("lazy.core.plugin").values(ts, "opts", false) or {}
    for _, p in ipairs(type(opts.ensure_installed) == "table" and opts.ensure_installed or {}) do
      if not avail[p] then
        table.insert(faults, "treesitter parser unavailable: " .. p)
      end
    end
  end
  -- 2. load every plugin (runs every config())
  local names = {}
  for n, p in pairs(require("lazy.core.config").plugins) do
    if not p._.loaded then
      names[#names + 1] = n
    end
  end
  table.sort(names)
  for _, n in ipairs(names) do
    local ok, err = pcall(require("lazy").load, { plugins = { n } })
    hook()
    drain_errmsg("load " .. n)
    if not ok then
      table.insert(faults, "load failed: " .. n .. ": " .. tostring(err))
    end
  end
  -- 3. open one file per filetype
  local exts = {
    "lua",
    "py",
    "rs",
    "go",
    "ts",
    "tsx",
    "js",
    "java",
    "kt",
    "rb",
    "ex",
    "c",
    "cpp",
    "f90",
    "zig",
    "cob",
    "vhd",
    "html",
    "css",
    "sql",
    "md",
    "http",
    "json",
    "yaml",
    "sh",
  }
  vim.fn.mkdir(vim.fn.stdpath("cache") .. "/smoke", "p")
  for _, e in ipairs(exts) do
    if e == "http" and vim.fn.executable("tree-sitter") ~= 1 then
      table.insert(env, "skipped .http probe: kulala.nvim builds its parser with the tree-sitter CLI (not found)")
      goto next_ext
    end
    local f = vim.fn.stdpath("cache") .. "/smoke/t." .. e
    vim.fn.writefile({ "" }, f)
    pcall(vim.cmd, "silent! edit " .. vim.fn.fnameescape(f))
    vim.wait(300)
    hook()
    drain_errmsg("edit ." .. e)
    ::next_ext::
  end
  -- 3b. features that silently vanish when lazy overrides a spec field
  pcall(require("lazy").load, { plugins = { "LuaSnip", "mini.nvim" } })
  vim.wait(300)
  local okls, ls = pcall(require, "luasnip")
  if okls then
    for _, ft in ipairs({ "c", "cobol", "fortran", "html", "javascript", "vhdl" }) do
      if #ls.get_snippets(ft) == 0 then
        table.insert(faults, "no LuaSnip snippets registered for " .. ft)
      end
    end
  else
    table.insert(faults, "LuaSnip not loadable")
  end
  for _, m in ipairs({ { "gcc", "n" }, { "gsa", "x" }, { "ga", "n" } }) do
    if vim.fn.maparg(m[1], m[2]) == "" then
      table.insert(faults, "mini.nvim map missing: " .. m[1])
    end
  end
  -- 4. poisoned-require recovery
  local dir = vim.fn.stdpath("cache") .. "/smoke_mod/lua"
  vim.fn.mkdir(dir, "p")
  vim.fn.writefile({ 'error("REAL_ERR")' }, dir .. "/zz_boom.lua")
  package.path = dir .. "/?.lua;" .. package.path
  pcall(require, "zz_boom")
  local ok2, e2 = pcall(require, "zz_boom")
  if ok2 or not tostring(e2):find("loop or previous") then
    table.insert(faults, "poison probe did not poison (LuaJIT behaviour changed)")
  end
  package.loaded["zz_boom"] = nil
  local ok3, e3 = pcall(require, "zz_boom")
  if ok3 or not tostring(e3):find("REAL_ERR") then
    table.insert(faults, "poison clear did not surface real error")
  end
  vim.defer_fn(finish, 1200)
end, 1500)
