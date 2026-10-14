-- tests/behaviour.lua — logic checks for modules that other tests only load:
-- path, runner, packages, commands, autocmds, theme, focus, highlights, format.
-- Run: nvim --headless "+luafile tests/behaviour.lua"   (exit 1 on failure)
local fails, ran = {}, 0
local function check(name, cond, detail)
  ran = ran + 1
  if not cond then
    table.insert(fails, name .. (detail and (" — " .. tostring(detail)) or ""))
  end
end

local function mkdir(p)
  vim.fn.mkdir(p, "p")
end
local function write(p, lines)
  mkdir(vim.fn.fnamemodify(p, ":h"))
  vim.fn.writefile(lines or { "" }, p)
end

vim.defer_fn(function()
  local tmp = vim.fn.tempname()
  mkdir(tmp)

  -- ── path ────────────────────────────────────────────────────────────────────
  local path = require("core.util.path")
  mkdir(tmp .. "/repo/.git")
  write(tmp .. "/repo/pkg/package.json", { "{}" })
  mkdir(tmp .. "/repo/pkg/src/deep")
  path.clear_cache()
  check("path: nearest package.json wins by default", path.find_root(tmp .. "/repo/pkg/src/deep") == tmp .. "/repo/pkg")
  vim.g.path_prefer_vcs = true
  path.clear_cache()
  check("path: prefer_vcs picks .git root", path.find_root(tmp .. "/repo/pkg/src/deep") == tmp .. "/repo")
  vim.g.path_prefer_vcs = nil
  path.clear_cache()
  check(
    "path: find_up finds marker",
    path.find_up({ "package.json" }, tmp .. "/repo/pkg/src/deep") == tmp .. "/repo/pkg"
  )
  check("path: find_package_root", path.find_package_root(tmp .. "/repo/pkg/src") == tmp .. "/repo/pkg")

  -- ── runner (capture the command instead of opening a terminal) ───────────────
  local term = require("core.util.term")
  local captured
  local orig_float = term.float
  term.float = function(cmd)
    captured = cmd
  end
  local runner = require("core.util.runner")

  mkdir(tmp .. "/mvn")
  write(tmp .. "/mvn/pom.xml", { "<project/>" })
  local mvn = runner.gradle_or_maven(tmp .. "/mvn", "test", "Foo")
  check("runner: maven filter uses -Dtest", mvn and mvn:find("-Dtest=", 1, true) and mvn:find("Foo", 1, true))
  mkdir(tmp .. "/gr")
  write(tmp .. "/gr/gradlew", { "#!/bin/sh" })
  vim.fn.setfperm(tmp .. "/gr/gradlew", "rwxr-xr-x")
  local gr = runner.gradle_or_maven(tmp .. "/gr", "test", "Foo")
  check("runner: gradlew filter uses --tests", gr and gr:find("./gradlew test --tests", 1, true))
  check("runner: no build tool => nil", runner.gradle_or_maven(tmp, "test") == nil)
  write(tmp .. "/y/yarn.lock")
  write(tmp .. "/p/pnpm-lock.yaml")
  check("runner: yarn detected", runner.detect_pkg_manager(tmp .. "/y") == "yarn")
  check("runner: pnpm detected", runner.detect_pkg_manager(tmp .. "/p") == "pnpm")
  check("runner: npm default", runner.detect_pkg_manager(tmp) == "npm")
  write(tmp .. "/v/vitest.config.ts")
  check("runner: vitest detected", runner.detect_js_runner(tmp .. "/v") == "vitest")

  -- run_file builds the right command for python and honours shell quoting
  local pyfile = tmp .. "/my dir/a b.py"
  write(pyfile, { "def foo():", "    return 1", "", "def bar():", "    return 2" })
  vim.cmd("silent! edit " .. vim.fn.fnameescape(pyfile))
  vim.bo.filetype = "python"
  captured = nil
  pcall(runner.run_file)
  check(
    "runner: run_file python cmd",
    captured and captured:find("a b.py", 1, true) and captured:find("'", 1, true),
    captured
  )
  -- nearest function (needs the python parser; skip quietly if absent)
  if pcall(vim.treesitter.get_parser, 0, "python") then
    vim.api.nvim_win_set_cursor(0, { 5, 4 })
    captured = nil
    pcall(runner.run_nearest_function)
    check("runner: nearest function -> pytest -k bar", captured and captured:find("pytest -k 'bar'", 1, true), captured)
  end
  -- injection safety: shell metacharacters in a filename must be escaped
  local evil = tmp .. "/ev il/$(touch pwned).py"
  write(evil, { "print(1)" })
  vim.cmd("silent! edit " .. vim.fn.fnameescape(evil))
  vim.bo.filetype = "python"
  captured = nil
  pcall(runner.run_file)
  check("runner: filename $(…) is quoted", captured and captured:find("'" .. evil .. "'", 1, true), captured)
  term.float = orig_float

  -- ── packages ────────────────────────────────────────────────────────────────
  local packages = require("core.util.packages")
  local all = packages.get("all")
  local seen, dup = {}, nil
  for _, p in ipairs(all) do
    if seen[p] then
      dup = p
    end
    seen[p] = true
  end
  check("packages: 'all' has no duplicates", dup == nil, dup)
  check("packages: validate() ok", packages.validate().ok)

  -- ── commands ────────────────────────────────────────────────────────────────
  vim.g.disable_autoformat = false
  vim.cmd("ToggleAutoformat")
  check("commands: ToggleAutoformat flips", vim.g.disable_autoformat == true)
  vim.cmd("ToggleAutoformat")
  check("commands: ToggleAutoformat flips back", vim.g.disable_autoformat == false)
  check(
    "format: on_save respects disable flag",
    (function()
      vim.g.disable_autoformat = true
      local r = require("core.util.format").on_save(vim.api.nvim_get_current_buf())
      vim.g.disable_autoformat = false
      return r == nil
    end)()
  )
  local msgs = {}
  local old_notify = vim.notify
  vim.notify = function(m)
    table.insert(msgs, tostring(m))
  end
  pcall(vim.cmd, "MasonInstallAll dry-run")
  vim.wait(500)
  vim.notify = old_notify
  check(
    "commands: MasonInstallAll dry-run reports",
    #msgs > 0 and table.concat(msgs):find("dry%-run") ~= nil,
    table.concat(msgs):sub(1, 120)
  )
  vim.wait(200)

  -- ── autocmds: trim whitespace on save, large-file flag ───────────────────────
  local txt = tmp .. "/trim.txt"
  write(txt, { "keep", "trailing   ", "tabs\t\t" })
  vim.cmd("silent! edit " .. vim.fn.fnameescape(txt))
  vim.bo.filetype = "text"
  vim.cmd("silent! write")
  local lines = vim.fn.readfile(txt)
  check("autocmds: trailing whitespace trimmed", lines[2] == "trailing" and lines[3] == "tabs", vim.inspect(lines))
  local big = tmp .. "/big.txt"
  local chunk = string.rep("x", 1000)
  local biglines = {}
  for i = 1, 600 do
    biglines[i] = chunk
  end
  write(big, biglines)
  vim.cmd("silent! edit " .. vim.fn.fnameescape(big))
  check("autocmds: >500KB flagged large_file", vim.b.large_file == true)
  vim.cmd("silent! edit " .. vim.fn.fnameescape(txt))
  check("autocmds: small file not flagged", not vim.b.large_file)

  -- ── theme ───────────────────────────────────────────────────────────────────
  local theme = require("core.theme")
  local before = vim.o.background
  theme.toggle()
  check("theme: toggle flips background", vim.o.background ~= before)
  theme.auto()
  check("theme: auto clears override", vim.g._nvim_theme_override == nil)
  check(
    "theme: switch rejects unknown",
    (function()
      local ok = pcall(theme.switch, "does-not-exist")
      return ok and theme.get_active_theme() == "tokyonight"
    end)()
  )

  -- ── highlights ──────────────────────────────────────────────────────────────
  local hl = require("core.highlights")
  for _, bg in ipairs({ "dark", "light" }) do
    vim.o.background = bg
    hl.apply()
    local g = vim.api.nvim_get_hl(0, { name = "DapBreakpoint" })
    check("highlights: DapBreakpoint set (" .. bg .. ")", g and g.fg ~= nil)
  end
  check("highlights: palettes differ", hl.palette("dark").BG ~= hl.palette("light").BG)

  -- ── database: :DBUIToggle opens its drawer when a connection is set ───────────
  vim.g.db = "sqlite:" .. tmp .. "/t.db"
  local ok_db = pcall(vim.cmd, "DBUIToggle")
  vim.wait(800)
  local drawer = false
  for _, w in ipairs(vim.api.nvim_list_wins()) do
    if vim.bo[vim.api.nvim_win_get_buf(w)].filetype == "dbui" then
      drawer = true
    end
  end
  check("database: DBUIToggle opens drawer", ok_db and drawer)
  pcall(vim.cmd, "DBUIToggle")
  vim.g.db = ""

  -- ── focus ───────────────────────────────────────────────────────────────────
  local focus = require("core.focus")
  vim.o.laststatus = 3
  vim.o.showtabline = 1
  vim.cmd("enew")
  focus.set(true)
  check("focus: active", focus.is_active())
  check("focus: strips chrome", vim.o.laststatus == 0 and vim.o.showtabline == 0)
  focus.set(false)
  vim.wait(300)
  check("focus: inactive", not focus.is_active())
  check(
    "focus: restores laststatus/showtabline",
    vim.o.laststatus == 3 and vim.o.showtabline == 1,
    ("laststatus=%s showtabline=%s"):format(vim.o.laststatus, vim.o.showtabline)
  )

  print(("behaviour: %d checks, %d failure(s)"):format(ran, #fails))
  for _, f in ipairs(fails) do
    print("  FAIL " .. f)
  end
  vim.cmd(#fails == 0 and "qa!" or "cquit 1")
end, 1500)
