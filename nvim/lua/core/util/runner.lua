-- lua/core/util/runner.lua — code execution engine
--
-- Entry points: run_file, run_tests, run_nearest_function, run_selection, pipe.
--
-- Contract shared by every table below:
--   * A runner, test or selection entry returns `cmd` on success, or `nil, reason`.
--     The reason names the missing binary and its install hint, so a notification is
--     actionable rather than "no runner found".
--   * Project overrides (.runner.lua) run before the built-ins. The file is trust-gated
--     through core.util.secure and memoised per (mtime, size) for CONFIG_TTL seconds.
--     A declined trust prompt is memoised as well, so it is not repeated on every run.
--     A later :trust takes effect within CONFIG_TTL seconds.
--   * Gradle/Maven task translation belongs to core.util.jvm. This module only calls it.
--   * Lua files run under this Neovim (`nvim -l`), so vim.* APIs resolve.
--   * Reads vim.g.runner_autosave, c_build_flags, cpp_build_flags, fortran_build_flags,
--     cmake_build_dir and disable_project_configs. All are registered in core/flags.lua.

local M = {}

local CONFIG_TTL = 30      -- seconds a parsed project file or a capability probe is reused
local CLEANUP_MS = 30000   -- a temp source file is removed this long after launch

-- ── Optional dependencies ─────────────────────────────────────────────────────
-- A missing module degrades only the feature it serves.

local function dep(name)
  local ok, mod = pcall(require, name)
  return ok and mod or nil
end

local term   = dep("core.util.term")
local path   = dep("core.util.path")
local exec   = dep("core.util.exec")
local secure = dep("core.util.secure")
local jvm    = dep("core.util.jvm")

-- ── Helpers ───────────────────────────────────────────────────────────────────

local function sq(s) return vim.fn.shellescape(s) end

local function root_of()
  return (path and path.find_root()) or vim.fn.getcwd()
end

local function pkg_root_of()
  return (path and path.find_package_root()) or root_of()
end

local function cd(dir, cmd)
  return "cd " .. sq(dir) .. " && " .. cmd
end

local function is_obj(v)
  return type(v) == "table" and v or {}
end

--- Directory of `file` relative to `root`: "." at the root, "./sub" below it.
local function rel_dir(root, file)
  local dir = vim.fn.fnamemodify(file, ":h")
  if dir == root then return "." end
  if vim.startswith(dir, root .. "/") then return "." .. dir:sub(#root + 1) end
  return dir
end

--- First executable among `names`, or nil plus a reason that carries the install hint.
local function first_bin(names, hint)
  for _, n in ipairs(names) do
    if vim.fn.executable(n) == 1 then return n end
  end
  local msg = (#names == 1)
    and ("'%s' not found on PATH"):format(names[1])
    or ("none of %s found on PATH"):format(table.concat(names, ", "))
  if hint then msg = msg .. "\n  install: " .. hint end
  return nil, msg
end

-- ── Memoised file reads ───────────────────────────────────────────────────────
-- Keyed by absolute path. Reused while (mtime, size) is unchanged and the entry is
-- younger than CONFIG_TTL. Failures are memoised as well, which is what stops a
-- declined trust prompt from reappearing on every run.

local _memo = {}

local function file_sig(p)
  local st = vim.uv.fs_stat(p)
  if not st then return "absent" end
  return ("%d.%d:%d"):format(st.mtime.sec, st.mtime.nsec, st.size)
end

local function memo_file(file, compute)
  local sig, now = file_sig(file), os.time()
  local hit = _memo[file]
  if hit and hit.sig == sig and (now - hit.at) < CONFIG_TTL then
    return hit.value, hit.reason
  end
  local value, reason = compute()
  _memo[file] = { sig = sig, at = now, value = value, reason = reason }
  return value, reason
end

--- package.json of `root` as a table, or nil plus a reason.
local function read_package_json(root)
  local file = root .. "/package.json"
  if vim.fn.filereadable(file) ~= 1 then return nil, "no package.json in " .. root end
  return memo_file(file, function()
    local ok, lines = pcall(vim.fn.readfile, file)
    if not ok then return nil, "cannot read " .. file end
    local okj, obj = pcall(vim.json.decode, table.concat(lines, "\n"))
    if not okj or type(obj) ~= "table" then return nil, "invalid JSON in " .. file end
    return obj
  end)
end

-- ── Project overrides (.runner.lua) ───────────────────────────────────────────

--- `.runner.lua` for the project root as a table keyed by filetype. Returns {} when
--- the file is absent or project configs are disabled (silent: that is a choice).
--- A present-but-unusable file is reported, with the reason, on every run.
local function project_config()
  if not (secure and secure.enabled()) then return {} end
  local file = root_of() .. "/.runner.lua"
  if vim.fn.filereadable(file) ~= 1 then return {} end
  local cfg, reason = memo_file(file, function()
    local ok, result = secure.load(file)
    if not ok then return nil, tostring(result) end
    if type(result) ~= "table" then
      return nil, "returns " .. type(result) .. " instead of a table"
    end
    return result
  end)
  if cfg then return cfg end
  vim.notify("[runner] .runner.lua not used: " .. reason, vim.log.levels.WARN)
  return {}
end

--- Project override for (filetype, kind), or nil to fall through to the built-in.
--- kind is "run" (arg = file) or "test" (no arg). A function may return nil to defer.
local function project_command(ft, kind, arg)
  local entry = project_config()[ft]
  local v = type(entry) == "table" and entry[kind] or nil
  if type(v) == "function" then
    local ok, out = pcall(v, arg)
    if not ok then
      vim.notify(("[runner] .runner.lua %s.%s raised: %s"):format(ft, kind, tostring(out)), vim.log.levels.ERROR)
      return nil
    end
    v = out
  end
  if v == nil then return nil end
  if type(v) ~= "string" or v == "" then
    vim.notify(
      ("[runner] .runner.lua %s.%s must be a string or a function returning one"):format(ft, kind),
      vim.log.levels.WARN
    )
    return nil
  end
  return v
end

-- ── Temp-file writes ─────────────────────────────────────────────────────────

--- Write `lines` to `file`. vim.fn.writefile raises E482 on failure rather than
--- returning an error code, so it runs under pcall; the return code and the file on
--- disk are then checked as well. Returns true, or false with a reason.
local function write_lines(lines, file)
  local ok, rc = pcall(vim.fn.writefile, lines, file)
  if not ok then return false, (tostring(rc):gsub("^Vim:", "")) end
  if rc ~= 0 then return false, "writefile returned " .. tostring(rc) end
  if vim.fn.filereadable(file) ~= 1 then return false, "file not readable after write" end
  return true
end

-- ── Launch ────────────────────────────────────────────────────────────────────

--- Run `cmd` in a floating terminal (toggleterm), or in a split if toggleterm is absent.
local function launch(cmd, opts)
  if term then
    term.float(cmd, opts)            -- toggleterm reports its own failures
    return true
  end
  vim.cmd("new")                     -- termopen needs a fresh buffer
  local term_opts = vim.empty_dict() -- an empty Lua table is sent as a list (E475)
  if opts and opts.on_exit then term_opts.on_exit = opts.on_exit end
  if vim.fn.termopen(cmd, term_opts) <= 0 then
    vim.notify("[runner] could not start terminal for: " .. cmd, vim.log.levels.ERROR)
    return false
  end
  return true
end

-- ── Build flags & compile/interpret helpers ───────────────────────────────────

--- Sanitised build flags from vim.g[var], or nil plus a reason. Strips and warns on
--- anything outside the allow-list (see core.util.exec).
local function build_flags(var, default)
  if not exec then return nil, "core.util.exec unavailable; refusing unsanitised " .. var end
  local raw = vim.g[var] or default
  local flags, changed = exec.sanitize_build_flags(raw)
  if changed then
    vim.notify(
      ("[runner] %s contained unsafe characters that were stripped.\n  original : %s\n  sanitised: %s")
        :format(var, tostring(raw), flags),
      vim.log.levels.WARN
    )
  end
  return flags
end

--- `compiler flags -o exe file && exe`. The binary sits beside the source. A source
--- with no extension would make exe == file and overwrite it, so it gets `.out`.
local function compile_and_run(compiler, flags, file)
  local exe = vim.fn.fnamemodify(file, ":r")
  if exe == file then exe = file .. ".out" end
  return ("%s %s -o %s %s && %s"):format(compiler, flags, sq(exe), sq(file), sq(exe))
end

--- Compiled language: check the compiler, sanitise its flags, compile and run.
local function native(compiler, hint, var, default)
  return function(file)
    local bin, why = first_bin({ compiler }, hint)
    if not bin then return nil, why end
    local flags, ferr = build_flags(var, default)
    if not flags then return nil, ferr end
    return compile_and_run(bin, flags, file)
  end
end

--- Interpreted language: `<bin> [args] file`.
local function interpreted(bin, hint, args)
  return function(file)
    local b, why = first_bin({ bin }, hint)
    if not b then return nil, why end
    return sq(b) .. (args and (" " .. args) or "") .. " " .. sq(file)
  end
end

--- Python interpreter: the active virtualenv first, then PATH.
local function python_bin()
  local venv = vim.env.VIRTUAL_ENV
  if venv and venv ~= "" and vim.fn.executable(venv .. "/bin/python") == 1 then
    return venv .. "/bin/python"
  end
  return first_bin({ "python3", "python", "py" }, "sudo zypper in python3")
end

-- ── Capability probes ─────────────────────────────────────────────────────────
-- Does `py` import `mod`? Keyed by the resolved interpreter path, so two different
-- pythons on PATH are probed separately. Memoised for CONFIG_TTL seconds.

local _probe = {}

local function python_has(py, mod)
  local exe = vim.fn.exepath(py)
  if exe == "" then exe = py end
  local key, now = exe .. "\0" .. mod, os.time()
  local hit = _probe[key]
  if hit and (now - hit.at) < CONFIG_TTL then return hit.ok end
  local r = vim.system({ exe, "-c", "import " .. mod }, { text = true }):wait(5000)
  local ok = r.code == 0
  _probe[key] = { ok = ok, at = now }
  return ok
end

-- ── JavaScript / TypeScript runtimes ──────────────────────────────────────────

local JS_HINT = "sudo zypper in nodejs   (TypeScript: npm install -g tsx)"

local JS_RUNTIMES = {
  javascript      = { "node" },
  javascriptreact = { "tsx", "node" },
  typescript      = { "tsx", "ts-node" },
  typescriptreact = { "tsx", "ts-node" },
}

local JDK_HINT = "sudo zypper in java-17-openjdk-devel"

-- ── File runners ──────────────────────────────────────────────────────────────
-- Each takes the file and returns cmd, or nil plus reason.

local function python_run(file)
  local py, why = python_bin()
  if not py then return nil, why end
  return sq(py) .. " " .. sq(file)
end

local function js_run(ft)
  return function(file)
    local bin, why = first_bin(JS_RUNTIMES[ft], JS_HINT)
    if not bin then return nil, why end
    return sq(bin) .. " " .. sq(file)
  end
end

local function lua_run(file)
  -- This Neovim's own runtime, so vim.* APIs resolve.
  return sq(vim.v.progpath) .. " -l " .. sq(file)
end

local function rust_run(file)
  local crate = path and path.find_up({ "Cargo.toml" }, vim.fn.fnamemodify(file, ":h"))
  if crate then
    local cargo, why = first_bin({ "cargo" }, "https://rustup.rs")
    if not cargo then return nil, why end
    return cd(crate, "cargo run")
  end
  local rustc, why = first_bin({ "rustc" }, "https://rustup.rs")
  if not rustc then return nil, why end
  return compile_and_run(rustc, "", file)
end

local function java_run(file)
  local javac, why = first_bin({ "javac" }, JDK_HINT)
  if not javac then return nil, why end
  local java, why2 = first_bin({ "java" }, JDK_HINT)
  if not java then return nil, why2 end
  local dir  = vim.fn.fnamemodify(file, ":h")
  local name = vim.fn.fnamemodify(file, ":t:r")
  return ("cd %s && %s %s && %s %s"):format(sq(dir), javac, sq(file), java, sq(name))
end

local function kotlin_run(file)
  local kc, why = first_bin({ "kotlinc" }, "https://kotlinlang.org/docs/command-line.html")
  if not kc then return nil, why end
  local java, why2 = first_bin({ "java" }, JDK_HINT)
  if not java then return nil, why2 end
  local dir = vim.fn.fnamemodify(file, ":h")
  local jar = sq(vim.fn.fnamemodify(file, ":t:r") .. ".jar")
  return ("cd %s && %s %s -include-runtime -d %s && %s -jar %s"):format(
    sq(dir), kc, sq(file), jar, java, jar)
end

local function cobol_run(file)
  local cobc, why = first_bin({ "cobc" }, "sudo zypper in gnucobol")
  if not cobc then return nil, why end
  return compile_and_run(cobc, "-x", file)
end

local function vhdl_run(file)
  local ghdl, why = first_bin({ "ghdl" }, "sudo zypper in ghdl")
  if not ghdl then return nil, why end
  local entity
  local ok, lines = pcall(vim.fn.readfile, file)
  if ok then
    for _, line in ipairs(lines) do
      local _, _, name = line:lower():find("^%s*entity%s+([%w_]+)%s+is")
      if name then entity = name; break end
    end
  end
  if not entity then
    return sq(ghdl) .. " -s " .. sq(file)   -- no entity: syntax check only
  end
  local vcd = vim.fn.getcwd() .. "/" .. entity .. "_wave.vcd"
  local cmd = ("%s -a %s && %s -e %s && %s -r %s --vcd=%s"):format(
    sq(ghdl), sq(file), sq(ghdl), sq(entity), sq(ghdl), sq(entity), sq(vcd))
  local gtkwave = first_bin({ "gtkwave" })
  if gtkwave then cmd = cmd .. " && " .. sq(gtkwave) .. " " .. sq(vcd) end
  return cmd
end

local RUNNERS = {
  python          = python_run,
  lua             = lua_run,
  javascript      = js_run("javascript"),
  javascriptreact = js_run("javascriptreact"),
  typescript      = js_run("typescript"),
  typescriptreact = js_run("typescriptreact"),
  go              = interpreted("go", "sudo zypper in go", "run"),
  rust            = rust_run,
  c               = native("gcc", "sudo zypper in gcc", "c_build_flags", "-Wall -Wextra -g"),
  cpp             = native("g++", "sudo zypper in gcc-c++", "cpp_build_flags", "-Wall -std=c++17 -g"),
  java            = java_run,
  kotlin          = kotlin_run,
  sh              = interpreted("bash", nil),
  bash            = interpreted("bash", nil),
  ruby            = interpreted("ruby", "sudo zypper in ruby"),
  elixir          = interpreted("elixir", "sudo zypper in elixir"),
  julia           = interpreted("julia", "https://julialang.org/downloads/"),
  zig             = interpreted("zig", "https://ziglang.org/download/", "run"),
  nim             = interpreted("nim", "https://nim-lang.org/install.html", "c -r"),
  cobol           = cobol_run,
  fortran         = native("gfortran", "sudo zypper in gcc-fortran", "fortran_build_flags", "-Wall"),
  vhdl            = vhdl_run,
}

-- ── Selection interpreters ────────────────────────────────────────────────────
-- Each returns the command prefix (the temp file is appended), or nil plus reason.

local function sel_bin(names, hint, args)
  return function()
    local b, why = first_bin(names, hint)
    if not b then return nil, why end
    return b .. (args and (" " .. args) or "")
  end
end

local SELECTION = {
  python = function()
    local py, why = python_bin()
    if not py then return nil, why end
    return sq(py)
  end,
  lua             = function() return sq(vim.v.progpath) .. " -l" end,
  javascript      = sel_bin(JS_RUNTIMES.javascript, JS_HINT),
  javascriptreact = sel_bin(JS_RUNTIMES.javascriptreact, JS_HINT),
  typescript      = sel_bin(JS_RUNTIMES.typescript, JS_HINT),
  typescriptreact = sel_bin(JS_RUNTIMES.typescriptreact, JS_HINT),
  ruby            = sel_bin({ "ruby" }, "sudo zypper in ruby"),
  elixir          = sel_bin({ "elixir" }, "sudo zypper in elixir"),
  julia           = sel_bin({ "julia" }, "https://julialang.org/downloads/"),
  sh              = sel_bin({ "bash" }),
  bash            = sel_bin({ "bash" }),
}

-- Temp-file extension per filetype, so tools that look at the suffix behave.
local EXT = {
  javascript = "js", javascriptreact = "jsx", typescript = "ts", typescriptreact = "tsx",
  sh = "sh", bash = "sh", lua = "lua", python = "py", ruby = "rb", elixir = "exs", julia = "jl",
}

-- ── Nearest-function lookup (tree-sitter) ─────────────────────────────────────
-- Names come from the tree-sitter `name` FIELD, which is what makes Go methods
-- (field_identifier), Rust, Ruby and JS `const f = () => {}` work.

local FUNCTION_NODE_TYPES = {
  function_definition  = true,  -- python, c, cpp
  method_definition    = true,  -- javascript, typescript
  function_declaration = true,  -- javascript, typescript, go
  method_declaration   = true,  -- go, java
  function_item        = true,  -- rust
  method               = true,  -- ruby
  singleton_method     = true,  -- ruby: def self.foo
}

local FUNCTION_VALUE_TYPES = { arrow_function = true, function_expression = true, ["function"] = true }

local function node_text(node, buf)
  local ok, txt = pcall(vim.treesitter.get_node_text, node, buf)
  return (ok and txt and txt ~= "") and txt or nil
end

local function nearest_function_name(buf)
  local ok, node = pcall(vim.treesitter.get_node, { bufnr = buf })
  if not ok or not node then return nil end
  while node do
    local t = node:type()
    local name_node
    if FUNCTION_NODE_TYPES[t] then
      name_node = node:field("name")[1]
    elseif t == "variable_declarator" then          -- const foo = () => {}
      local val = node:field("value")[1]
      if val and FUNCTION_VALUE_TYPES[val:type()] then name_node = node:field("name")[1] end
    end
    if name_node then
      local name = node_text(name_node, buf)
      if name then return name end
    end
    node = node:parent()
  end
  return nil
end

--- Rust test path: enclosing `mod` names joined with `::`, then the function name.
local function rust_qualified_name(buf, fname)
  local parts = {}
  local ok, node = pcall(vim.treesitter.get_node, { bufnr = buf })
  while ok and node do
    if node:type() == "mod_item" then
      local n = node:field("name")[1]
      local txt = n and node_text(n, buf)
      if txt then table.insert(parts, 1, txt) end
    end
    node = node:parent()
  end
  table.insert(parts, fname)
  return table.concat(parts, "::")
end

-- ── JavaScript package & test command ─────────────────────────────────────────

local PACKAGE_MANAGERS = { npm = true, pnpm = true, yarn = true, bun = true }

local LOCKFILES = {
  { "bun.lockb", "bun" }, { "bun.lock", "bun" },
  { "pnpm-lock.yaml", "pnpm" }, { "yarn.lock", "yarn" }, { "package-lock.json", "npm" },
}

local VITEST_CONFIGS = {
  "vitest.config.ts", "vitest.config.js", "vitest.config.mts", "vitest.config.mjs",
  "vitest.config.cjs", "vitest.workspace.ts", "vitest.workspace.js",
}

--- Package manager: a declared `packageManager` field (corepack) wins, then lockfiles.
function M.detect_pkg_manager(root)
  local pkg = read_package_json(root)
  local declared = pkg and type(pkg.packageManager) == "string"
    and pkg.packageManager:match("^(%a+)@") or nil
  if declared and PACKAGE_MANAGERS[declared] then return declared end
  for _, e in ipairs(LOCKFILES) do
    if vim.fn.filereadable(root .. "/" .. e[1]) == 1 then return e[2] end
  end
  return "npm"
end

--- "vitest" | "jest" | nil, from config files and package.json dependencies.
function M.detect_js_runner(root)
  for _, f in ipairs(VITEST_CONFIGS) do
    if vim.fn.filereadable(root .. "/" .. f) == 1 then return "vitest" end
  end
  local pkg = read_package_json(root)
  if not pkg then return nil end
  local deps = vim.tbl_extend("force", is_obj(pkg.dependencies), is_obj(pkg.devDependencies))
  if deps.vitest then return "vitest" end
  if deps.jest or type(pkg.jest) == "table" then return "jest" end
  return nil
end

--- The command that runs the project's test suite.
function M.detect_js_test_cmd(root)
  local pm = M.detect_pkg_manager(root)
  if pm == "bun"  then return "bun test" end
  if pm == "pnpm" then return "pnpm test" end
  if pm == "yarn" then return "yarn test" end
  return "npm test"
end

--- Why the JS test command would fail, or nil when it can run. bun needs no script.
function M.js_test_problem(root)
  if M.detect_pkg_manager(root) == "bun" then return nil end
  local pkg, why = read_package_json(root)
  if not pkg then return why end
  if type(is_obj(pkg.scripts).test) ~= "string" then
    return 'package.json has no "test" script (add one, or use bun test)'
  end
  return nil
end

-- ── Test and nearest-test entries ─────────────────────────────────────────────
-- Each takes a ctx and returns cmd or nil plus reason. ctx.nearest selects the
-- nearest-function scope; otherwise the whole suite runs.

local function test_python(ctx)
  local py, why = python_bin()
  if not py then return nil, why end
  if not python_has(py, "pytest") then
    return nil, ("pytest is not importable from %s\n  install: %s -m pip install pytest"):format(py, py)
  end
  local target = ""
  if ctx.nearest then
    target = " " .. sq(ctx.file)
    if ctx.name then target = target .. " -k " .. sq(ctx.name) .. " -v" end
  end
  return cd(ctx.root, sq(py) .. " -m pytest" .. target)
end

local function test_js(ctx)
  local root = ctx.pkg
  local problem = M.js_test_problem(root)
  if problem then return nil, problem end
  local base = M.detect_js_test_cmd(root)
  if not (ctx.nearest and ctx.name) then return cd(root, base) end
  local q = sq(ctx.name)
  local runner = M.detect_js_runner(root)
  if runner == "vitest" then return cd(root, "npx vitest run -t " .. q) end
  if runner == "jest" then return cd(root, "npx jest -t " .. q) end
  local sep = (base == "bun test") and " -t " or " -- -t "
  return cd(root, base .. sep .. q)
end

local function test_go(ctx)
  local go, why = first_bin({ "go" }, "sudo zypper in go")
  if not go then return nil, why end
  local target, run = "./...", ""
  if ctx.nearest then
    target = rel_dir(ctx.root, ctx.file)
    if ctx.name then run = " -run " .. sq("^" .. ctx.name .. "$") end
  end
  return cd(ctx.root, go .. " test " .. sq(target) .. run)
end

local function test_rust(ctx)
  local cargo, why = first_bin({ "cargo" }, "https://rustup.rs")
  if not cargo then return nil, why end
  if ctx.nearest and ctx.name then
    return cd(ctx.root, cargo .. " test " .. sq(rust_qualified_name(ctx.buf, ctx.name)))
  end
  return cd(ctx.root, cargo .. " test")
end

local function test_ruby(ctx)
  local cmd
  if vim.fn.filereadable(ctx.root .. "/Gemfile") == 1 then
    if vim.fn.executable("bundle") ~= 1 then
      return nil, "Gemfile present but bundle is not on PATH\n  install: gem install bundler"
    end
    cmd = "bundle exec rspec"
  else
    local rspec, why = first_bin({ "rspec" }, "gem install rspec")
    if not rspec then return nil, why end
    cmd = rspec
  end
  if ctx.nearest then cmd = cmd .. " " .. sq(ctx.file .. ":" .. ctx.line) end
  return cd(ctx.root, cmd)
end

local function test_mix(ctx)
  local mix, why = first_bin({ "mix" }, "sudo zypper in elixir")
  if not mix then return nil, why end
  local cmd = mix .. " test"
  if ctx.nearest then
    local root = ctx.root
    local rel = vim.startswith(ctx.file, root .. "/") and ctx.file:sub(#root + 2) or ctx.file
    cmd = cmd .. " " .. sq(rel .. ":" .. ctx.line)
  end
  return cd(ctx.root, cmd)
end

local function test_jvm(ctx)
  if not jvm then return nil, "core.util.jvm is unavailable" end
  return jvm.command(ctx.root, "test", ctx.nearest and ctx.name or nil)
end

local function test_zig(ctx)
  local zig, why = first_bin({ "zig" }, "https://ziglang.org/download/")
  if not zig then return nil, why end
  if vim.fn.filereadable(ctx.root .. "/build.zig") ~= 1 then
    return nil, "no build.zig in " .. ctx.root
  end
  return cd(ctx.root, zig .. " build test")
end

local function test_ctest(ctx)
  local ctest, why = first_bin({ "ctest" }, "sudo zypper in cmake")
  if not ctest then return nil, why end
  local build = tostring(vim.g.cmake_build_dir or "build")
  if vim.fn.isdirectory(ctx.root .. "/" .. build) ~= 1 then
    return nil, ("no %s/ directory yet — configure and build first (<leader>ccb)"):format(build)
  end
  return cd(ctx.root, ctest .. " --test-dir " .. sq(build) .. " --output-on-failure")
end

local TESTS = {
  python          = test_python,
  rust            = test_rust,
  go              = test_go,
  javascript      = test_js,
  javascriptreact = test_js,
  typescript      = test_js,
  typescriptreact = test_js,
  ruby            = test_ruby,
  elixir          = test_mix,
  java            = test_jvm,
  kotlin          = test_jvm,
  zig             = test_zig,
  c               = test_ctest,
  cpp             = test_ctest,
}

-- Filetypes where "run the nearest test" is meaningful.
local NEAREST_OK = {
  python = true, rust = true, go = true, ruby = true, elixir = true, java = true, kotlin = true,
  javascript = true, javascriptreact = true, typescript = true, typescriptreact = true,
}

local TEST_INFO = {
  fortran = "Fortran has no standard unit-test runner.\nUse <leader>ftb to build & run.",
  vhdl    = "VHDL has no standard unit-test runner.\nUse <leader>vhr (GHDL Run & View).",
  cobol   = "COBOL has no standard unit-test runner.\nUse <leader>cob to compile & run.",
}

local function test_ctx(nearest)
  local buf = vim.api.nvim_get_current_buf()
  return {
    buf     = buf,
    nearest = nearest,
    root    = root_of(),
    pkg     = pkg_root_of(),
    file    = vim.fn.expand("%:p"),
    line    = vim.fn.line("."),
    name    = nearest and nearest_function_name(buf) or nil,
  }
end

-- ── Public: run_file ──────────────────────────────────────────────────────────

function M.run_file()
  if vim.bo.buftype ~= "" then
    vim.notify("[runner] run_file: not a normal file buffer", vim.log.levels.WARN)
    return
  end
  if vim.bo.modified and vim.g.runner_autosave ~= false then
    local ok, err = pcall(vim.cmd, "write")
    if not ok then
      vim.notify("[runner] autosave failed; running the version on disk\n" .. tostring(err), vim.log.levels.WARN)
    end
  end
  local file = vim.fn.expand("%:p")
  if file == "" or vim.fn.filereadable(file) ~= 1 then
    vim.notify("[runner] no file on disk to run (save it first)", vim.log.levels.ERROR)
    return
  end

  local ft  = vim.bo.filetype
  local cmd = project_command(ft, "run", file)
  if not cmd then
    local runner = RUNNERS[ft]
    if not runner then
      vim.notify("[runner] no runner for filetype '" .. ft .. "' (add one in .runner.lua)", vim.log.levels.WARN)
      return
    end
    local why
    cmd, why = runner(file)
    if not cmd then
      vim.notify("[runner] " .. ft .. ": " .. why, vim.log.levels.ERROR)
      return
    end
  end
  launch(cmd)
end

-- ── Public: run_tests ─────────────────────────────────────────────────────────

function M.run_tests()
  local ft  = vim.bo.filetype
  local cmd = project_command(ft, "test")
  if cmd then launch(cmd); return end
  if TEST_INFO[ft] then
    vim.notify(TEST_INFO[ft], vim.log.levels.INFO)
    return
  end
  local fn = TESTS[ft]
  if not fn then
    vim.notify("[runner] no test runner for: " .. ft, vim.log.levels.WARN)
    return
  end
  local c, why = fn(test_ctx(false))
  if not c then
    vim.notify("[runner] " .. ft .. ": " .. why, vim.log.levels.ERROR)
    return
  end
  launch(c)
end

-- ── Public: run_nearest_function ──────────────────────────────────────────────
-- Scope is the nearest function when one is found. Without a name, the scope is the
-- file (python, ruby, elixir) or the package (go, JS, rust), never the whole project.

function M.run_nearest_function()
  local ft = vim.bo.filetype
  if not NEAREST_OK[ft] then
    vim.notify(
      ("[runner] nearest-test run is not available for %s; use <leader>'t for the suite"):format(ft),
      vim.log.levels.WARN
    )
    return
  end
  local ctx = test_ctx(true)
  local cmd, why = TESTS[ft](ctx)
  if not cmd then
    vim.notify("[runner] " .. ft .. ": " .. why, vim.log.levels.ERROR)
    return
  end
  if ctx.name then
    vim.notify("[runner] running nearest function: " .. ctx.name, vim.log.levels.INFO)
  end
  launch(cmd)
end

-- ── Public: run_selection ─────────────────────────────────────────────────────

function M.run_selection(start_line, end_line)
  local ft  = vim.bo.filetype
  local buf = vim.api.nvim_get_current_buf()

  if not start_line or not end_line then
    -- Called without a range: read the last visual marks.
    local lcount = vim.api.nvim_buf_line_count(buf)
    local ok_s, ms = pcall(vim.api.nvim_buf_get_mark, buf, "<")
    local ok_e, me = pcall(vim.api.nvim_buf_get_mark, buf, ">")
    local sl = ok_s and ms[1] or 0
    local el = ok_e and me[1] or 0
    if not (sl > 0 and el > 0 and sl <= lcount and el <= lcount) then
      vim.notify("[runner] no valid visual selection", vim.log.levels.WARN)
      return
    end
    start_line, end_line = sl, el
  end
  if start_line > end_line then start_line, end_line = end_line, start_line end

  local sel = SELECTION[ft]
  if not sel then
    vim.notify("[runner] no selection interpreter for: " .. ft, vim.log.levels.WARN)
    return
  end
  local interp, why = sel()
  if not interp then
    vim.notify("[runner] " .. ft .. ": " .. why, vim.log.levels.ERROR)
    return
  end

  local lines = vim.api.nvim_buf_get_lines(buf, start_line - 1, end_line, false)
  local tmp = vim.fn.tempname() .. "." .. (EXT[ft] or ft)
  local wrote, werr = write_lines(lines, tmp)
  if not wrote then
    vim.notify("[runner] could not write the selection to " .. tmp .. ": " .. werr, vim.log.levels.ERROR)
    return
  end

  local cleaned = false
  local function cleanup()
    if cleaned then return end
    cleaned = true
    pcall(os.remove, tmp)
  end
  launch(interp .. " " .. sq(tmp), { on_exit = function() vim.schedule(cleanup) end })
  vim.defer_fn(cleanup, CLEANUP_MS)
end

-- ── Public: pipe ──────────────────────────────────────────────────────────────

---@param cmd        string
---@param stdin_text string
function M.pipe(cmd, stdin_text)
  if type(cmd) ~= "string" or cmd == "" then
    vim.notify("[runner] pipe(): cmd must be non-empty", vim.log.levels.ERROR)
    return
  end
  local tmp = vim.fn.tempname()
  local wrote, werr = write_lines(vim.split(stdin_text or "", "\n", { plain = true }), tmp)
  if not wrote then
    vim.notify("[runner] pipe(): could not write stdin to " .. tmp .. ": " .. werr, vim.log.levels.ERROR)
    return
  end
  launch(cmd .. " < " .. sq(tmp), {
    on_exit = function() vim.schedule(function() pcall(os.remove, tmp) end) end,
  })
  vim.defer_fn(function() pcall(os.remove, tmp) end, CLEANUP_MS)
end

-- ── Public: Gradle/Maven (stable entry for java.lua and kotlin.lua) ───────────

--- Build command for a Gradle-named task. Delegates to core.util.jvm.
---@return string|nil cmd
---@return string|nil reason
function M.gradle_or_maven(root, task, filter)
  if not jvm then return nil, "core.util.jvm is unavailable" end
  return jvm.command(root, task, filter)
end

-- ── Self-check ────────────────────────────────────────────────────────────────

--- Internal consistency of the dispatch tables. Empty when sound.
---@return string[] issues
function M.validate()
  local issues = {}
  for ft in pairs(NEAREST_OK) do
    if not TESTS[ft] then
      table.insert(issues, "nearest-test filetype without a test entry: " .. ft)
    end
  end
  for ft in pairs(SELECTION) do
    if not RUNNERS[ft] then
      table.insert(issues, "selection interpreter without a file runner: " .. ft)
    end
  end
  return issues
end

return M
