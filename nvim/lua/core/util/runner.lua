-- lua/core/util/runner.lua — run and check the current file, or a selection
--
-- Entry points: run_file, run_selection, syntax_check, resolve_run.
-- Every command is launched through core.util.task (overseer.nvim). This module
-- builds command strings only.
--
-- Contract: an entry returns `cmd` on success, or `nil, reason`. The reason names
-- the missing binary and a short install hint.
--
-- Reads: vim.g.runner_autosave, c_build_flags, cpp_build_flags, fortran_build_flags.
-- Per-project commands belong in .overseer.lua.

local task = require("core.util.task")
local path = require("core.util.path")
local exec = require("core.util.exec")
local cache = require("core.util.cache")
local flags = require("core.flags")
local cobol = require("core.util.cobol")
local vhdl = require("core.util.vhdl")

local M = {}

local CONFIG_TTL = 30

-- ── Helpers ───────────────────────────────────────────────────────────────────

local function sq(s)
  return vim.fn.shellescape(s)
end

local function cd(dir, cmd)
  return "cd " .. sq(dir) .. " && " .. cmd
end

--- First executable among `names`, or nil plus a reason with an install hint.
local function first_bin(names, hint)
  for _, n in ipairs(names) do
    if vim.fn.executable(n) == 1 then
      return n
    end
  end
  local msg = (#names == 1) and ("'%s' not found on PATH"):format(names[1])
    or ("none of %s found on PATH"):format(table.concat(names, ", "))
  if hint then
    msg = msg .. "\n  install: " .. hint
  end
  return nil, msg
end

--- Per-tool cache directory. Build products go here, never beside the source.
local function cache_dir(name)
  local dir = vim.fn.stdpath("cache") .. "/" .. name
  vim.fn.mkdir(dir, "p")
  return dir
end

--- Write `lines` to `file`. vim.fn.writefile raises on failure, so it runs under pcall.
local function write_lines(lines, file)
  local ok, rc = pcall(vim.fn.writefile, lines, file)
  if not ok then
    return false, (tostring(rc):gsub("^Vim:", ""))
  end
  if rc ~= 0 then
    return false, "writefile returned " .. tostring(rc)
  end
  return true
end

--- Sanitised build flags from vim.g[var]. Strips and warns on anything outside the allow-list.
local function build_flags(var, default)
  local raw = flags.get(var, default)
  local clean, changed = exec.sanitize_build_flags(raw)
  if changed then
    vim.notify(
      ("[runner] %s contained unsafe characters, stripped.\n  original : %s\n  sanitised: %s"):format(
        var,
        tostring(raw),
        clean
      ),
      vim.log.levels.WARN
    )
  end
  return clean
end

-- ── Memoised package.json ─────────────────────────────────────────────────────

local _files = cache.new({ name = "runner.files", ttl = CONFIG_TTL })

--- package.json of `root` as a table, or nil plus a reason.
local function read_package_json(root)
  local file = root .. "/package.json"
  if vim.fn.filereadable(file) ~= 1 then
    return nil, "no package.json in " .. root
  end
  return _files:memo(file, file, function()
    local ok, lines = pcall(vim.fn.readfile, file)
    if not ok then
      return nil, "cannot read " .. file
    end
    local okj, obj = pcall(vim.json.decode, table.concat(lines, "\n"))
    if not okj or type(obj) ~= "table" then
      return nil, "invalid JSON in " .. file
    end
    return obj
  end)
end

-- ── Command builders ──────────────────────────────────────────────────────────

--- Build product for `file`: a per-tool cache directory. The hash keeps two sources with one stem apart.
local function exe_for(file)
  local stem = vim.fn.fnamemodify(file, ":t:r")
  return cache_dir("native-run") .. "/" .. stem .. "-" .. vim.fn.sha256(file):sub(1, 8)
end

--- `compiler flags -o exe file && exe`. The executable goes to the cache, never beside the source.
local function compile_and_run(compiler, fl, file)
  local exe = exe_for(file)
  return ("%s %s -o %s %s && %s"):format(compiler, fl, sq(exe), sq(file), sq(exe))
end

--- Compiled language: check the compiler, sanitise flags, compile and run.
local function native(compiler, hint, var, default)
  return function(file)
    local bin, why = first_bin({ compiler }, hint)
    if not bin then
      return nil, why
    end
    return compile_and_run(bin, build_flags(var, default), file)
  end
end

--- Interpreted language: `<bin> [args] file`.
local function interpreted(bin, hint, args)
  return function(file)
    local b, why = first_bin({ bin }, hint)
    if not b then
      return nil, why
    end
    return sq(b) .. (args and (" " .. args) or "") .. " " .. sq(file)
  end
end

--- Python interpreter: the active virtualenv first, then PATH.
local function python_bin()
  local venv = vim.env.VIRTUAL_ENV
  if venv and venv ~= "" and vim.fn.executable(venv .. "/bin/python") == 1 then
    return venv .. "/bin/python"
  end
  return first_bin({ "python3", "python", "py" }, "python3")
end

-- Hints and runtime tables are defined before the functions that read them.
local JS_HINT = "nodejs (TypeScript: npm install -g tsx)"
local JDK_HINT = "java / openjdk (e.g. java-17-openjdk-devel or equivalent)"
local JS_RUNTIMES = {
  javascript = { "node" },
  javascriptreact = { "tsx", "node" },
  typescript = { "tsx", "ts-node" },
  typescriptreact = { "tsx", "ts-node" },
}

-- ── File runners (file → cmd, or nil plus reason) ─────────────────────────────

local function python_run(file)
  local py, why = python_bin()
  if not py then
    return nil, why
  end
  return sq(py) .. " " .. sq(file)
end

local function js_run(ft)
  return function(file)
    local bin, why = first_bin(JS_RUNTIMES[ft], JS_HINT)
    if not bin then
      return nil, why
    end
    return sq(bin) .. " " .. sq(file)
  end
end

local function lua_run(file)
  -- This Neovim's own runtime, so vim.* APIs resolve.
  return sq(vim.v.progpath) .. " -l " .. sq(file)
end

local function rust_run(file)
  local crate = path.find_up({ "Cargo.toml" }, vim.fn.fnamemodify(file, ":h"))
  if crate then
    local cargo, why = first_bin({ "cargo" }, "https://rustup.rs")
    if not cargo then
      return nil, why
    end
    -- A file under src/bin or examples runs as its own target. Anything else runs the crate's default binary.
    local rel = file:sub(#crate + 2)
    local bin = rel:match("^src/bin/([^/]+)%.rs$")
    local example = rel:match("^examples/([^/]+)%.rs$")
    local target = bin and (" --bin " .. sq(bin)) or (example and (" --example " .. sq(example)) or "")
    return cd(crate, cargo .. " run" .. target)
  end
  local rustc, why = first_bin({ "rustc" }, "https://rustup.rs")
  if not rustc then
    return nil, why
  end
  return compile_and_run(rustc, "", file)
end

local function java_run(file)
  -- Single-file source launcher (JDK 11+): compiles in memory, so no class files are written beside the
  -- source. The launcher also accepts a package declaration, which the old javac-then-java path did not.
  local java, why = first_bin({ "java" }, JDK_HINT)
  if not java then
    return nil, why
  end
  return sq(java) .. " " .. sq(file)
end

local function kotlin_run(file)
  local kc, why = first_bin({ "kotlinc" }, "https://kotlinlang.org/docs/command-line.html")
  if not kc then
    return nil, why
  end
  local java, why2 = first_bin({ "java" }, JDK_HINT)
  if not java then
    return nil, why2
  end
  local jar = cache_dir("kotlin-run")
    .. "/"
    .. vim.fn.fnamemodify(file, ":t:r")
    .. "-"
    .. vim.fn.sha256(file):sub(1, 8)
    .. ".jar"
  return ("%s %s -include-runtime -d %s && %s -jar %s"):format(kc, sq(file), sq(jar), java, sq(jar))
end

local function cobol_run(file)
  return cobol.run_cmd(file)
end

local function fortran_run(file)
  local gf, why = first_bin({ "gfortran" }, "gfortran / gcc-fortran")
  if not gf then
    return nil, why
  end
  local dir = cache_dir("fortran-run")
  local exe = dir .. "/" .. vim.fn.fnamemodify(file, ":t:r")
  return ("%s %s -J %s -o %s %s && %s"):format(
    gf,
    build_flags("fortran_build_flags", "-Wall"),
    sq(dir),
    sq(exe),
    sq(file),
    sq(exe)
  )
end

local function vhdl_run(file)
  local ghdl, why = first_bin({ "ghdl" }, "ghdl")
  if not ghdl then
    return nil, why
  end
  local ok, lines = pcall(vim.fn.readfile, file)
  local entity = ok and vhdl.parse_entity(lines) or nil
  if not entity then
    return sq(ghdl) .. " -s " .. sq(file) -- no entity: syntax check only
  end
  -- Work library and waveform live in the cache, so nothing is written into the project tree.
  local wd = M.vhdl_workdir()
  local vcd = wd .. "/" .. entity .. ".vcd"
  local cmd = ("%s -a --workdir=%s %s && %s -e --workdir=%s %s && %s -r --workdir=%s %s --vcd=%s"):format(
    sq(ghdl),
    sq(wd),
    sq(file),
    sq(ghdl),
    sq(wd),
    sq(entity),
    sq(ghdl),
    sq(wd),
    sq(entity),
    sq(vcd)
  )
  local gtkwave = first_bin({ "gtkwave" })
  if gtkwave then
    cmd = cmd .. " && " .. sq(gtkwave) .. " " .. sq(vcd)
  end
  return cmd
end

local RUNNERS = {
  python = python_run,
  lua = lua_run,
  javascript = js_run("javascript"),
  javascriptreact = js_run("javascriptreact"),
  typescript = js_run("typescript"),
  typescriptreact = js_run("typescriptreact"),
  go = interpreted("go", "go", "run"),
  rust = rust_run,
  c = native("gcc", "gcc", "c_build_flags", "-Wall -Wextra -g"),
  cpp = native("g++", "g++ / gcc-c++", "cpp_build_flags", "-Wall -std=c++17 -g"),
  java = java_run,
  kotlin = kotlin_run,
  sh = interpreted("bash", nil),
  bash = interpreted("bash", nil),
  ruby = interpreted("ruby", "ruby"),
  elixir = interpreted("elixir", "elixir"),
  julia = interpreted("julia", "https://julialang.org/downloads/"),
  zig = interpreted("zig", "https://ziglang.org/download/", "run"),
  nim = interpreted("nim", "https://nim-lang.org/install.html", "c -r"),
  cobol = cobol_run,
  fortran = fortran_run,
  vhdl = vhdl_run,
}

-- ── Selection interpreters (prefix only; the temp file is appended) ───────────

local function sel_bin(names, hint, args)
  return function()
    local b, why = first_bin(names, hint)
    if not b then
      return nil, why
    end
    return sq(b) .. (args and (" " .. args) or "")
  end
end

local SELECTION = {
  python = function()
    local py, why = python_bin()
    if not py then
      return nil, why
    end
    return sq(py)
  end,
  lua = function()
    return sq(vim.v.progpath) .. " -l"
  end,
  javascript = sel_bin(JS_RUNTIMES.javascript, JS_HINT),
  javascriptreact = sel_bin(JS_RUNTIMES.javascriptreact, JS_HINT),
  typescript = sel_bin(JS_RUNTIMES.typescript, JS_HINT),
  typescriptreact = sel_bin(JS_RUNTIMES.typescriptreact, JS_HINT),
  ruby = sel_bin({ "ruby" }, "ruby"),
  elixir = sel_bin({ "elixir" }, "elixir"),
  julia = sel_bin({ "julia" }, "https://julialang.org/downloads/"),
  sh = sel_bin({ "bash" }),
  bash = sel_bin({ "bash" }),
}

-- Temp-file extension per filetype, so tools that read the suffix behave.
local EXT = {
  javascript = "js",
  javascriptreact = "jsx",
  typescript = "ts",
  typescriptreact = "tsx",
  sh = "sh",
  bash = "sh",
  lua = "lua",
  python = "py",
  ruby = "rb",
  elixir = "exs",
  julia = "jl",
}

-- ── Syntax checks ─────────────────────────────────────────────────────────────

local SYNTAX = {
  c = function(file)
    local gcc, why = first_bin({ "gcc" }, "gcc")
    if not gcc then
      return nil, why
    end
    return sq(gcc) .. " -Wall -Wextra -fsyntax-only " .. sq(file)
  end,
  fortran = function(file)
    local gf, why = first_bin({ "gfortran" }, "gfortran / gcc-fortran")
    if not gf then
      return nil, why
    end
    return ("%s -fsyntax-only -J %s %s"):format(sq(gf), sq(cache_dir("fortran-run")), sq(file))
  end,
  cobol = function(file)
    return cobol.syntax_cmd(file)
  end,
  vhdl = function(file)
    local ghdl, why = first_bin({ "ghdl" }, "ghdl")
    if not ghdl then
      return nil, why
    end
    return sq(ghdl) .. " -s " .. sq(file)
  end,
}

-- ── Launch ────────────────────────────────────────────────────────────────────

local function launch(cmd, interactive)
  return task.run({ cmd = cmd, interactive = interactive })
end

-- ── Public ────────────────────────────────────────────────────────────────────

--- True when this filetype has a file runner. Pure: resolves no command and writes nothing, so it is safe
--- to call from overseer template conditions on every listing.
---@param ft string
---@return boolean
function M.supports(ft)
  return RUNNERS[ft] ~= nil
end

--- Work library and waveform directory for ghdl. Kept in the cache, out of the project tree.
---@return string
function M.vhdl_workdir()
  return cache_dir("vhdl-work")
end

--- Resolve the command to run the given file, returning cmd or nil, reason.
function M.resolve_run(file)
  local ft = vim.bo.filetype
  local runner = RUNNERS[ft]
  if not runner then
    return nil, "no runner for filetype '" .. ft .. "'"
  end
  return runner(file)
end

--- Run the current file. Autosaves a modified buffer first, unless vim.g.runner_autosave is false.
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

  local cmd, why = M.resolve_run(file)
  if not cmd then
    vim.notify("[runner] " .. vim.bo.filetype .. ": " .. why, vim.log.levels.ERROR)
    return
  end
  launch(cmd, true)
end

--- Check syntax of the current buffer without running it.
function M.syntax_check()
  local ft = vim.bo.filetype
  local file = vim.fn.expand("%:p")
  if file == "" then
    vim.notify("[runner] save the buffer first", vim.log.levels.WARN)
    return
  end
  local fn = SYNTAX[ft]
  if not fn then
    vim.notify("[runner] no syntax check for filetype '" .. ft .. "'", vim.log.levels.WARN)
    return
  end
  local cmd, why = fn(file)
  if not cmd then
    vim.notify("[runner] " .. ft .. ": " .. why, vim.log.levels.ERROR)
    return
  end
  launch(cmd, false)
end

--- Run lines start_line..end_line through the filetype's interpreter.
--- Without arguments, uses the last visual selection.
function M.run_selection(start_line, end_line)
  local ft = vim.bo.filetype
  local buf = vim.api.nvim_get_current_buf()

  if not start_line or not end_line then
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
  if start_line > end_line then
    start_line, end_line = end_line, start_line
  end

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

  -- The shell that runs the interpreter also removes the temp file, after it exits.
  local script = interp .. " " .. sq(tmp) .. "; rc=$?; rm -f " .. sq(tmp) .. "; exit $rc"
  launch("sh -c " .. sq(script), true)
end

-- ── Project package-manager helpers (used by web.lua and test.lua) ─────────────

local PACKAGE_MANAGERS = { npm = true, pnpm = true, yarn = true, bun = true }

local LOCKFILES = {
  { "bun.lockb", "bun" },
  { "bun.lock", "bun" },
  { "pnpm-lock.yaml", "pnpm" },
  { "yarn.lock", "yarn" },
  { "package-lock.json", "npm" },
}

--- Package manager: a declared `packageManager` field wins, then lockfiles, then npm.
function M.detect_pkg_manager(root)
  local pkg = read_package_json(root)
  local declared = pkg and type(pkg.packageManager) == "string" and pkg.packageManager:match("^(%a+)@") or nil
  if declared and PACKAGE_MANAGERS[declared] then
    return declared
  end
  for _, e in ipairs(LOCKFILES) do
    if vim.fn.filereadable(root .. "/" .. e[1]) == 1 then
      return e[2]
    end
  end
  return "npm"
end

--- The command that runs the project's JS test suite.
function M.detect_js_test_cmd(root)
  local pm = M.detect_pkg_manager(root)
  if pm == "bun" then
    return "bun test"
  end
  if pm == "pnpm" then
    return "pnpm test"
  end
  if pm == "yarn" then
    return "yarn test"
  end
  return "npm test"
end

return M
