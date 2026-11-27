-- tests/run.lua — unit tests for the pure modules. Needs no plugins and no network.
--
--   nvim --headless -u NONE -l tests/run.lua
--
-- Exits 1 when any test fails. Each test is independent; a failure names the test and the assertion.

local root = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p:h:h")
vim.opt.rtp:prepend(root)

local results = { passed = 0, failed = {} }

local function case(name, fn)
  local ok, err = xpcall(fn, debug.traceback)
  if ok then
    results.passed = results.passed + 1
  else
    results.failed[#results.failed + 1] = name .. "\n    " .. tostring(err):gsub("\n", "\n    ")
  end
end

local function eq(got, want, msg)
  if not vim.deep_equal(got, want) then
    error(("%s: expected %s, got %s"):format(msg or "eq", vim.inspect(want), vim.inspect(got)), 2)
  end
end

local function truthy(v, msg)
  if not v then
    error(msg or "expected a true value", 2)
  end
end

local function tmpdir(name)
  local dir = vim.fn.tempname() .. "-" .. name
  vim.fn.mkdir(dir, "p")
  return dir
end

local function touch(path)
  vim.fn.mkdir(vim.fn.fnamemodify(path, ":h"), "p")
  vim.fn.writefile({ "x" }, path)
end

-- ── core.util.cache ───────────────────────────────────────────────────────────

local cache = require("core.util.cache")

case("cache: a stored nil is a hit", function()
  local c = cache.new({ ttl = 60 })
  c:set("k", nil)
  local hit, v = c:lookup("k")
  eq(hit, true, "hit")
  eq(v, nil, "value")
end)

case("cache: a changed signature is a miss", function()
  local c = cache.new({ ttl = 60 })
  c:set("k", "v", "sig-a")
  eq((c:lookup("k", "sig-b")), false, "stale signature")
  c:set("k", "v", "sig-a") -- a mismatch evicts the entry, so set it again
  eq(select(2, c:lookup("k", "sig-a")), "v", "matching signature")
end)

case("cache: ttl 0 expires at once", function()
  local c = cache.new({ ttl = 0 })
  c:set("k", "v")
  eq((c:lookup("k")), false, "expired")
end)

case("cache: memo computes once while the file is unchanged", function()
  local c = cache.new({ ttl = 60 })
  local file = tmpdir("memo") .. "/f"
  touch(file)
  local calls = 0
  local function compute()
    calls = calls + 1
    return "result"
  end
  eq(c:memo("k", file, compute), "result")
  eq(c:memo("k", file, compute), "result")
  eq(calls, 1, "compute calls")
end)

case("cache: clear_all empties the store", function()
  local c = cache.new({ ttl = 60 })
  c:set("a", 1)
  c:clear_all()
  eq((c:lookup("a")), false, "after clear")
end)

-- ── core.util.path ────────────────────────────────────────────────────────────

local path = require("core.util.path")

case("path: a repository bounds the walk; a stray primary marker above it is ignored", function()
  local base = tmpdir("bound")
  touch(base .. "/outer/go.mod") -- stray primary above the repository
  vim.fn.mkdir(base .. "/outer/repo/.git", "p")
  vim.fn.mkdir(base .. "/outer/repo/sub", "p")
  eq(path.find_root(base .. "/outer/repo/sub"), base .. "/outer/repo", "root")
end)

case("path: a primary marker inside the repository wins over the repository root", function()
  local base = tmpdir("primary")
  vim.fn.mkdir(base .. "/repo/.git", "p")
  touch(base .. "/repo/pyproject.toml")
  vim.fn.mkdir(base .. "/repo/src", "p")
  eq(path.find_root(base .. "/repo/src"), base .. "/repo", "root")
end)

case("path: nested primary below the repository root is the project", function()
  local base = tmpdir("nested")
  vim.fn.mkdir(base .. "/repo/.git", "p")
  touch(base .. "/repo/services/api/Cargo.toml")
  vim.fn.mkdir(base .. "/repo/services/api/src", "p")
  eq(path.find_root(base .. "/repo/services/api/src"), base .. "/repo/services/api", "root")
end)

-- ── core.util.dburl (pure helpers) ────────────────────────────────────────────

local dburl = require("core.util.dburl")

case("dburl: dotenv lines: plain, export, quoted, comment, unclosed quote", function()
  eq({ dburl.parse_dotenv_line("DATABASE_URL=postgres://h/db") }, { "DATABASE_URL", "postgres://h/db" })
  eq({ dburl.parse_dotenv_line('export K="a b"') }, { "K", "a b" })
  eq({ dburl.parse_dotenv_line("K='x' # note") }, { "K", "x" })
  eq({ dburl.parse_dotenv_line("# only a comment") }, { nil, nil })
  eq({ dburl.parse_dotenv_line('K="unclosed') }, { nil, nil })
end)

case("dburl: validate_url accepts a scheme and rejects spaces, no scheme, and empty", function()
  truthy((dburl.validate_url("postgres://u@h/db")), "valid url")
  eq((dburl.validate_url("has space://x")), false, "whitespace")
  eq((dburl.validate_url("no-scheme-here")), false, "no scheme")
  eq((dburl.validate_url("")), false, "empty")
end)

case("dburl: dialect from the scheme", function()
  eq(dburl.dialect("postgresql://h/db"), "pg")
  eq(dburl.dialect("mariadb://h/db"), "mysql")
  eq(dburl.dialect("sqlite:///tmp/x.db"), "sqlite")
  eq(dburl.dialect("redis://h"), nil)
end)

-- ── core.util.vhdl (pure parser) ──────────────────────────────────────────────

local vhdl = require("core.util.vhdl")

case("vhdl: comment stripping keeps dashes inside strings and character literals", function()
  eq(vhdl.strip_comment("a <= b; -- note"), "a <= b; ")
  eq(vhdl.strip_comment('s := "--";'), 's := "--";')
  eq(vhdl.strip_comment("c <= '\"'; -- note"), "c <= '\"'; ")
end)

case("vhdl: entity ports and generics are parsed", function()
  local lines = {
    "entity counter is",
    "  generic (WIDTH : integer := 8; DEPTH : natural);",
    "  port (",
    "    clk, rst : in std_logic;",
    "    q : out std_logic_vector(WIDTH - 1 downto 0)",
    "  );",
    "end entity counter;",
  }
  local name, ports, generics, err = vhdl.parse_entity(lines)
  eq(name, "counter", "name")
  eq(err, nil, "err")
  eq(#ports, 3, "port count")
  eq(ports[1].name, "clk")
  eq(ports[3].dir, "out")
  eq(generics[1].default, "8", "default")
  eq(generics[2].default, nil, "no default")
end)

case("vhdl: placeholder values for known types, nil for unknown", function()
  eq(vhdl.placeholder("integer"), "0")
  eq(vhdl.placeholder("std_logic_vector(7 downto 0)"), "(others => '0')")
  eq(vhdl.placeholder("my_pkg.t_record"), nil)
end)

case("vhdl: an unknown generic type leaves a <value> marker and is reported", function()
  local _, unresolved = vhdl.testbench("core", {}, { { name = "CFG", typ = "my_pkg.t_cfg", default = nil } })
  eq(unresolved, { "CFG" })
end)

-- ── core.util.jsonc ───────────────────────────────────────────────────────────

local jsonc = require("core.util.jsonc")

case("jsonc: comments and trailing commas removed; // inside strings kept", function()
  local obj, err = jsonc.decode('{"url":"http://x", // c\n "a":[1,2,], /* b */ "k": 1,}')
  eq(err, nil, "err")
  eq(obj.url, "http://x", "url")
  eq(obj.a, { 1, 2 }, "array")
  eq(obj.k, 1, "k")
end)

-- ── core.util.exec ────────────────────────────────────────────────────────────

local exec = require("core.util.exec")

case("exec: build flags are cut to the allow-list and reported as changed", function()
  local clean, changed = exec.sanitize_build_flags('-Wall $(rm) "x"')
  eq(clean, "-Wall rm x", "clean")
  eq(changed, true, "changed")
end)

case("exec: clean build flags pass through unchanged", function()
  local clean, changed = exec.sanitize_build_flags("-O2 -std=c11")
  eq(clean, "-O2 -std=c11", "clean")
  eq(changed, false, "changed")
end)

-- ── core.util.platform / packages / jvm / cobol ───────────────────────────────

local platform = require("core.util.platform")
case("platform: an unknown tool gets the generic install hint", function()
  eq(platform.install_hint("definitely-not-a-tool"), "install definitely-not-a-tool")
end)

local packages = require("core.util.packages")
case("packages: the real registry is well-formed", function()
  eq(packages.validate().ok, true, "validate")
  eq(packages.bin("clangd"), "clangd", "bin")
  truthy(vim.tbl_contains(packages.get("extras"), "vale"), "extras include vale")
end)

local jvm = require("core.util.jvm")
case("jvm: every Gradle goal has a Maven counterpart", function()
  eq(jvm.validate(), {}, "issues")
end)

case("jvm: test bundles drop the non-bundle jars", function()
  eq(jvm.filter_test_bundles({ "/x/jacocoagent.jar", "/x/a.jar" }), { "/x/a.jar" })
end)

case("jvm: an empty directory has no build tool and says why", function()
  local tool, why = jvm.detect_tool(tmpdir("jvm-empty"))
  eq(tool, nil, "tool")
  truthy(type(why) == "string", "reason")
end)

local cobol = require("core.util.cobol")
case("cobol: dialect from a marker file, then from JCL, then the default", function()
  local marked = tmpdir("cob-marker")
  vim.fn.writefile({ "mf" }, marked .. "/.cobol-dialect")
  eq({ cobol.detect_dialect(marked) }, { "mf", "marker" })
  local jcl = tmpdir("cob-jcl")
  touch(jcl .. "/JOB.jcl")
  eq({ cobol.detect_dialect(jcl) }, { "ibm", "jcl" })
  eq({ cobol.detect_dialect(tmpdir("cob-none")) }, { "default", "default" })
end)

-- ── core.util.runner / core.flags ─────────────────────────────────────────────

local runner = require("core.util.runner")
case("runner: supports() is a pure filetype check", function()
  eq(runner.supports("python"), true)
  eq(runner.supports("vhdl"), true)
  eq(runner.supports("no-such-filetype"), false)
end)

case("runner: vhdl work library lives in the cache, not the project", function()
  truthy(runner.vhdl_workdir():find("vhdl-work", 1, true), "path")
  eq(vim.fn.isdirectory(runner.vhdl_workdir()), 1, "created")
end)

local flags = require("core.flags")
case("flags: defaults come back when unset; the registry is complete", function()
  eq(flags.get("format_timeout_ms", 3000), 3000)
  truthy(flags.by_name["mason_install_timeout_ms"], "registered")
  eq(flags.type_issues(), {}, "type issues")
end)

-- ── report ────────────────────────────────────────────────────────────────────

print(("tests: %d passed, %d failed"):format(results.passed, #results.failed))
if #results.failed > 0 then
  for _, f in ipairs(results.failed) do
    print("FAIL " .. f)
  end
  os.exit(1)
end
