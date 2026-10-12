-- tests/units.lua — pure-function checks for the small utility modules.
-- Run: nvim --headless "+luafile tests/units.lua"   (exit 1 on failure)
local fails = {}
local function check(name, cond)
  if not cond then
    table.insert(fails, name)
  end
end

-- jsonc: comments + trailing commas, strings containing // and , are preserved
local jsonc = require("core.util.jsonc")
local t = jsonc.decode('{ "a": "http://x,]", /* c */ "b": [1, 2,], // tail\n }')
check("jsonc decode", t and t.a == "http://x,]" and #t.b == 2)

-- vhdl: entity parse + testbench
local vhdl = require("core.util.vhdl")
local name, ports, gens = vhdl.parse_entity({
  "entity adder is",
  "  generic ( W : integer := 8 );",
  "  port ( a, b : in std_logic_vector(W-1 downto 0); -- comment",
  "         s : out std_logic_vector(W-1 downto 0) );",
  "end entity;",
})
check("vhdl name", name == "adder")
check("vhdl ports", #ports == 3 and ports[3].dir == "out")
check("vhdl generics", #gens == 1 and gens[1].default == "8")
check(
  "vhdl testbench",
  table.concat(vhdl.testbench(name, ports, gens), "\n"):find("uut: entity work.adder", 1, true) ~= nil
)

-- format: timeout resolution order (per-ft > global > default)
local format = require("core.util.format")
vim.g.format_timeout_ms, vim.g.format_timeout_by_ft = nil, nil
check("format default", format.timeout_ms(0) == 3000)
vim.g.format_timeout_ms = 1234
check("format global", format.timeout_ms(0) == 1234)
vim.bo.filetype = "lua"
vim.g.format_timeout_by_ft = { lua = 77 }
check("format per-ft", format.timeout_ms(0) == 77)
vim.g.format_timeout_ms, vim.g.format_timeout_by_ft = nil, nil

-- exec.sanitize_build_flags: allow-list
local exec = require("core.util.exec")
local clean, changed = exec.sanitize_build_flags("-Wall; rm -rf / $(x) `y`")
check("sanitize strips", changed and not clean:find("[;$`()]"))

-- secure: untrusted file is refused, disabled flag honoured
local secure = require("core.util.secure")
local tmp = vim.fn.tempname() .. ".lua"
vim.fn.writefile({ "return { ok = true }" }, tmp)
local ok_load = secure.load(tmp)
check("secure refuses untrusted", ok_load == false)
vim.g.disable_project_configs = true
check("secure disabled", secure.enabled() == false)
vim.g.disable_project_configs = nil

-- reload: atomic rollback on failing module
local rdir = vim.fn.tempname()
vim.fn.mkdir(rdir, "p")
vim.fn.writefile({ "return { v = 1 }" }, rdir .. "/zz_reload_t.lua")
package.path = rdir .. "/?.lua;" .. package.path
local before = require("zz_reload_t")
vim.fn.writefile({ 'error("boom")' }, rdir .. "/zz_reload_t.lua")
local ok_r = require("core.util.reload").reload("zz_reload_t")
check("reload rolls back", ok_r == false and package.loaded["zz_reload_t"] == before)

-- flags: registry types and defaults are consistent
local flags = require("core.flags")
check("flags registry non-empty", #flags.list > 20)
vim.g.format_timeout_ms = "oops"
local bad = false
for _, i in ipairs(flags.type_issues()) do
  if i:find("format_timeout_ms", 1, true) then
    bad = true
  end
end
check("flags detect wrong type", bad)
vim.g.format_timeout_ms = nil

print(("units: %d failure(s)"):format(#fails))
for _, f in ipairs(fails) do
  print("  FAIL " .. f)
end
vim.cmd(#fails == 0 and "qa!" or "cquit 1")
