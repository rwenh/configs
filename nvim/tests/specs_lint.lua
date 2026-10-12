-- tests/specs_lint.lua — guard against lazy.nvim "last spec wins" bugs.
-- lazy merges only opts/dependencies/cmd/event/ft/keys across specs of the same plugin; every other field
-- (init, config, build, cond, version, branch, main, priority) is overridden, so earlier ones silently never run.
-- Run: nvim --headless "+luafile tests/specs_lint.lua"   (exit 1 on violation)
local OVERRIDE = { "config", "init", "build", "cond", "version", "branch", "main", "priority" }
local root = vim.fn.stdpath("config") .. "/lua/"
local by = {}
for _, path in ipairs(vim.fn.glob(root .. "plugins/specs/**/*.lua", false, true)) do
  local mod = path:sub(#root + 1):gsub("%.lua$", ""):gsub("/", ".")
  if not mod:find("%.shared$") and mod ~= "plugins.specs.init" then
    local ok, spec = pcall(require, mod)
    if ok and type(spec) == "table" then
      for _, e in ipairs(spec) do
        if type(e) == "table" and type(e[1]) == "string" then
          by[e[1]] = by[e[1]] or {}
          table.insert(by[e[1]], { mod = mod:gsub("^plugins%.specs%.", ""), e = e })
        end
      end
    elseif not ok then
      print("LOAD ERROR " .. mod .. ": " .. tostring(spec))
      vim.cmd("cquit 1")
    end
  end
end
local bad = {}
for name, list in pairs(by) do
  if #list > 1 then
    for _, fld in ipairs(OVERRIDE) do
      local holders = {}
      for _, x in ipairs(list) do
        if x.e[fld] ~= nil then
          table.insert(holders, x.mod)
        end
      end
      if #holders > 1 then
        table.insert(bad, ("%s: `%s` set in %d specs (%s)"):format(name, fld, #holders, table.concat(holders, ", ")))
      end
    end
  end
end
table.sort(bad)
print(("specs_lint: %d plugin(s) checked, %d violation(s)"):format(vim.tbl_count(by), #bad))
for _, b in ipairs(bad) do
  print("  " .. b)
end
vim.cmd(#bad == 0 and "qa!" or "cquit 1")
