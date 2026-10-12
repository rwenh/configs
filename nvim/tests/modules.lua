-- tests/modules.lua — every Lua module under lua/ must load, and referenced core.util.* modules must exist.
-- Run: nvim --headless "+luafile tests/modules.lua"   (exit 1 on any failure)
local root = vim.fn.stdpath("config") .. "/lua/"
local failures, count = {}, 0
for _, path in ipairs(vim.fn.glob(root .. "**/*.lua", false, true)) do
  local mod = path:sub(#root + 1):gsub("%.lua$", ""):gsub("/init$", ""):gsub("/", ".")
  count = count + 1
  local ok, err = pcall(require, mod)
  if not ok then
    table.insert(failures, mod .. ": " .. tostring(err):gsub("\n", " ⏎ "):sub(1, 220))
  end
end

-- every require("core.…") written anywhere in the tree must resolve to a file
local referenced = {}
for _, path in ipairs(vim.fn.glob(vim.fn.stdpath("config") .. "/**/*.lua", false, true)) do
  if not path:find("/tests/", 1, true) then
    local f = io.open(path, "r")
    local text = f and f:read("*a") or ""
    if f then
      f:close()
    end
    for m in text:gmatch('require%("(core%.[%w_%.]+)"%)') do
      referenced[m] = true
    end
  end
end
for m in pairs(referenced) do
  local rel = m:gsub("%.", "/")
  if vim.fn.filereadable(root .. rel .. ".lua") ~= 1 and vim.fn.filereadable(root .. rel .. "/init.lua") ~= 1 then
    table.insert(failures, "referenced but missing file: " .. m)
  end
end
table.sort(failures)
print(("modules: %d loaded, %d failure(s)"):format(count, #failures))
for _, f in ipairs(failures) do
  print("  " .. f)
end
vim.cmd(#failures == 0 and "qa!" or "cquit 1")
