-- scripts/check_docs.lua — the documentation and the lock must describe the code.
--
--   nvim --headless -u NONE -l scripts/check_docs.lua
--
-- 1. Every key in the first column of KEYMAPS.md is bound in lua/ (quoted, or as a bracket-string map).
-- 2. Every lazy-lock.json entry is used by a spec, so no pin refers to a plugin the config no longer loads.
-- 3. Every plugin repository a spec declares has a lock entry, so nothing installs at an unpinned commit.
--
-- Keys that are built at runtime, Ctrl/Alt chords, and Vim built-ins are listed explicitly below.
-- Exits 1 when any check fails.

local root = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p:h:h")
local failures = {}

local function read(p)
  local f = io.open(p, "r")
  if not f then
    return nil
  end
  local s = f:read("*a")
  f:close()
  return s
end

local src_parts = {}
for _, p in ipairs(vim.fn.globpath(root, "**/*.lua", false, true)) do
  if not p:find("/%.git/") and not p:find("/tests/") and not p:find("/scripts/check_") then
    src_parts[#src_parts + 1] = read(p) or ""
  end
end
local src = table.concat(src_parts, "\n")

-- ── 1. KEYMAPS.md keys are bound ──────────────────────────────────────────────

-- Documented shorthand that the code expresses differently or not at all.
local ALIAS = {
  ["<Backspace>"] = "<bs>", -- treesitter incremental selection
}
-- Keys another plugin owns. stay-in-place.nvim binds < and > (set_keymaps defaults to true), and this
-- config does not bind them, so the docs row is verified by the plugin, not by a literal here.
local PLUGIN_OWNED = { ["<"] = true, [">"] = true }
-- Built-in Vim keys, and keys the config reads through a table or a loop rather than as a literal.
local SKIP = {
  zo = true,
  zc = true,
  za = true, -- Vim built-ins
  jk = true,
  kj = true, -- better-escape.nvim maps j then k, not a literal
}

local function bound(lhs)
  if src:find('"' .. lhs .. '"', 1, true) or src:find("'" .. lhs .. "'", 1, true) then
    return true
  end
  -- Backslashes are doubled in Lua string literals: "<leader>\\t" is written as <leader>\t in the docs.
  local escaped = lhs:gsub("\\", "\\\\")
  if escaped ~= lhs and (src:find('"' .. escaped .. '"', 1, true) or src:find("'" .. escaped .. "'", 1, true)) then
    return true
  end
  -- Bracket-string maps such as [[<C-\>]] carry the key with no quotes around it.
  if lhs:find("<", 1, true) and src:find(lhs, 1, true) then
    return true
  end
  return false
end

local keymaps = read(root .. "/KEYMAPS.md") or ""
local checked, seen = 0, {}
local section = ""
for line in keymaps:gmatch("[^\n]+") do
  if line:match("^###? ") then
    section = line
  end
  -- "Inside Neo-tree" lists keys of the explorer window, which the plugin defines, not this config.
  local plugin_window = section:find("Inside", 1, true) ~= nil
  local token = line:match("^|%s*`([^`]+)`%s*|")
  if token and not plugin_window and not token:find("%s") and not token:find("+", 1, true) then
    local lhs = token
    if token:match("^F%d+$") then
      lhs = "<" .. token .. ">" -- the function-key table writes F5, the code writes <F5>
    end
    if lhs:match("^<leader>h%d$") or lhs:match("^<M%-%d>$") then
      lhs = nil -- built in a loop: "<leader>h" .. i and "<M-" .. i .. ">"
    end
    if lhs and not SKIP[lhs] and not PLUGIN_OWNED[lhs] and not seen[lhs] then
      seen[lhs] = true
      checked = checked + 1
      local target = ALIAS[lhs] or lhs
      if not bound(target) then
        failures[#failures + 1] = ("KEYMAPS.md documents %s, but no binding for it exists in lua/"):format(lhs)
      end
    end
  end
end

-- ── 2 and 3. lazy-lock.json and the specs agree ───────────────────────────────

local lock = vim.json.decode(read(root .. "/lazy-lock.json") or "{}")
local lock_count = 0
for name in pairs(lock) do
  lock_count = lock_count + 1
  if name ~= "lazy.nvim" and not src:find(name, 1, true) then
    failures[#failures + 1] = ("lazy-lock.json pins %s, but no spec in lua/ uses it"):format(name)
  end
end

local lower_keys = {}
for name in pairs(lock) do
  lower_keys[#lower_keys + 1] = name:lower()
end
-- Strings that look like owner/repo but name something else: file paths, labels, LSP methods.
local NOT_A_REPO = {
  ["Continue/Start"] = true,
  ["c/code"] = true,
  ["cpp/cmake"] = true,
  ["run/test"] = true,
  ["gcc/clang"] = true,
  ["workspace/didChangeConfiguration"] = true,
}
local declared = {}
for repo in src:gmatch('"([%w_%.%-]+/[%w_%.%-]+)"') do
  local last = repo:match("/(.+)$")
  local looks_like_path = repo:find("^%.")
    or last:find("%.so$")
    or last:find("%.dll$")
    or last:find("%.dylib$")
    or last:find("%.sh$")
    or last:find("%.rb$")
  if not declared[repo] and not NOT_A_REPO[repo] and not looks_like_path then
    declared[repo] = true
    local l = repo:lower()
    local pinned = false
    for _, k in ipairs(lower_keys) do
      if l:find(k, 1, true) then
        pinned = true
        break
      end
    end
    if not pinned then
      failures[#failures + 1] = ("spec declares %s, which has no lazy-lock.json entry"):format(repo)
    end
  end
end

-- ── report ────────────────────────────────────────────────────────────────────

if #failures > 0 then
  table.sort(failures)
  print(("check_docs: %d problem(s)"):format(#failures))
  for _, msg in ipairs(failures) do
    print("  " .. msg)
  end
  os.exit(1)
end
print(
  ("check_docs: %d keymap(s) bound, %d locked plugin(s) all in use, every declared repo pinned"):format(
    checked,
    lock_count
  )
)
