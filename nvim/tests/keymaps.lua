-- tests/keymaps.lua — every documented keymap must exist.
-- Run: nvim --headless "+luafile tests/keymaps.lua"   (exit 1 on missing keys)
local lib = dofile(vim.fn.stdpath("config") .. "/tests/lib/keymaps.lua")

-- Keys created only when an LSP server / jdtls / DAP session attaches. They cannot be
-- exercised headless without installed servers, so they are verified statically instead
-- (the callback registering them must exist in the named source file).
local ATTACH_ONLY = {
  ["lua/plugins/specs/lsp.lua"] = {
    "gd",
    "gD",
    "gi",
    "gr",
    "K",
    "<leader>k",
    "<leader>,a",
    "<leader>,r",
    "<leader>,f",
    "<leader>,o",
    "<leader>,i",
    "<leader>,d",
    "<leader>,l",
    "<leader>,t",
    "<leader>ty",
    "]d",
    "[d",
  },
  ["lua/plugins/specs/lang/java.lua"] = {
    "<leader>jvo",
    "<leader>jvv",
    "<leader>jvc",
    "<leader>jvm",
    "<leader>jvt",
    "<leader>jvn",
    "<leader>jvR",
    "<leader>jvs",
    "<leader>jvS",
  },
  ["lua/plugins/specs/lang/c.lua"] = {},
  ["lua/plugins/specs/lang/cpp.lua"] = { "<leader>ch" }, -- clangd attach
  ["lua/plugins/specs/lang/css.lua"] = { "<leader>cv" }, -- cssls attach
}

local function read(p)
  local f = io.open(p, "r")
  if not f then
    return ""
  end
  local s = f:read("*a")
  f:close()
  return s
end

-- Parse KEYMAP_REFERENCE.md: backticked tokens in the first table column.
local function documented()
  local out, skipped = {}, 0
  for line in read(vim.fn.stdpath("config") .. "/KEYMAP_REFERENCE.md"):gmatch("[^\n]+") do
    local first = line:match("^|%s*(.-)%s*|")
    if first and first:find("`", 1, true) then
      for tok in first:gmatch("`([^`]+)`") do
        if tok:match("^<leader>") then
          table.insert(out, tok)
        elseif tok:match("^g%a+$") or tok:match("^[%[%]]%a$") or tok == "s" or tok == "-" then
          table.insert(out, tok)
        elseif tok:match("^F%d+$") then
          table.insert(out, "<" .. tok .. ">")
        elseif tok:match("^<leader") == nil and tok:match("^;") then
          skipped = skipped + 1 -- shorthand like `;o`
        end
      end
    end
  end
  return out, skipped
end

vim.defer_fn(function()
  local db = lib.collect()
  local have = {}
  for _, e in pairs(db) do
    have[e.lhs] = true
  end

  local static_ok = {}
  for src, keys in pairs(ATTACH_ONLY) do
    local text = read(vim.fn.stdpath("config") .. "/" .. src)
    for _, k in ipairs(keys) do
      local needle = k:gsub("<leader>", "<leader>")
      if text:find(needle, 1, true) or text:find(needle:gsub("^<leader>", ""), 1, true) then
        static_ok[lib.canon(k)] = true
      end
    end
  end

  local docs, skipped = documented()
  local missing, checked = {}, 0
  for _, tok in ipairs(docs) do
    checked = checked + 1
    local c = lib.canon(tok)
    if not have[c] and not static_ok[c] then
      table.insert(missing, tok)
    end
  end
  table.sort(missing)
  print(
    ("documented keys checked: %d  (shorthand skipped: %d)  collected maps: %d"):format(
      checked,
      skipped,
      vim.tbl_count(db)
    )
  )
  print(("MISSING (%d):"):format(#missing))
  for _, m in ipairs(missing) do
    print("  " .. m)
  end
  vim.cmd(#missing == 0 and "qa!" or "cquit 1")
end, 1500)
