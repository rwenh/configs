-- tests/lib/keymaps.lua — collect every keymap the config can create (shared by tests + doc generator)
local M = {}

local MODES = { "n", "v", "x", "o", "i", "t" }

function M.canon(lhs)
  lhs = lhs:gsub("<[Ll]eader>", "<Space>"):gsub("<[Ll]ocal[Ll]eader>", "<Space>")
  return vim.fn.keytrans(vim.api.nvim_replace_termcodes(lhs, true, true, true))
end

local function add(db, lhs, mode, desc, scope)
  if not lhs or lhs == "" then
    return
  end
  local key = M.canon(lhs) .. "\0" .. mode
  local e = db[key] or { lhs = M.canon(lhs), mode = mode, desc = "", scopes = {} }
  if desc and desc ~= "" and e.desc == "" then
    e.desc = desc
  end
  e.scopes[scope] = true
  db[key] = e
end

local function each_mode(m)
  if type(m) == "string" then
    return { m }
  end
  return m or { "n" }
end

--- Load all plugins, open one file per filetype inside a throw-away git repo, gather maps.
---@return table db  keyed by canon-lhs\0mode
function M.collect()
  local db = {}
  local lazy_cfg = require("lazy.core.config")
  local names = {}
  for n, p in pairs(lazy_cfg.plugins) do
    if not p._.loaded then
      names[#names + 1] = n
    end
  end
  table.sort(names)

  -- 1. lazy `keys` specs (declared maps, incl. ft-scoped ones)
  for _, p in pairs(lazy_cfg.plugins) do
    if type(p.keys) == "table" then
      for _, k in ipairs(p.keys) do
        local lhs = type(k) == "string" and k or (k.lhs or k[1])
        if type(lhs) == "string" and type(k) == "table" and k[2] ~= nil then
          local ft = k.ft
          local scope = ft and ("ft:" .. (type(ft) == "table" and table.concat(ft, ",") or ft)) or "global"
          for _, m in ipairs(each_mode(k.mode)) do
            add(db, lhs, m, k.desc, scope)
          end
        end
      end
    end
  end

  -- 2. load everything so setup()/config() register their maps
  for _, n in ipairs(names) do
    pcall(require("lazy").load, { plugins = { n } })
  end

  -- 3. open a file per filetype in a git repo (gitsigns / ft autocmds / buffer-local maps)
  local dir = vim.fn.stdpath("cache") .. "/keymap_probe"
  vim.fn.mkdir(dir, "p")
  vim.fn.system({ "git", "-C", dir, "init", "-q" })
  vim.fn.system({
    "git",
    "-C",
    dir,
    "-c",
    "user.name=t",
    "-c",
    "user.email=t@t",
    "commit",
    "--allow-empty",
    "-qm",
    "init",
  })
  vim.fn.mkdir(dir .. "/config", "p") -- Rails marker so rails-only buffer maps register
  vim.fn.writefile({ "" }, dir .. "/config/application.rb")
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
  -- gitsigns only attaches to tracked files: create + commit them first
  for _, e in ipairs(exts) do
    vim.fn.writefile({ "" }, dir .. "/t." .. e)
  end
  vim.fn.system({ "git", "-C", dir, "add", "-A" })
  vim.fn.system({ "git", "-C", dir, "-c", "user.name=t", "-c", "user.email=t@t", "commit", "-qm", "files" })
  for _, e in ipairs(exts) do
    local f = dir .. "/t." .. e
    if not (e == "http" and vim.fn.executable("tree-sitter") ~= 1) then
      pcall(vim.cmd, "silent! edit " .. vim.fn.fnameescape(f))
      vim.wait(250)
      local buf = vim.api.nvim_get_current_buf()
      local ft = vim.bo[buf].filetype
      for _, m in ipairs(MODES) do
        for _, km in ipairs(vim.api.nvim_buf_get_keymap(buf, m)) do
          add(db, km.lhs, m, km.desc, "ft:" .. ft)
        end
      end
    end
  end

  -- 4. global maps
  for _, m in ipairs(MODES) do
    for _, km in ipairs(vim.api.nvim_get_keymap(m)) do
      add(db, km.lhs, m, km.desc, "global")
    end
  end
  return db
end

return M
