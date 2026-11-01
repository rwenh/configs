-- lua/core/lazy.lua — lazy.nvim spec loader and setup options (was lua/plugins/init.lua)
--
-- Why this lives under core.lazy: the module name `plugins` is a require-cycle and poisoned-require
-- magnet. This module builds the spec list; lua/plugins/ holds only spec files, imported by name.
--
-- The config root is found from this file's own location, not from stdpath("config"), so the
-- repository works from any checkout. The language spec directory and the docs generator
-- The language spec directory depends on that. The docs generator finds its own root the same way.
--
-- Setup is explicit: init.lua calls M.setup(). scripts/gen_docs.lua does not use this module. It reads the
-- spec sources directly, so it runs without loading lazy.nvim. M.specs() and M.opts() stay available for
-- callers that need the spec list or options without running setup.
--
-- Lockfile: after :Lazy update, run :Lazy lock and commit lazy-lock.json. On a new machine, run :Lazy restore.

local ROOT = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p:h:h:h")
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
vim.opt.rtp:prepend(lazypath)

local M = {}

local function safe_spec(mod)
  local ok, result = pcall(require, mod)
  if not ok then
    vim.notify(string.format("[lazy] failed to load spec '%s':\n%s", mod, tostring(result)), vim.log.levels.WARN)
    return {}
  end
  if type(result) ~= "table" then
    vim.notify(string.format("[lazy] spec '%s' returned %s (expected table)", mod, type(result)), vim.log.levels.WARN)
    return {}
  end
  return result
end

-- Lang stems already imported explicitly by plugins.specs (avoid double-loading).
local function already_imported_lang_stems(core_specs)
  local stems = {}
  for _, entry in ipairs(core_specs) do
    if type(entry) == "table" and type(entry.import) == "string" then
      local stem = entry.import:match("^plugins%.specs%.lang%.([%w_]+)$")
      if stem then
        stems[stem] = true
      end
    end
  end
  return stems
end

local function collect_specs()
  local specs = {}

  local core_specs = safe_spec("plugins.specs")
  vim.list_extend(specs, core_specs)

  local already = already_imported_lang_stems(core_specs)
  local lang_path = ROOT .. "/lua/plugins/specs/lang"
  local ok_scan, entries = pcall(vim.fn.readdir, lang_path)
  if ok_scan then
    table.sort(entries)
    for _, entry in ipairs(entries) do
      local stem = entry:match("^(.-)%.lua$")
      if stem and stem ~= "shared" and not already[stem] then
        vim.list_extend(specs, safe_spec("plugins.specs.lang." .. stem))
      end
    end
  else
    vim.notify("[lazy] could not read lang spec directory: " .. lang_path, vim.log.levels.WARN)
  end

  return specs
end

--- The complete spec list: every core spec plus any language spec not imported explicitly.
---@return table[]
function M.specs()
  return collect_specs()
end

local BASE_OPTS = {
  defaults = { version = false, lazy = true },
  install = { colorscheme = { "tokyonight", "catppuccin", "habamax" } },
  checker = { enabled = true, notify = false, frequency = 86400 },
  change_detection = { enabled = true, notify = false },
  ui = {
    border = "rounded",
    icons = {
      cmd = " ",
      config = " ",
      event = " ",
      ft = " ",
      init = " ",
      imports = " ",
      keys = " ",
      plugin = " ",
      runtime = " ",
      require = " ",
      source = " ",
      start = " ",
      task = " ",
      lazy = "󰒲 ",
      not_loaded = "󰒲 ",
      loaded = "●",
      list = { "●", "➜", "★", "‒" },
    },
  },
  performance = {
    cache = { enabled = true },
    reset_packpath = true,
    rtp = {
      disabled_plugins = {
        "gzip",
        "matchit",
        "matchparen",
        "tarPlugin",
        "tohtml",
        "tutor",
        "zipPlugin",
        "2html_plugin",
        "getscript",
        "getscriptPlugin",
        "vimball",
        "vimballPlugin",
        "rrhelper",
        "logiPat",
        "spellfile_plugin",
      },
    },
  },
}

--- lazy.nvim setup options, with `overrides` merged over the defaults.
---@param overrides table?
---@return table
function M.opts(overrides)
  return vim.tbl_deep_extend("force", BASE_OPTS, overrides or {})
end

--- Run lazy.nvim setup with the full spec list.
---@param overrides table?
function M.setup(overrides)
  require("lazy").setup(collect_specs(), M.opts(overrides))
end

return M
