-- lua/core/lazy.lua — lazy.nvim spec loader and setup options (was lua/plugins/init.lua)
--
-- Why this lives under core.lazy: the module name `plugins` is a require-cycle and poisoned-require
-- magnet. This module builds the spec list; lua/plugins/ holds only spec files, imported by name.
--
-- The spec list is plugins/specs/init.lua, the registry. A language spec file the registry does not
-- import is never loaded. scripts/check_graph.lua reports such orphans, and scripts/gen_docs.lua reads
-- the same registry without calling lazy.nvim.
--
-- A failed registry load is fatal: M.setup() raises and init.lua reports it.
--
-- Setup is explicit: init.lua calls M.setup().
--
-- Lockfile: after :Lazy update, run :Lazy lock and commit lazy-lock.json. On a new machine, run :Lazy restore.

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
vim.opt.rtp:prepend(lazypath)

local M = {}

--- The complete spec list: the plugin registry (plugins/specs/init.lua). A registry that fails to load is
--- fatal. Failures inside individual language files are reported by lazy.nvim when it imports them.
local function collect_specs()
  local ok, registry = pcall(require, "plugins.specs")
  if not ok then
    error("cannot load the plugin registry (plugins.specs): " .. tostring(registry), 0)
  end
  if type(registry) ~= "table" then
    error("plugins.specs returned " .. type(registry) .. " (expected a table)", 0)
  end
  return registry
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
