-- lua/plugins/init.lua — lazy.nvim spec loader
--
-- LOCKFILE GUIDANCE ─────────────────────────────────────────────────────────
--   1. After a clean install or after :Lazy update, run:
--        :Lazy lock
--
--   2. Commit lazy-lock.json to your config repository so the lockfile
--      travels with your dotfiles.
--
--   3. On a new machine (or after git pull), restore exact commits with:
--
--        :Lazy restore
--
--   4. To deliberately update and re-lock:
--
--        :Lazy update   → pulls latest commits
--        :Lazy lock     → re-pins to the new commits
--        (test, then commit the updated lazy-lock.json)
--
--   5. Alternatively, per-plugin version pins (version = "^5", "1.*" etc.)
--      can be used for critical plugins whose authors provide semver tags.
--
-- ────────────────────────────────────────────────────────────────────────────

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
vim.opt.rtp:prepend(lazypath)

-- ── Spec loader ──────────────────────────────────────────────────────────────

local SPEC_DIRS = {
  "plugins.specs",
  "plugins.specs.lang",
}

local function safe_spec(mod)
  local ok, result = pcall(require, mod)
  if not ok then
    vim.notify(
      string.format("[plugins] failed to load spec '%s':\n%s", mod, tostring(result)),
      vim.log.levels.WARN
    )
    return {}
  end
  if type(result) ~= "table" then
    vim.notify(
      string.format("[plugins] spec '%s' returned %s (expected table)", mod, type(result)),
      vim.log.levels.WARN
    )
    return {}
  end
  return result
end

local function collect_specs()
  local specs = {}
  for _, base in ipairs(SPEC_DIRS) do
    local ok_path, base_path = pcall(function()
      return vim.fn.stdpath("config") .. "/lua/" .. base:gsub("%.", "/")
    end)
    if not ok_path then goto continue end

    local ok_scan, entries = pcall(vim.fn.readdir, base_path)
    if not ok_scan then goto continue end

    for _, entry in ipairs(entries) do
      local stem = entry:match("^(.-)%.lua$")
      if stem and stem ~= "init" and stem ~= "shared" then
        local mod_name = base .. "." .. stem
        vim.list_extend(specs, safe_spec(mod_name))
      end
    end

    ::continue::
  end
  return specs
end

local specs = collect_specs()

-- ── lazy.nvim setup ──────────────────────────────────────────────────────────

require("lazy").setup(specs, {
  defaults = {
    version = false,
    lazy    = true,
  },
  install  = { colorscheme = { "tokyonight", "catppuccin", "habamax" } },
  checker  = {
    enabled   = true,
    notify    = false,
    frequency = 86400,   -- daily
  },
  change_detection = { enabled = true, notify = false },
  ui = {
    border = "rounded",
    icons  = {
      cmd        = " ", config     = " ", event    = " ",
      ft         = " ", init       = " ", imports  = " ",
      keys       = " ", plugin     = " ", runtime  = " ",
      require    = " ", source     = " ", start    = " ",
      task       = " ", lazy       = "󰒲 ", not_loaded = "󰒲 ",
      loaded     = "●",  list      = { "●", "➜", "★", "‒" },
    },
  },
  performance = {
    cache          = { enabled = true },
    reset_packpath = true,
    rtp = {
      disabled_plugins = {
        "gzip", "matchit", "matchparen", "tarPlugin", "tohtml", "tutor",
        "zipPlugin", "2html_plugin", "getscript", "getscriptPlugin",
        "vimball", "vimballPlugin", "rrhelper", "logiPat", "spellfile_plugin",
      },
    },
  },
})
