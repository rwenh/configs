-- lua/core/options.lua - Neovim options (cleaned up for Nvim 0.10+)

local opt = vim.opt
local g   = vim.g

-- ═══════════════════════════════════════════════════════════════════════════
-- GENERAL
-- ═══════════════════════════════════════════════════════════════════════════

opt.number         = true
opt.relativenumber = true
opt.signcolumn     = "yes:1"   -- explicit single-column width, no layout jump
opt.wrap           = true
opt.linebreak      = true
opt.breakindent    = true
opt.mouse          = "a"
opt.clipboard      = "unnamedplus"  -- requires xclip/xsel/wl-clipboard/win32yank
opt.undofile       = true
opt.swapfile       = false
opt.backup         = false
opt.confirm        = true   -- ask instead of failing on unsaved changes

-- ═══════════════════════════════════════════════════════════════════════════
-- SEARCH
-- ═══════════════════════════════════════════════════════════════════════════

opt.ignorecase = true
opt.smartcase  = true
opt.hlsearch   = true
opt.incsearch  = true

-- ═══════════════════════════════════════════════════════════════════════════
-- INDENTATION
-- ═══════════════════════════════════════════════════════════════════════════

opt.expandtab   = true
opt.shiftwidth  = 2
opt.softtabstop = 2
opt.tabstop     = 2
opt.autoindent  = true
-- FIX #3: smartindent removed — it's a legacy Vim feature that predates
-- treesitter and can interfere with treesitter-based indentation. Notably
-- it mishandles Python comment dedentation. autoindent alone is correct
-- when treesitter indent is active.

-- ═══════════════════════════════════════════════════════════════════════════
-- DISPLAY
-- ═══════════════════════════════════════════════════════════════════════════

opt.termguicolors = true
opt.laststatus    = 3
opt.cmdheight     = 1
opt.scrolloff     = 10
opt.sidescrolloff = 10
opt.splitbelow    = true
opt.splitright    = true
opt.cursorline    = true
opt.showmode      = false   -- lualine shows this already

-- FIX #6: Suppress noisy messages that are redundant with lualine/noice.
-- "s" = silence "search hit BOTTOM/TOP" wrap messages
-- "I" = suppress the :intro splash screen on startup
opt.shortmess:append("sI")

-- ═══════════════════════════════════════════════════════════════════════════
-- COMPLETION
-- ═══════════════════════════════════════════════════════════════════════════

opt.completeopt = { "menu", "menuone", "noselect" }
opt.pumheight   = 15

-- ═══════════════════════════════════════════════════════════════════���═══════
-- FOLDING (nvim-ufo)
-- ═══════════════════════════════════════════════════════════════════════════

opt.foldcolumn     = "1"
opt.foldlevel      = 99
opt.foldlevelstart = 99
opt.foldenable     = true

-- ═══════════════════════════════════════════════════════════════════════════
-- WINDOW
-- ═══════════════════════════════════════════════════════════════════════════

opt.winwidth    = 30
opt.winminwidth = 10

-- ═══════════════════════════════════════════════════════════════════════════
-- PERFORMANCE
-- ═══════════════════════════════════════════════════════════════════════════

opt.updatetime  = 200
opt.timeoutlen  = 500
opt.ttimeoutlen = 10
-- NOTE: lazyredraw removed — deprecated and causes issues in Nvim 0.10+

-- ═══════════════════════════════════════════════════════════════════════════
-- SPELLING
-- ═══════════════════════════════════════════════════════════════════════════

opt.spelllang = "en_us"

-- ═══════════════════════════════════════════════════════════════════════════
-- SESSION
-- ═══════════════════════════════════════════════════════════════════════════

-- FIX #7: Explicit sessionoptions — the default includes "options" which
-- persists all global options into session files. A stale session would then
-- silently override the settings defined here on restore. Removing "options"
-- from the list keeps session restore focused on layout/buffers only.
opt.sessionoptions = "buffers,curdir,folds,help,tabpages,winsize,winpos,terminal,localoptions"

-- ═══════════════════════════════════════════════════════════════════════════
-- DISABLE BUILT-IN PLUGINS
-- ═══════════════════════════════════════════════════════════════════════════

-- RECALIBRATION: Loop through all built-in plugins for clarity and maintainability
local builtin_plugins = {
  "gzip",
  "zip",
  "zipPlugin",
  "tar",
  "tarPlugin",
  "getscript",
  "getscriptPlugin",
  "vimball",
  "vimballPlugin",
  "2html_plugin",
  "netrw",
  "netrwPlugin",
  "netrwSettings",
  "netrwFileHandlers",
  "matchit",
  "matchparen",
}

for _, plugin in ipairs(builtin_plugins) do
  g["loaded_" .. plugin] = 1
end
