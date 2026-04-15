-- lua/core/options.lua - Neovim options (Nvim 0.11+)
--
-- FIX (v2.3.1):
--   • foldexpr updated to "v:lua.vim.treesitter.foldexpr()".
--
-- FIX (v2.3.2):
--   • vim.g.auto_cd_root initialised to false (opt-in, discoverable).
--
-- FIX (v2.3.3):
--   • sessionoptions: removed "terminal". Restoring terminal buffers almost
--     always fails and leaves dead "[Process exited]" windows in every session.
--     "help" also removed — help buffers are ephemeral and should not be
--     persisted across sessions.
--
-- FIX (v2.3.9b):
--   • matchparen removed from the disabled builtin_plugins list. It was
--     suppressed since v2.0 with no replacement, leaving bracket-under-cursor
--     highlighting completely off. vim-matchup (added to advanced.lua) supersedes
--     matchparen: it handles multi-line matches, adds treesitter-aware % motions,
--     and sets g:loaded_matchparen itself on init so the builtin never
--     double-loads. Removing our override here lets vim-matchup's hand-off work.

local opt = vim.opt
local g   = vim.g

-- ═══════════════════════════════════════════════════════════════════════════
-- GENERAL
-- ═══════════════════════════════════════════════════════════════════════════

opt.number         = true
opt.relativenumber = true
opt.signcolumn     = "yes:1"
opt.wrap           = true
opt.linebreak      = true
opt.breakindent    = true
opt.showbreak      = "↳ "
opt.breakindentopt = "shift:2,min:40"
opt.mouse          = "a"

if vim.fn.has("clipboard") == 1 then
  opt.clipboard = "unnamedplus"
end

opt.undofile = true
opt.swapfile = false
opt.backup   = false
opt.confirm  = true

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
opt.showmode      = false

opt.shortmess:append("sIc")

-- ═══════════════════════════════════════════════════════════════════════════
-- COMPLETION
-- ═══════════════════════════════════════════════════════════════════════════

opt.completeopt = { "menu", "menuone", "noselect" }
opt.pumheight   = 15

-- ═══════════════════════════════════════════════════════════════════════════
-- FOLDING (nvim-ufo + treesitter)
-- ═══════════════════════════════════════════════════════════════════════════

opt.foldcolumn     = "1"
opt.foldlevel      = 99
opt.foldlevelstart = 99
opt.foldenable     = true

opt.foldmethod = "expr"
-- FIX (v2.3.1): replaces deprecated "nvim_treesitter#foldexpr()" shim.
opt.foldexpr = "v:lua.vim.treesitter.foldexpr()"

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
opt.ttimeoutlen = 50

-- ═══════════════════════════════════════════════════════════════════════════
-- SPELLING
-- ═══════════════════════════════════════════════════════════════════════════

opt.spelllang = "en_us"

-- ═══════════════════════════════════════════════════════════════════════════
-- SESSION
-- ═══════════════════════════════════════════════════════════════════════════

-- FIX (v2.3.3): "terminal" removed — persisting terminal buffers almost always
-- fails, leaving dead "[Process exited]" windows on every session restore.
-- "help" removed — help buffers are ephemeral and should not be persisted.
opt.sessionoptions = "buffers,curdir,folds,tabpages,winsize,winpos,localoptions"

-- ═══════════════════════════════════════════════════════════════════════════
-- GLOBAL FLAGS
-- ═══════════════════════════════════════════════════════════════════════════

-- FIX (v2.3.2): auto_cd_root initialised to false (opt-in).
-- Toggle at runtime with :ToggleAutoCd
-- To enable by default on every startup, change false → true here.
g.auto_cd_root = false

-- ═══════════════════════════════════════════════════════════════════════════
-- DISABLE BUILT-IN PLUGINS
-- ═══════════════════════════════════════════════════════════════════════════

local builtin_plugins = {
  "gzip", "zip", "zipPlugin", "tar", "tarPlugin",
  "getscript", "getscriptPlugin",
  "vimball", "vimballPlugin",
  "2html_plugin",
  "netrw", "netrwPlugin", "netrwSettings", "netrwFileHandlers",
  -- FIX (v2.3.9b): matchparen intentionally NOT in this list.
  -- vim-matchup (advanced.lua) supersedes the builtin and sets
  -- g:loaded_matchparen itself, preventing double-loading. Disabling it here
  -- blocked that hand-off and left bracket highlighting off since v2.0.
  "matchit",
}

for _, plugin in ipairs(builtin_plugins) do
  g["loaded_" .. plugin] = 1
end
