-- lua/core/options.lua - Neovim options (Nvim 0.11+)
--
-- FIX (v2.3.1):
--   • foldexpr updated to "v:lua.vim.treesitter.foldexpr()".
--
-- FIX (v2.3.2):
--   • vim.g.auto_cd_root was never initialised. The AutoCdRoot autocmd in
--     autocmds.lua gates on `if not vim.g.auto_cd_root then return end`, so
--     the feature was permanently disabled on every startup — users had no
--     way to know it existed or that :ToggleAutoCd could enable it.
--     Initialised to false here (opt-in, same behaviour as before) with a
--     comment explaining how to enable it persistently.

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

opt.sessionoptions = "buffers,curdir,folds,help,tabpages,winsize,winpos,terminal,localoptions"

-- ═══════════════════════════════════════════════════════════════════════════
-- GLOBAL FLAGS
-- ═══════════════════════════════════════════════════════════════════════════

-- FIX (v2.3.2): auto_cd_root was never initialised, making the AutoCdRoot
-- autocmd in autocmds.lua permanently inactive. Explicitly set to false so
-- the feature is opt-in but discoverable. Toggle at runtime with:
--   :ToggleAutoCd
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
  "matchit", "matchparen",
}

for _, plugin in ipairs(builtin_plugins) do
  g["loaded_" .. plugin] = 1
end
