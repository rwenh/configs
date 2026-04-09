-- lua/core/options.lua - Neovim options (Nvim 0.11+)
--
-- FIX (v2.3.1):
--   • foldexpr updated from deprecated "nvim_treesitter#foldexpr()" vimscript
--     function (nvim-treesitter v3 removed it) to the canonical Neovim 0.10+
--     built-in: "v:lua.vim.treesitter.foldexpr()".
--     The old string caused E117 (Unknown function) on nvim-treesitter v3+,
--     silently falling back to foldmethod=manual and breaking all folds.
--     The new expression calls vim.treesitter.foldexpr() directly via v:lua,
--     which is available on Neovim 0.10+ without any plugin dependency.
--     nvim-ufo overrides this at runtime on BufEnter for its managed buffers;
--     this value is the baseline for unmanaged buffers (text, help, etc.).

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
-- FIX: "v:lua.vim.treesitter.foldexpr()" replaces the deprecated
-- "nvim_treesitter#foldexpr()" vimscript shim removed in nvim-treesitter v3.
-- vim.treesitter.foldexpr is a built-in Neovim 0.10+ function; no plugin
-- needed. nvim-ufo overrides this on BufEnter for its own managed buffers.
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
