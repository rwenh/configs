-- lua/core/options.lua — Neovim options (Nvim 0.11+)
--

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

opt.clipboard = "unnamedplus"

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
-- Uses the native Lua treesitter foldexpr introduced in Neovim 0.10+.
-- (Replaced deprecated "nvim_treesitter#foldexpr()" shim in v2.3.1.)
opt.foldexpr = "v:lua.vim.treesitter.foldexpr()"

-- ═══════════════════════════════════════════════════════════════════════════
-- WINDOW
-- ═══════════════════════════════════════════════════════════════════════════

opt.winwidth    = 30
opt.winminwidth = 10

-- ═══════════════════════════════════════════════════════════════════════════
-- PERFORMANCE
-- ═══════════════════════════════════════════════════════════════════════════

opt.updatetime = 200

opt.timeoutlen  = 500
opt.ttimeoutlen = 50

-- ═══════════════════════════════════════════════════════════════════════════
-- SPELLING
-- ═══════════════════════════════════════════════════════════════════════════

opt.spelllang = "en_us"

-- ═══════════════════════════════════════════════════════════════════════════
-- SESSION
-- ═══════════════════════════════════════════════════════════════════════════

-- Removed in v2.3.3:
--   "terminal" — persisted terminal buffers fail on restore ("[Process exited]")
--   "help"     — help buffers are ephemeral
opt.sessionoptions = table.concat({
  "buffers",       -- open buffers
  "curdir",        -- current working directory
  "folds",         -- fold state
  "tabpages",      -- all tab pages
  "winsize",       -- window sizes
  "winpos",        -- window position on screen
  "localoptions",  -- buffer-local and window-local options
}, ",")

-- ═══════════════════════════════════════════════════════════════════════════
-- GLOBAL FLAGS
-- ═══════════════════════════════════════════════════════════════════════════

-- Opt-in: auto-cd to project root on BufEnter.
-- Toggle at runtime with :ToggleAutoCd.
-- Set to true here to enable for every session.
g.auto_cd_root = false

-- Opt-in: auto-save before running a file with runner.lua.
-- Set to false to disable the silent pre-run write.
g.runner_autosave = true

-- ═══════════════════════════════════════════════════════════════════════════
-- DISABLE BUILT-IN PLUGINS
-- ═══════════════════════════════════════════════════════════════════════════

local disabled_builtins = {
  "gzip", "zip", "zipPlugin", "tar", "tarPlugin",
  "getscript", "getscriptPlugin",
  "vimball", "vimballPlugin",
  "2html_plugin",
  "netrw", "netrwPlugin", "netrwSettings", "netrwFileHandlers",

  -- matchparen intentionally NOT here (restored in v2.3.9b):
  --   vim-matchup (advanced.lua) supersedes the builtin and sets
  --   g:loaded_matchparen itself, preventing double-loading.

  -- matchit IS disabled (v2.3.11):
  --   vim-matchup does NOT set g:loaded_matchit — they are separate builtins.
  --   vim-matchup provides a superior treesitter-aware replacement for % motion.
  "matchit",
}

for _, plugin in ipairs(disabled_builtins) do
  g["loaded_" .. plugin] = 1
end
