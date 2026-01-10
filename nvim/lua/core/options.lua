-- lua/core/options.lua - Neovim options

local opt = vim.opt
local g = vim.g

-- General
opt.number = true
opt.relativenumber = true
opt.signcolumn = "yes"
opt.wrap = true
opt.linebreak = true
opt.breakindent = true
opt.mouse = "a"
opt.clipboard = "unnamedplus"
opt.undofile = true
opt.swapfile = false
opt.backup = false

-- Search
opt.ignorecase = true
opt.smartcase = true
opt.hlsearch = true
opt.incsearch = true

-- Indentation
opt.expandtab = true
opt.shiftwidth = 2
opt.softtabstop = 2
opt.tabstop = 2
opt.autoindent = true
opt.smartindent = true

-- Display
opt.termguicolors = true
opt.laststatus = 3
opt.cmdheight = 1
opt.scrolloff = 10
opt.sidescrolloff = 10
opt.splitbelow = true
opt.splitright = true
opt.cursorline = true

-- Completion
opt.completeopt = { "menu", "menuone", "noselect" }
opt.pumheight = 15

-- Folding (for nvim-ufo)
opt.foldcolumn = "1"
opt.foldlevel = 99
opt.foldlevelstart = 99
opt.foldenable = true

-- Window
opt.winwidth = 30
opt.winminwidth = 10

-- Performance
opt.updatetime = 200
opt.timeoutlen = 500
opt.ttimeoutlen = 10
opt.lazyredraw = true

-- Disable some default plugins
g.loaded_gzip = 1
g.loaded_zip = 1
g.loaded_zipPlugin = 1
g.loaded_tar = 1
g.loaded_tarPlugin = 1
g.loaded_getscript = 1
g.loaded_getscriptPlugin = 1
g.loaded_vimball = 1
g.loaded_vimballPlugin = 1
g.loaded_2html_plugin = 1
g.loaded_netrw = 1
g.loaded_netrwPlugin = 1
g.loaded_netrwSettings = 1
g.loaded_netrwFileHandlers = 1