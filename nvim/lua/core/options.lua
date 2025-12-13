-- ~/.config/nvim/lua/core/options.lua

-- Leader keys
vim.g.mapleader = " "
vim.g.maplocalleader = ","

-- Global variables for Health command
vim.g.ide_colorcolumn = 88
vim.g.ide_memory_limit_mb = 1024

-- UI Options
vim.opt.termguicolors = true
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.cursorline = true
vim.opt.signcolumn = "yes"
vim.opt.wrap = false
vim.opt.scrolloff = 8
vim.opt.sidescrolloff = 8
vim.opt.colorcolumn = "88"
vim.opt.conceallevel = 0
vim.opt.pumheight = 12
vim.opt.pumblend = 10
vim.opt.winblend = 10

-- Indentation
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.softtabstop = 4
vim.opt.expandtab = true
vim.opt.smartindent = true
vim.opt.shiftround = true

-- Search
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.incsearch = true
vim.opt.hlsearch = true

-- Performance
vim.opt.updatetime = 200
vim.opt.timeoutlen = 400
vim.opt.lazyredraw = false
vim.opt.redrawtime = 10000

-- Files
vim.opt.undofile = true
vim.opt.undolevels = 10000
vim.opt.backup = false
vim.opt.swapfile = false
vim.opt.autowrite = true
vim.opt.autoread = true
vim.opt.confirm = true

-- Splits
vim.opt.splitright = true
vim.opt.splitbelow = true
vim.opt.winminwidth = 5

-- Completion
vim.opt.completeopt = { "menu", "menuone", "noselect" }

-- Editor
vim.opt.mouse = "a"
vim.opt.clipboard = "unnamedplus"
vim.opt.laststatus = 3
vim.opt.cmdheight = 0
vim.opt.showmode = false
vim.opt.showcmd = false

-- Spelling
vim.opt.spell = false
vim.opt.spelllang = "en_us"

-- Folding
vim.opt.foldmethod = "manual"
vim.opt.foldlevel = 99
vim.opt.foldlevelstart = 99

-- Misc
vim.opt.shortmess:append("IWcC")
vim.opt.fillchars = { eob = " " }
vim.opt.list = true
vim.opt.listchars = { tab = "→ ", trail = "·", extends = "»", precedes = "«", nbsp = "☣" }

-- Smooth scroll (Neovim 0.10+)
if vim.fn.has("nvim-0.10") == 1 then
  vim.opt.smoothscroll = true
end
