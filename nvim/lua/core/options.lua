-- ~/.config/nvim/lua/core/options.lua

vim.g.mapleader = " "
vim.g.maplocalleader = ","

local o = vim.opt

-- UI
o.termguicolors = true
o.number = true
o.relativenumber = true
o.cursorline = true
o.signcolumn = "yes"
o.wrap = false
o.scrolloff = 8
o.sidescrolloff = 8
o.colorcolumn = "88"
o.conceallevel = 0
o.pumheight = 12
o.pumblend = 10
o.winblend = 10

-- Indentation
o.tabstop = 4
o.shiftwidth = 4
o.softtabstop = 4
o.expandtab = true
o.smartindent = true
o.shiftround = true

-- Search
o.ignorecase = true
o.smartcase = true
o.incsearch = true
o.hlsearch = true

-- Performance
o.updatetime = 200
o.timeoutlen = 400
o.lazyredraw = false
o.redrawtime = 10000

-- Files
o.undofile = true
o.undolevels = 10000
o.backup = false
o.swapfile = false
o.autowrite = true
o.autoread = true
o.confirm = true

-- Splits
o.splitright = true
o.splitbelow = true
o.winminwidth = 5

-- Completion
o.completeopt = { "menu", "menuone", "noselect" }

-- Editor
o.mouse = "a"
o.clipboard = "unnamedplus"
o.laststatus = 3
o.cmdheight = 0
o.showmode = false
o.showcmd = false

-- Folding
o.foldmethod = "expr"
o.foldexpr = "nvim_treesitter#foldexpr()"
o.foldlevel = 99
o.foldlevelstart = 99

o.shortmess:append("IWcC")
o.fillchars = { eob = " ", fold = " ", foldopen = "", foldclose = "" }
o.list = true
o.listchars = { tab = "→ ", trail = "·", extends = "»", precedes = "«", nbsp = "␣" }

-- Smooth scroll
if vim.fn.has("nvim-0.10") == 1 then
  o.smoothscroll = true
end