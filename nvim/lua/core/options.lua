-- ~/.config/nvim/lua/core/options.lua
-- Core Neovim options and settings

-- Leader keys (must be set before lazy.nvim)
vim.g.mapleader = " "
vim.g.maplocalleader = ","

-- Security (optional)
if _G.nvim_ide.security_mode then
  for _, plugin in ipairs({
    "gzip", "tar", "tarPlugin", "zip", "zipPlugin",
    "getscript", "getscriptPlugin", "vimball", "vimballPlugin",
    "2html_plugin", "logipat", "rrhelper", "spellfile_plugin",
    "netrw", "netrwPlugin", "netrwSettings", "netrwFileHandlers"
  }) do
    vim.g["loaded_" .. plugin] = 1
  end
  vim.opt.modeline = false
  vim.opt.exrc = false
  vim.opt.secure = true
end

-- UI Settings
vim.opt.termguicolors = true
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.cursorline = true
vim.opt.cursorcolumn = false
vim.opt.signcolumn = "yes:2"
vim.opt.wrap = false
vim.opt.scrolloff = 8
vim.opt.sidescrolloff = 8
vim.opt.colorcolumn = "80,100,120"
vim.opt.conceallevel = 0

-- Indentation
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.softtabstop = 4
vim.opt.expandtab = true
vim.opt.smartindent = true
vim.opt.autoindent = true

-- Search
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.incsearch = true
vim.opt.hlsearch = true
vim.opt.wrapscan = true

-- Performance
vim.opt.updatetime = 300
vim.opt.timeoutlen = 500
vim.opt.ttimeoutlen = 10
vim.opt.redrawtime = 10000
vim.opt.maxmempattern = 20000
vim.opt.lazyredraw = false  -- CRITICAL: Must be false for noice.nvim

-- Files
vim.opt.undofile = true
vim.opt.undolevels = 10000
vim.opt.backup = false
vim.opt.writebackup = false
vim.opt.swapfile = false
vim.opt.autowrite = true
vim.opt.autoread = true
vim.opt.confirm = true

-- Splits
vim.opt.splitright = true
vim.opt.splitbelow = true
vim.opt.winminwidth = 5
vim.opt.winheight = 5
vim.opt.winminheight = 5

-- Completion
vim.opt.completeopt = { "menu", "menuone", "noselect", "noinsert" }
vim.opt.pumheight = 15
vim.opt.pumwidth = 25

-- Editor
vim.opt.mouse = "a"
vim.opt.clipboard = "unnamedplus"
vim.opt.laststatus = 3
vim.opt.cmdheight = 1
vim.opt.showmode = false
vim.opt.showcmd = true
vim.opt.ruler = true

-- Folding
vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "nvim_treesitter#foldexpr()"
vim.opt.foldlevel = 20
vim.opt.foldlevelstart = 20

-- Disable intro message
vim.opt.shortmess:append("I")
