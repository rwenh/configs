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

if vim.g.disable_treesitter_folds then
  opt.foldmethod = "indent"
else
  opt.foldmethod = "expr"
  opt.foldexpr   = "v:lua.vim.treesitter.foldexpr()"
end

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

if type(vim.g.spell_wordlist) == "string" and vim.g.spell_wordlist ~= "" then
  local expanded = vim.fn.expand(vim.g.spell_wordlist)
  opt.spellfile = { expanded, vim.fn.stdpath("config") .. "/spell/en.utf-8.add" }
end

-- ═══════════════════════════════════════════════════════════════════════════
-- SESSION
-- ═══════════════════════════════════════════════════════════════════════════

opt.sessionoptions = table.concat({
  "buffers", "curdir", "folds", "tabpages",
  "winsize", "winpos", "localoptions",
}, ",")

-- ═══════════════════════════════════════════════════════════════════════════
-- GLOBAL FLAGS (escape hatches)
-- ═══════════════════════════════════════════════════════════════════════════

g.auto_cd_root    = false
g.runner_autosave = true

-- ═══════════════════════════════════════════════════════════════════════════
-- PER-FILETYPE OPTION OVERRIDES
-- ═══════════════════════════════════════════════════════════════════════════
--

local BUFFER_LOCAL_OPTS = {
  shiftwidth = true, tabstop = true, softtabstop = true,
  expandtab  = true, autoindent = true, cindent = true,
  textwidth  = true, commentstring = true, fileformat = true,
  fileencoding = true,
}

-- Known window-local option names (non-exhaustive).
local WINDOW_LOCAL_OPTS = {
  wrap = true, spell = true, number = true, relativenumber = true,
  cursorline = true, signcolumn = true, colorcolumn = true,
  foldcolumn = true, conceallevel = true, concealcursor = true,
  list = true, linebreak = true,
}

local function apply_ft_opt(buf, key, val)
  if BUFFER_LOCAL_OPTS[key] then
    pcall(function() vim.bo[buf][key] = val end)
  elseif WINDOW_LOCAL_OPTS[key] then
    pcall(function() vim.wo[key] = val end)
  else
    -- Unknown scope: try buffer first, fall back to window.
    local ok = pcall(function() vim.bo[buf][key] = val end)
    if not ok then pcall(function() vim.wo[key] = val end) end
  end
end

local BUILTIN_FT_OPTS = {
  html            = { shiftwidth = 2, tabstop = 2 },
  css             = { shiftwidth = 2, tabstop = 2 },
  javascript      = { shiftwidth = 2, tabstop = 2 },
  typescript      = { shiftwidth = 2, tabstop = 2 },
  json            = { shiftwidth = 2, tabstop = 2 },
  yaml            = { shiftwidth = 2, tabstop = 2 },
  javascriptreact = { shiftwidth = 2, tabstop = 2 },
  typescriptreact = { shiftwidth = 2, tabstop = 2 },
  go              = { shiftwidth = 4, tabstop = 4, expandtab = false },
  markdown        = { wrap = true, spell = true },
}

local ft_opts = vim.tbl_deep_extend(
  "force",
  BUILTIN_FT_OPTS,
  type(vim.g.filetype_options) == "table" and vim.g.filetype_options or {}
)

vim.api.nvim_create_autocmd("FileType", {
  group    = vim.api.nvim_create_augroup("FiletypeOptions", { clear = true }),
  callback = function(e)
    local overrides = ft_opts[vim.bo[e.buf].filetype]
    if not overrides then return end
    for key, val in pairs(overrides) do
      apply_ft_opt(e.buf, key, val)
    end
  end,
  desc = "Apply per-filetype option overrides (scope-aware)",
})

-- ═══════════════════════════════════════════════════════════════════════════
-- DISABLE BUILT-IN PLUGINS
-- ═══════════════════════════════════════════════════════════════════════════

local disabled_builtins = {
  "gzip", "zip", "zipPlugin", "tar", "tarPlugin",
  "getscript", "getscriptPlugin",
  "vimball", "vimballPlugin",
  "2html_plugin",
  "netrw", "netrwPlugin", "netrwSettings", "netrwFileHandlers",
  "matchit",
}

for _, plugin in ipairs(disabled_builtins) do
  g["loaded_" .. plugin] = 1
end
