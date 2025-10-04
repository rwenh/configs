" =============================================================================
" Optimized Vim IDE Configuration
" Compatible with Vim 8.2+ and Neovim 0.8+
" Performance-focused with modular design
" =============================================================================

" -----------------------------------------------------------------------------
" 0. Performance & Security - Load First
" -----------------------------------------------------------------------------
if has('vim_starting')
  set encoding=utf-8
  scriptencoding utf-8
endif

" Disable unnecessary providers (Neovim)
if has('nvim')
  let g:loaded_python_provider = 0
  let g:loaded_python3_provider = 0
  let g:loaded_ruby_provider = 0
  let g:loaded_perl_provider = 0
  let g:loaded_node_provider = 0
endif

" Disable built-in plugins for faster startup
let g:loaded_gzip = 1
let g:loaded_tar = 1
let g:loaded_tarPlugin = 1
let g:loaded_zip = 1
let g:loaded_zipPlugin = 1
let g:loaded_getscript = 1
let g:loaded_getscriptPlugin = 1
let g:loaded_vimball = 1
let g:loaded_vimballPlugin = 1
let g:loaded_2html_plugin = 1
let g:loaded_logiPat = 1
let g:loaded_rrhelper = 1
let g:loaded_netrw = 1
let g:loaded_netrwPlugin = 1
let g:loaded_netrwSettings = 1
let g:loaded_netrwFileHandlers = 1
let g:loaded_matchit = 1
let g:loaded_matchparen = 1

" Core performance settings
set regexpengine=1
set synmaxcol=300
set lazyredraw
set ttyfast
set scrolljump=5
set ttyscroll=3
set maxmempattern=5000000
set updatetime=100
set redrawtime=2000
set re=1

" Security
set nomodeline
set modelines=0
set secure

" Better mouse support
if has('mouse_sgr') && !has('nvim')
  set ttymouse=sgr
endif

" -----------------------------------------------------------------------------
" 1. Core Settings
" -----------------------------------------------------------------------------
set nocompatible
filetype plugin indent on
syntax enable

" File encoding
set fileencoding=utf-8
set fileencodings=utf-8,ucs-bom,latin1

" Editing behavior
set backspace=indent,eol,start
set history=10000
set undolevels=2000
set undoreload=20000

" Display
set showcmd
set ruler
set number
set relativenumber
set cursorline
set laststatus=2
set scrolloff=8
set sidescrolloff=5
set showmatch
set matchtime=2

" Search
set incsearch
set hlsearch
set ignorecase
set smartcase
set wrapscan

" Indentation
set autoindent
set smartindent
set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4
set shiftround
set smarttab

" Buffers and windows
set hidden
set switchbuf=useopen,usetab
set splitbelow
set splitright
set winminheight=0
set winminwidth=0

" Completion
set wildmenu
set wildmode=longest:full,full
set wildignore+=*.o,*~,*.pyc,*.class,*.jar
set wildignore+=*/.git/*,*/.hg/*,*/.svn/*
set wildignore+=*/node_modules/*,*/bower_components/*
set wildignore+=*.DS_Store,*.log,*.tmp
set completeopt=menuone,noselect,preview

" Timing and UI
set timeoutlen=500
set ttimeoutlen=10
set shortmess+=acFI
set signcolumn=auto
set pumheight=15
set cmdheight=1
set noshowmode

" File handling
set nobackup
set nowritebackup
set noswapfile
set autoread
set autowrite

" Folding
set foldmethod=indent
set foldlevel=99
set foldlevelstart=99

" List characters
set list
set listchars=tab:▸\ ,eol:¬,trail:·,extends:→,precedes:←,nbsp:⦸
set fillchars=vert:│,fold:─,diff:─

" GUI settings
if has('gui_running')
  set guifont=JetBrainsMono\ Nerd\ Font\ Mono:h12
  set guioptions=ac
  set columns=120
  set lines=35
endif

" Terminal colors
if has('termguicolors') && ($COLORTERM ==# 'truecolor' || $COLORTERM ==# '24bit')
  set termguicolors
endif

" Persistent undo
if has('persistent_undo')
  let s:undodir = has('nvim') ? stdpath('cache') . '/undo' : expand('~/.vim/undo')
  if !isdirectory(s:undodir)
    call mkdir(s:undodir, 'p', 0700)
  endif
  if isdirectory(s:undodir) && getfperm(s:undodir) =~# 'rw'
    set undofile
    let &undodir = s:undodir
  endif
endif

" -----------------------------------------------------------------------------
" 2. Plugin Manager (vim-plug)
" -----------------------------------------------------------------------------
function! s:InstallVimPlug()
  let l:plug_path = has('nvim') ?
        \ stdpath('data') . '/site/autoload/plug.vim' :
        \ expand('~/.vim/autoload/plug.vim')
  
  if empty(glob(l:plug_path))
    try
      let l:plug_url = 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
      if executable('curl')
        call system('curl -fsSL --tlsv1.2 -o ' . shellescape(l:plug_path) . ' --create-dirs ' . l:plug_url)
      elseif executable('wget')
        call system('wget --secure-protocol=TLSv1_2 -O ' . shellescape(l:plug_path) . ' ' . l:plug_url)
      else
        throw 'Neither curl nor wget found'
      endif
      
      if v:shell_error == 0
        echom 'vim-plug installed successfully'
        return 1
      else
        throw 'Download failed with exit code ' . v:shell_error
      endif
    catch
      echohl ErrorMsg
      echo 'Failed to install vim-plug: ' . v:exception
      echohl None
      return 0
    endtry
  endif
  return 1
endfunction

if !s:InstallVimPlug()
  finish
endif

let s:plug_dir = has('nvim') ? stdpath('data') . '/plugged' : expand('~/.vim/plugged')

call plug#begin(s:plug_dir)

" Theme and UI
Plug 'lifepillar/vim-solarized8'
Plug 'ryanoasis/vim-devicons'

" Core plugins (both Vim and Neovim)
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'voldikss/vim-floaterm'
Plug 'christoomey/vim-tmux-navigator'

" Snippets
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

" Language support
Plug 'sheerun/vim-polyglot'

" Tags and navigation
Plug 'preservim/tagbar', { 'on': 'TagbarToggle' }
Plug 'ludovicchabant/vim-gutentags'

" Linting and formatting (Vim)
if !has('nvim-0.7')
  Plug 'dense-analysis/ale', { 'for': ['python', 'javascript', 'typescript', 'go', 'rust'] }
  Plug 'sbdchd/neoformat', { 'on': 'Neoformat' }
endif

" Git integration
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'

" Editor enhancements
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-repeat'
Plug 'jiangmiao/auto-pairs'
Plug 'wellle/targets.vim'
Plug 'matze/vim-move'
Plug 'liuchengxu/vim-which-key'
Plug 'jdhao/better-escape.vim'
Plug 'tpope/vim-sleuth'
Plug 'editorconfig/editorconfig-vim'

" Utilities
Plug 'mbbill/undotree', { 'on': 'UndotreeToggle' }
Plug 'tpope/vim-obsession'

" Database
Plug 'tpope/vim-dadbod'
Plug 'kristijanhusak/vim-dadbod-ui'
Plug 'kristijanhusak/vim-dadbod-completion'

" Neovim-specific plugins
if has('nvim-0.7')
  " UI enhancements
  Plug 'nvim-lualine/lualine.nvim'
  Plug 'akinsho/bufferline.nvim', { 'tag': '*' }
  Plug 'nvim-tree/nvim-web-devicons'
  Plug 'nvim-tree/nvim-tree.lua'
  
  " Fuzzy finder
  Plug 'nvim-telescope/telescope.nvim', { 'tag': '0.1.4' }
  Plug 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'make' }
  Plug 'nvim-telescope/telescope-ui-select.nvim'
  Plug 'nvim-lua/plenary.nvim'
  
  " Treesitter
  Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
  Plug 'nvim-treesitter/nvim-treesitter-textobjects'
  Plug 'nvim-treesitter/nvim-treesitter-context'
  
  " Terminal
  Plug 'akinsho/toggleterm.nvim', {'tag' : '*'}
  
  " LSP and completion
  Plug 'neovim/nvim-lspconfig'
  Plug 'williamboman/mason.nvim'
  Plug 'williamboman/mason-lspconfig.nvim'
  Plug 'hrsh7th/nvim-cmp'
  Plug 'hrsh7th/cmp-nvim-lsp'
  Plug 'hrsh7th/cmp-buffer'
  Plug 'hrsh7th/cmp-path'
  Plug 'hrsh7th/cmp-cmdline'
  Plug 'hrsh7th/cmp-nvim-lua'
  Plug 'saadparwaiz1/cmp_luasnip'
  Plug 'j-hui/fidget.nvim'
  Plug 'folke/neodev.nvim'
  Plug 'L3MON4D3/LuaSnip'
  Plug 'rafamadriz/friendly-snippets'
  
  " Formatting and linting
  Plug 'stevearc/conform.nvim'
  Plug 'mfussenegger/nvim-lint'
  
  " Debugging
  Plug 'mfussenegger/nvim-dap'
  Plug 'rcarriga/nvim-dap-ui'
  Plug 'theHamsta/nvim-dap-virtual-text'
  
  " Git enhancements
  Plug 'lewis6991/gitsigns.nvim'
  Plug 'sindrets/diffview.nvim'
  
  " Enhanced UI
  Plug 'windwp/nvim-autopairs'
  Plug 'phaazon/hop.nvim'
  Plug 'folke/which-key.nvim'
  Plug 'folke/todo-comments.nvim'
  Plug 'rcarriga/nvim-notify'
  Plug 'stevearc/dressing.nvim'
  Plug 'folke/trouble.nvim'
  
  " Session management
  Plug 'folke/persistence.nvim'
  
  " Testing
  Plug 'nvim-neotest/neotest'
  Plug 'nvim-neotest/neotest-python'
  Plug 'nvim-neotest/neotest-jest'
  Plug 'nvim-neotest/neotest-go'
  
  " AI assistance
  if has('nvim-0.8') && executable('node')
    Plug 'github/copilot.vim'
  endif
else
  " Vim-specific alternatives
  Plug 'preservim/nerdtree', { 'on': ['NERDTreeToggle', 'NERDTreeFind'] }
  Plug 'tiagofumo/vim-nerdtree-syntax-highlight', { 'on': 'NERDTreeToggle' }
  Plug 'xuyuanp/nerdtree-git-plugin', { 'on': 'NERDTreeToggle' }
  Plug 'airblade/vim-gitgutter'
  Plug 'puremourning/vimspector'
endif

call plug#end()

" -----------------------------------------------------------------------------
" 3. Theme Configuration
" -----------------------------------------------------------------------------
function! s:SetupTheme()
  try
    let s:hour = str2nr(strftime('%H'))
    if s:hour >= 6 && s:hour < 18
      set background=light
    else
      set background=dark
    endif
    colorscheme solarized8
  catch
    colorscheme desert
  endtry
endfunction

augroup ThemeSetup
  autocmd!
  autocmd VimEnter * call s:SetupTheme()
augroup END

function! ToggleTheme()
  if &background ==# 'dark'
    set background=light
  else
    set background=dark
  endif
  colorscheme solarized8
endfunction

" -----------------------------------------------------------------------------
" 4. Key Mappings
" -----------------------------------------------------------------------------
let mapleader = ' '
let maplocalleader = ','

" Quick exit insert mode
inoremap jk <Esc>
inoremap kj <Esc>

" Config management
nnoremap <leader><CR> :source $MYVIMRC \| echo 'Config reloaded!'<CR>
nnoremap <leader>ec :edit $MYVIMRC<CR>

" File operations
nnoremap <C-s> :write<CR>
inoremap <C-s> <C-o>:write<CR>
nnoremap <leader>w :write<CR>
nnoremap <leader>q :quit<CR>
nnoremap <leader>Q :quit!<CR>

" Window navigation
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Window management
nnoremap <leader>wv :vsplit<CR>
nnoremap <leader>ws :split<CR>
nnoremap <leader>wc :close<CR>
nnoremap <leader>wo :only<CR>
nnoremap <leader>w= <C-w>=

" Buffer management
nnoremap <Tab> :bnext<CR>
nnoremap <S-Tab> :bprevious<CR>
nnoremap <leader>bd :bdelete<CR>
nnoremap <leader>bD :bdelete!<CR>

" Search enhancements
nnoremap <leader>sc :nohlsearch<CR>
nnoremap <leader>sr :%s/\<<C-r><C-w>\>//gc<Left><Left><Left>
vnoremap <leader>sr "hy:%s/<C-r>h//gc<Left><Left><Left>

" Better movement
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk

" Center search results
nnoremap n nzzzv
nnoremap N Nzzzv
nnoremap * *zzzv
nnoremap # #zzzv

" Keep selection when indenting
vnoremap < <gv
vnoremap > >gv

" Better paste
nnoremap Y y$
nnoremap <leader>p "+p
nnoremap <leader>P "+P
vnoremap <leader>p "+p
vnoremap <leader>y "+y

" Comments
nnoremap <leader>/ :Commentary<CR>
vnoremap <leader>/ :Commentary<CR>

" File explorer
if has('nvim-0.7')
  nnoremap <leader>e :NvimTreeToggle<CR>
  nnoremap <leader>E :NvimTreeFindFile<CR>
else
  nnoremap <leader>e :NERDTreeToggle<CR>
  nnoremap <leader>E :NERDTreeFind<CR>
endif

" Fuzzy finder
if has('nvim-0.7')
  nnoremap <leader>ff <cmd>Telescope find_files<CR>
  nnoremap <leader>fg <cmd>Telescope live_grep<CR>
  nnoremap <leader>fb <cmd>Telescope buffers<CR>
  nnoremap <leader>fh <cmd>Telescope help_tags<CR>
  nnoremap <leader>fr <cmd>Telescope oldfiles<CR>
  nnoremap <leader>fc <cmd>Telescope commands<CR>
else
  nnoremap <leader>ff :Files<CR>
  nnoremap <leader>fg :Rg<CR>
  nnoremap <leader>fb :Buffers<CR>
  nnoremap <leader>fh :History<CR>
  nnoremap <leader>fc :Commands<CR>
endif

" Terminal
if has('nvim-0.7')
  nnoremap <leader>tt :ToggleTerm<CR>
else
  nnoremap <leader>tt :FloatermToggle<CR>
  nnoremap <leader>tn :FloatermNew<CR>
endif

" Git operations
nnoremap <leader>gs :Git<CR>
nnoremap <leader>gc :Git commit<CR>
nnoremap <leader>gp :Git push<CR>
nnoremap <leader>gl :Git pull<CR>
nnoremap <leader>gb :Git blame<CR>
nnoremap <leader>gd :Gdiffsplit<CR>

" Utilities
nnoremap <leader>un :set number!<CR>
nnoremap <leader>ur :set relativenumber!<CR>
nnoremap <leader>uw :set wrap!<CR>
nnoremap <leader>us :set spell!<CR>
nnoremap <leader>uh :set hlsearch!<CR>
nnoremap <leader>ut :call ToggleTheme()<CR>

" Tagbar
nnoremap <F8> :TagbarToggle<CR>

" Undotree
nnoremap <leader>u :UndotreeToggle<CR>

" -----------------------------------------------------------------------------
" 5. Neovim-Specific Lua Configuration
" -----------------------------------------------------------------------------
if has('nvim-0.7')
  lua << EOF
-- Mason setup for LSP management
require('mason').setup({
  ui = { border = 'rounded' }
})

require('mason-lspconfig').setup({
  ensure_installed = {
    'lua_ls', 'pyright', 'ts_ls', 'gopls', 'rust_analyzer',
    'clangd', 'bashls', 'yamlls', 'jsonls'
  },
  automatic_installation = true,
})

-- LSP setup
local lspconfig = require('lspconfig')
local capabilities = require('cmp_nvim_lsp').default_capabilities()

local on_attach = function(client, bufnr)
  local opts = { buffer = bufnr, silent = true }
  vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
  vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
  vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
  vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
  vim.keymap.set('n', '<leader>D', vim.lsp.buf.type_definition, opts)
  vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, opts)
  vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action, opts)
  vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
  vim.keymap.set('n', '<leader>f', vim.lsp.buf.format, opts)
end

local servers = { 'pyright', 'ts_ls', 'lua_ls', 'gopls', 'rust_analyzer', 'clangd', 'bashls' }
for _, lsp in ipairs(servers) do
  lspconfig[lsp].setup({
    on_attach = on_attach,
    capabilities = capabilities,
  })
end

-- Completion setup
local cmp = require('cmp')
local luasnip = require('luasnip')
require('luasnip.loaders.from_vscode').lazy_load()

cmp.setup({
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  mapping = cmp.mapping.preset.insert({
    ['<C-b>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.abort(),
    ['<CR>'] = cmp.mapping.confirm({ select = true }),
    ['<Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end, { 'i', 's' }),
    ['<S-Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { 'i', 's' }),
  }),
  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    { name = 'luasnip' },
    { name = 'nvim_lua' },
  }, {
    { name = 'buffer', keyword_length = 3 },
    { name = 'path' },
  })
})

-- Treesitter setup
require('nvim-treesitter.configs').setup({
  ensure_installed = {
    "lua", "vim", "vimdoc", "python", "javascript", "typescript",
    "go", "rust", "c", "cpp", "bash", "yaml", "json", "html", "css"
  },
  auto_install = false,
  highlight = { enable = true },
  indent = { enable = true },
  textobjects = {
    select = {
      enable = true,
      lookahead = true,
      keymaps = {
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        ["ac"] = "@class.outer",
        ["ic"] = "@class.inner",
      },
    },
  },
})

-- File explorer
require('nvim-tree').setup({
  disable_netrw = true,
  update_focused_file = { enable = true },
  diagnostics = { enable = true },
  view = { width = 35 },
  renderer = {
    highlight_git = true,
    icons = {
      show = { git = true },
    }
  },
})

-- Telescope
require('telescope').setup({
  defaults = {
    prompt_prefix = "🔍 ",
    selection_caret = " ",
    path_display = { "truncate" },
    file_ignore_patterns = {
      "node_modules", ".git/", "*.o", "*.class", "%.pdf",
      "%.mkv", "%.mp4", "%.zip"
    },
  },
  extensions = {
    fzf = {
      fuzzy = true,
      override_generic_sorter = true,
      override_file_sorter = true,
      case_mode = "smart_case",
    },
  }
})

require('telescope').load_extension('fzf')
require('telescope').load_extension('ui-select')

-- Lualine
require('lualine').setup({
  options = {
    theme = 'solarized',
    globalstatus = true,
  },
})

-- Bufferline
require('bufferline').setup({
  options = {
    diagnostics = "nvim_lsp",
    separator_style = "thin",
    offsets = {
      { filetype = "NvimTree", text = "File Explorer", text_align = "left" }
    },
  }
})

-- Gitsigns
require('gitsigns').setup({
  current_line_blame = true,
  current_line_blame_opts = { delay = 300 },
})

-- Toggleterm
require('toggleterm').setup({
  size = 20,
  direction = 'float',
  float_opts = { border = 'curved' },
})

-- Conform (formatting)
require('conform').setup({
  formatters_by_ft = {
    python = { "black" },
    javascript = { "prettier" },
    typescript = { "prettier" },
    lua = { "stylua" },
    go = { "gofmt" },
    rust = { "rustfmt" },
  },
  format_on_save = {
    timeout_ms = 500,
    lsp_fallback = true,
  },
})

-- Nvim-lint
require('lint').linters_by_ft = {
  python = { 'ruff' },
  javascript = { 'eslint' },
  typescript = { 'eslint' },
}

vim.api.nvim_create_autocmd({ "BufWritePost" }, {
  callback = function()
    require("lint").try_lint()
  end,
})

-- DAP (Debugging)
local dap = require('dap')
local dapui = require('dapui')

dap.adapters.python = {
  type = 'executable',
  command = 'python3',
  args = { '-m', 'debugpy.adapter' },
}

dap.configurations.python = {
  {
    type = 'python',
    request = 'launch',
    name = 'Launch file',
    program = '${file}',
    pythonPath = function()
      return '/usr/bin/python3'
    end,
  },
}

dapui.setup()
require('nvim-dap-virtual-text').setup()

dap.listeners.after.event_initialized["dapui_config"] = function()
  dapui.open()
end

-- Autopairs
require('nvim-autopairs').setup({
  fast_wrap = {},
})

-- Hop
require('hop').setup()
vim.keymap.set('n', '<leader>j', ':HopChar1<CR>', { desc = "Hop to character" })
vim.keymap.set('n', '<leader>J', ':HopWord<CR>', { desc = "Hop to word" })

-- Which-key
require('which-key').setup()
require('which-key').register({
  ["<leader>f"] = { name = "+find" },
  ["<leader>g"] = { name = "+git" },
  ["<leader>d"] = { name = "+debug" },
  ["<leader>t"] = { name = "+test/terminal" },
  ["<leader>w"] = { name = "+window" },
  ["<leader>b"] = { name = "+buffer" },
  ["<leader>u"] = { name = "+toggle" },
})

-- Notifications
require('notify').setup({
  timeout = 3000,
  render = "default",
})
vim.notify = require('notify')

-- Todo comments
require('todo-comments').setup()

-- Trouble (diagnostics)
require('trouble').setup()

-- Neotest
require('neotest').setup({
  adapters = {
    require('neotest-python'),
    require('neotest-jest'),
    require('neotest-go'),
  },
})

-- Persistence (sessions)
require('persistence').setup()

-- Highlight on yank
vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank({ timeout = 300 })
  end,
})

EOF

  " Neovim-specific key mappings
  nnoremap <leader>df <cmd>lua vim.diagnostic.open_float()<CR>
  nnoremap <leader>dq <cmd>lua vim.diagnostic.setqflist()<CR>
  nnoremap [d <cmd>lua vim.diagnostic.goto_prev()<CR>
  nnoremap ]d <cmd>lua vim.diagnostic.goto_next()<CR>
  
  nnoremap <leader>db <cmd>lua require('dap').toggle_breakpoint()<CR>
  nnoremap <leader>dc <cmd>lua require('dap').continue()<CR>
  nnoremap <leader>di <cmd>lua require('dap').step_into()<CR>
  nnoremap <leader>do <cmd>lua require('dap').step_over()<CR>
  nnoremap <leader>du <cmd>lua require('dapui').toggle()<CR>
  
  nnoremap <leader>tr <cmd>lua require('neotest').run.run()<CR>
  nnoremap <leader>tf <cmd>lua require('neotest').run.run(vim.fn.expand('%'))<CR>
  nnoremap <leader>ts <cmd>lua require('neotest').summary.toggle()<CR>
  
  nnoremap <leader>xx <cmd>TroubleToggle<CR>
endif

" -----------------------------------------------------------------------------
" 6. Enhanced Terminal Execution System
" -----------------------------------------------------------------------------
function! s:ValidateFile(filepath)
  if !filereadable(a:filepath)
    echohl ErrorMsg
    echo 'Error: File not readable: ' . a:filepath
    echohl None
    return 0
  endif
  return 1
endfunction

function! s:GetFileInfo()
  let l:filepath = expand('%:p')
  return {
  \ 'filepath': l:filepath,
  \ 'dirname': fnamemodify(l:filepath, ':h'),
  \ 'filename': expand('%:t'),
  \ 'basename': expand('%:t:r'),
  \ 'extension': expand('%:e'),
  \ 'filetype': &filetype
  \ }
endfunction

let s:command_map = {
\ 'python': {
\   'run': 'python3 "{filepath}"',
\   'test': 'python3 -m pytest "{dirname}"',
\   'debug': 'python3 -m pdb "{filepath}"'
\ },
\ 'javascript': {
\   'run': 'node "{filepath}"',
\   'test': 'npm test',
\   'debug': 'node --inspect-brk "{filepath}"'
\ },
\ 'typescript': {
\   'compile': 'tsc "{filepath}"',
\   'run': 'ts-node "{filepath}"',
\   'test': 'npm test'
\ },
\ 'c': {
\   'compile': 'gcc -Wall -Wextra -O2 -g "{filepath}" -o "{basename}" -lm',
\   'run': './{basename}',
\   'debug': 'gdb ./{basename}'
\ },
\ 'cpp': {
\   'compile': 'g++ -Wall -Wextra -O2 -g -std=c++17 "{filepath}" -o "{basename}" -lm',
\   'run': './{basename}',
\   'debug': 'gdb ./{basename}'
\ },
\ 'rust': {
\   'compile': 'rustc "{filepath}"',
\   'run': './{basename}',
\   'build': 'cargo build',
\   'test': 'cargo test',
\   'debug': 'rust-gdb ./{basename}'
\ },
\ 'go': {
\   'run': 'go run "{filepath}"',
\   'build': 'go build -o "{basename}" "{filepath}"',
\   'test': 'go test',
\   'debug': 'dlv debug "{filepath}"'
\ },
\ 'java': {
\   'compile': 'javac -cp . "{filepath}"',
\   'run': 'java -cp . {basename}',
\   'debug': 'jdb -classpath . {basename}'
\ },
\ 'ruby': {
\   'run': 'ruby "{filepath}"',
\   'test': 'ruby -Itest "{filepath}"'
\ },
\ 'php': { 'run': 'php "{filepath}"' },
\ 'perl': { 'run': 'perl "{filepath}"' },
\ 'lua': { 'run': 'lua "{filepath}"' },
\ 'r': { 'run': 'Rscript "{filepath}"' },
\ 'julia': { 'run': 'julia "{filepath}"' },
\ 'sh': { 'run': 'bash "{filepath}"' },
\ 'zsh': { 'run': 'zsh "{filepath}"' },
\ 'vim': { 'run': 'vim -s "{filepath}"' },
\ 'haskell': {
\   'compile': 'ghc "{filepath}"',
\   'run': './{basename}',
\   'interpret': 'runhaskell "{filepath}"'
\ }
\ }

function! s:FormatCmd(template, fileinfo)
  return substitute(a:template, '{\(\w\+\)}', '\=get(a:fileinfo, submatch(1), submatch(0))', 'g')
endfunction

function! s:RunInTerminal(action)
  if &modified | write | endif
  let l:fileinfo = s:GetFileInfo()
  if !s:ValidateFile(l:fileinfo.filepath) | return | endif

  let l:entry = get(s:command_map, l:fileinfo.filetype, {})
  let l:command = get(l:entry, a:action, '')

  if empty(l:command)
    echohl WarningMsg
    echo 'No ' . a:action . ' command for filetype: ' . l:fileinfo.filetype
    echohl None
    return
  endif

  let l:formatted_cmd = s:FormatCmd(l:command, l:fileinfo)
  let l:term_name = a:action . '_' . l:fileinfo.filetype

  try
    if has('nvim-0.7')
      execute 'ToggleTerm direction=float name=' . l:term_name . ' cmd="' . l:formatted_cmd . '"'
    else
      silent! execute 'FloatermKill ' . l:term_name
      execute 'FloatermNew --name=' . l:term_name .
            \ ' --autoclose=0 --cwd=' . fnameescape(l:fileinfo.dirname) .
            \ ' ' . l:formatted_cmd
    endif
    echo a:action . ': ' . l:fileinfo.filename
  catch
    echohl ErrorMsg
    echo 'Failed to ' . a:action . ': ' . v:exception
    echohl None
  endtry
endfunction

" Execution mappings
nnoremap <silent> .r :call <SID>RunInTerminal('run')<CR>
nnoremap <silent> .c :call <SID>RunInTerminal('compile')<CR>
nnoremap <silent> .b :call <SID>RunInTerminal('build')<CR>
nnoremap <silent> .t :call <SID>RunInTerminal('test')<CR>
nnoremap <silent> .d :call <SID>RunInTerminal('debug')<CR>
nnoremap <silent> .i :call <SID>RunInTerminal('interpret')<CR>

" Function key mappings
nnoremap <F5> :call <SID>RunInTerminal('run')<CR>
nnoremap <F6> :call <SID>RunInTerminal('compile')<CR>
nnoremap <F7> :call <SID>RunInTerminal('build')<CR>
nnoremap <F9> :call <SID>RunInTerminal('test')<CR>

" -----------------------------------------------------------------------------
" 7. Database Integration
" -----------------------------------------------------------------------------
let g:db_ui_use_nerd_fonts = 1
let g:db_ui_winwidth = 40
let g:db_ui_save_location = has('nvim') ? stdpath('config') . '/db_ui' : expand('~/.vim/db_ui')

" Database connections (use environment variables)
let g:dbs = {}
if !empty($DATABASE_DEV_URL)
  let g:dbs['dev'] = $DATABASE_DEV_URL
endif
if !empty($DATABASE_STAGING_URL)
  let g:dbs['staging'] = $DATABASE_STAGING_URL
endif
if !empty($DATABASE_LOCAL_URL)
  let g:dbs['local'] = $DATABASE_LOCAL_URL
endif

nnoremap <leader>Du :DBUIToggle<CR>
nnoremap <leader>Df :DBUIFindBuffer<CR>
nnoremap <leader>Dr :DBUIRenameBuffer<CR>
nnoremap <leader>Dq :DBUILastQueryInfo<CR>

" -----------------------------------------------------------------------------
" 8. Clipboard Integration
" -----------------------------------------------------------------------------
set clipboard=unnamed,unnamedplus

function! s:SetupClipboard()
  if has('wsl') && executable('clip.exe')
    let g:clipboard = {
          \ 'name': 'WSL',
          \ 'copy': { '+': 'clip.exe', '*': 'clip.exe' },
          \ 'paste': { '+': 'powershell.exe -NoProfile -Command "Get-Clipboard"',
          \            '*': 'powershell.exe -NoProfile -Command "Get-Clipboard"' },
          \ 'cache_enabled': 0 }
  elseif $XDG_SESSION_TYPE ==# 'wayland' && executable('wl-copy')
    augroup WaylandClipboard
      autocmd!
      autocmd TextYankPost * if v:event.operator ==# 'y' |
                           \ call system('wl-copy', @") | endif
    augroup END
  elseif executable('xclip')
    augroup X11Clipboard
      autocmd!
      autocmd TextYankPost * if v:event.operator ==# 'y' |
                           \ call system('xclip -selection clipboard', @") | endif
    augroup END
  elseif has('mac') && executable('pbcopy')
    augroup MacClipboard
      autocmd!
      autocmd TextYankPost * if v:event.operator ==# 'y' |
                           \ call system('pbcopy', @") | endif
    augroup END
  endif
endfunction

augroup ClipboardSetup
  autocmd!
  autocmd VimEnter * call s:SetupClipboard()
augroup END

" Clipboard utilities
nnoremap <leader>yf :let @+=expand('%:t')<CR>:echo 'Copied filename'<CR>
nnoremap <leader>yF :let @+=expand('%:p')<CR>:echo 'Copied full path'<CR>
nnoremap <leader>yd :let @+=expand('%:p:h')<CR>:echo 'Copied directory'<CR>

" -----------------------------------------------------------------------------
" 9. Autocommands and Event Handling
" -----------------------------------------------------------------------------
function! HandleLargeFile()
  let l:file_size = getfsize(expand('%'))
  if l:file_size > 10485760  " 10MB
    setlocal eventignore+=FileType
    setlocal bufhidden=unload
    setlocal undolevels=-1
    setlocal syntax=off
    setlocal nowrap
    setlocal noundofile
    setlocal noswapfile
    setlocal nocursorline
    setlocal nocursorcolumn
    setlocal norelativenumber
    echo 'Large file (' . (l:file_size / 1048576) . 'MB): optimizations applied'
  endif
endfunction

function! StripTrailingWhitespace()
  if &modifiable && &filetype !~# 'markdown'
    let l:save = winsaveview()
    silent! %s/\s\+$//e
    call winrestview(l:save)
  endif
endfunction

function! AutoCreateDirectories()
  let l:dir = expand('%:p:h')
  if !isdirectory(l:dir)
    call mkdir(l:dir, 'p')
  endif
endfunction

function! RestoreCursorPosition()
  if line("'\"") > 0 && line("'\"") <= line("$") && &filetype !~# 'commit'
    execute "normal! g`\""
  endif
endfunction

augroup VimrcAutocommands
  autocmd!
  
  " Performance
  autocmd BufReadPre * call HandleLargeFile()
  
  " Auto-save
  autocmd FocusLost * silent! wall
  autocmd BufWritePre * call StripTrailingWhitespace()
  autocmd BufWritePre * call AutoCreateDirectories()
  
  " UI
  autocmd VimResized * wincmd =
  autocmd BufReadPost * call RestoreCursorPosition()
  
  " Terminal
  if has('nvim')
    autocmd TermOpen * setlocal nonumber norelativenumber signcolumn=no
  endif
  
  " Language-specific settings
  autocmd FileType markdown setlocal spell textwidth=80 wrap linebreak
  autocmd FileType gitcommit setlocal spell textwidth=72
  autocmd FileType python setlocal tabstop=4 shiftwidth=4 expandtab
  autocmd FileType javascript,typescript,html,css,json,yaml,lua
        \ setlocal tabstop=2 shiftwidth=2 expandtab
  autocmd FileType go setlocal tabstop=4 shiftwidth=4 noexpandtab
  autocmd FileType make setlocal tabstop=8 shiftwidth=8 noexpandtab
  autocmd FileType sql setlocal tabstop=2 shiftwidth=2 expandtab commentstring=--%s
  autocmd FileType vim setlocal tabstop=2 shiftwidth=2 expandtab
  
  " SQL completion
  autocmd FileType sql setlocal omnifunc=vim_dadbod_completion#omni
augroup END

" -----------------------------------------------------------------------------
" 10. Custom Commands and Utilities
" -----------------------------------------------------------------------------
command! VimInfo call s:ShowVimInfo()
command! ReloadConfig source $MYVIMRC | echo 'Configuration reloaded!'
command! EditConfig edit $MYVIMRC
command! CleanBuffers call s:CloseOtherBuffers()
command! FindProjectRoot call s:FindProjectRoot()

function! s:ShowVimInfo()
  echo '=================== VIM IDE INFO ==================='
  echo 'Version: ' . (has('nvim') ? 'Neovim' : 'Vim ' . v:version/100 . '.' . v:version%100)
  echo 'Config: ' . $MYVIMRC
  echo 'Filetype: ' . &filetype
  echo 'Colorscheme: ' . (exists('g:colors_name') ? g:colors_name : 'none')
  echo 'Python3: ' . (executable('python3') ? trim(system('python3 --version')) : 'N/A')
  echo 'Node.js: ' . (executable('node') ? trim(system('node --version')) : 'N/A')
  echo 'Git: ' . (executable('git') ? trim(system('git --version')) : 'N/A')
  if has('nvim-0.7')
    echo 'LSP clients: ' . len(vim.lsp.get_active_clients())
  endif
  echo '===================================================='
endfunction

function! s:CloseOtherBuffers()
  let l:current = bufnr('%')
  let l:last = bufnr(')
  let l:count = 0
  
  for i in range(1, l:last)
    if buflisted(i) && i != l:current
      execute 'bdelete ' . i
      let l:count += 1
    endif
  endfor
  
  echo 'Closed ' . l:count . ' buffers'
endfunction

function! s:FindProjectRoot()
  let l:markers = ['.git', '.svn', '.hg', 'package.json', 'Cargo.toml', 'go.mod', 'Makefile', 'pyproject.toml']
  let l:current = expand('%:p:h')

  while l:current != '/'
    for l:marker in l:markers
      let l:path = l:current . '/' . l:marker
      if isdirectory(l:path) || filereadable(l:path)
        execute 'cd ' . l:current
        echo 'Project root: ' . l:current
        return
      endif
    endfor
    let l:current = fnamemodify(l:current, ':h')
  endwhile

  echo 'Project root not found'
endfunction

" -----------------------------------------------------------------------------
" 11. Plugin-Specific Configurations
" -----------------------------------------------------------------------------

" UltiSnips
let g:UltiSnipsExpandTrigger = '<C-j>'
let g:UltiSnipsJumpForwardTrigger = '<C-j>'
let g:UltiSnipsJumpBackwardTrigger = '<C-k>'

" Floaterm (Vim)
if !has('nvim-0.7')
  let g:floaterm_width = 0.9
  let g:floaterm_height = 0.9
  let g:floaterm_autoclose = 0
endif

" FZF
let g:fzf_layout = { 'down': '~40%' }
let g:fzf_history_dir = has('nvim') ? stdpath('data') . '/fzf-history' : expand('~/.vim/fzf-history')

" ALE (Vim only)
if !has('nvim-0.7')
  let g:ale_linters = {
  \ 'python': ['ruff', 'mypy'],
  \ 'javascript': ['eslint'],
  \ 'typescript': ['eslint'],
  \ 'go': ['gopls'],
  \ 'rust': ['analyzer'],
  \ }
  
  let g:ale_fixers = {
  \ '*': ['remove_trailing_lines', 'trim_whitespace'],
  \ 'python': ['black', 'isort'],
  \ 'javascript': ['prettier'],
  \ 'typescript': ['prettier'],
  \ 'go': ['gofmt'],
  \ }
  
  let g:ale_fix_on_save = 1
  let g:ale_lint_on_text_changed = 'never'
  let g:ale_lint_on_insert_leave = 0
  let g:ale_lint_on_enter = 0
endif

" Gutentags
let g:gutentags_cache_dir = has('nvim') ? stdpath('cache') . '/tags' : expand('~/.vim/tags')
let g:gutentags_generate_on_new = 1
let g:gutentags_generate_on_missing = 1
let g:gutentags_generate_on_write = 1

" GitGutter (Vim only)
if !has('nvim-0.7')
  let g:gitgutter_enabled = 1
  let g:gitgutter_map_keys = 0
  set updatetime=100
endif

" Better-escape
let g:better_escape_shortcut = ['jk', 'kj']
let g:better_escape_interval = 200

" Which-key (Vim)
if !has('nvim-0.7')
  nnoremap <silent> <leader> :<c-u>WhichKey '<Space>'<CR>
  nnoremap <silent> <localleader> :<c-u>WhichKey ','<CR>
endif

" Copilot
if exists('g:loaded_copilot')
  let g:copilot_no_tab_map = v:true
  imap <silent><script><expr> <C-J> copilot#Accept("\<CR>")
endif

" -----------------------------------------------------------------------------
" 12. Final Settings and Initialization
" -----------------------------------------------------------------------------

" Load local vimrc if it exists
if filereadable(expand('~/.vimrc.local'))
  source ~/.vimrc.local
endif

" Ensure plugins are installed on first run
augroup PluginCheck
  autocmd!
  autocmd VimEnter * if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
    \| PlugInstall --sync | source $MYVIMRC
  \| endif
augroup END

" Welcome message
if has('vim_starting') && !has('nvim')
  augroup WelcomeMessage
    autocmd!
    autocmd VimEnter * echo 'Vim IDE loaded. Press <leader> to see key mappings.'
  augroup END
endif