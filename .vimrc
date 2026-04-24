" =============================================================================
" VIM IDE
" =============================================================================

set nocompatible
if has('vim_starting')
  set encoding=utf-8
endif
scriptencoding utf-8

let s:boot_time = reltime()

" =============================================================================
" LAYER 0: STARTUP PROFILING & FEATURE FLAGS
" =============================================================================

let g:vimide_debug           = 0
let g:vimide_minimal         = 0
let g:vimide_diagnostics     = 'coc'
" When 1: test/db/rest defer to BufReadPost; debug defers to CmdUndefined.
" LSP and git are always eager regardless of this flag.
let g:vimide_lazy_aggressive = 1

" Feature module flags — override in ~/.vimrc.local
let g:module_git_enabled     = 1
let g:module_lsp_enabled     = 1
let g:module_test_enabled    = 1
let g:module_db_enabled      = 1
let g:module_rest_enabled    = 1
let g:module_debug_enabled   = 1

" =============================================================================
" LAYER 1: CORE ABSTRACTION APIs
" =============================================================================

" --- Logging -----------------------------------------------------------------

function! Log(msg, level) abort
  let l:prefix = '[vimide] '
  if a:level ==# 'error'
    echohl ErrorMsg
  elseif a:level ==# 'warn'
    echohl WarningMsg
  else
    echohl None
  endif
  echom l:prefix . a:msg
  echohl None
endfunction

" --- Profiling mark ----------------------------------------------------------
" Pure observer. Enable with: let g:vimide_debug = 1

function! s:Mark(label) abort
  if !g:vimide_debug | return | endif
  call Log(a:label . ' +' . reltimestr(reltime(s:boot_time)), 'info')
endfunction

" --- Error boundary ----------------------------------------------------------
" Named so failures identify their source in the log.

function! s:Safe(name, Fn) abort
  try
    call call(a:Fn, [])
  catch
    call Log(a:name . ' failed: ' . v:exception, 'error')
  endtry
endfunction

" --- State store -------------------------------------------------------------

let s:state = {}

function! SetState(key, val) abort
  let s:state[a:key] = a:val
endfunction

function! State(key) abort
  return get(s:state, a:key, '')
endfunction

function! RequireState(key) abort
  let l:val = State(a:key)
  if empty(l:val)
    throw 'Missing state: ' . a:key
  endif
  return l:val
endfunction

" --- Feature predicate -------------------------------------------------------

function! FeatureEnabled(name) abort
  return get(g:, 'module_' . a:name . '_enabled', 0)
endfunction

" --- Module registry ---------------------------------------------------------
"
" Contract:
"   - Every feature is registered exactly once via s:RegisterModule()
"   - s:LoadModules() is the single dispatch point
"   - Features load once per session (immutable session model)
"   - s:TeardownModule() / s:ReloadModule() are debug tools only
"   - Teardown does NOT guarantee clean state for all side effects
"     (installed <Plug> maps persist); document this limitation per module
"
" Spec keys:
"   init     funcref  — required; called on load
"   teardown funcref  — optional; called on teardown
"   deps     list     — module names to load first (resolved before init)
"   lazy     string   — '' (eager) | 'buf' (BufReadPost) | 'cmd' (CmdUndefined)
"                       ignored when g:vimide_lazy_aggressive = 0

let s:modules = {}

function! s:RegisterModule(name, spec) abort
  let s:modules[a:name] = extend({
    \ 'enabled':  FeatureEnabled(a:name),
    \ 'loaded':   0,
    \ 'init':     v:null,
    \ 'teardown': v:null,
    \ 'deps':     [],
    \ 'lazy':     '',
    \ }, a:spec)
endfunction

function! s:LoadModule(name) abort
  if !has_key(s:modules, a:name)
    call Log('Unknown module: ' . a:name, 'error') | return
  endif
  let l:m = s:modules[a:name]
  if !l:m.enabled || l:m.loaded | return | endif
  for l:dep in l:m.deps
    call s:LoadModule(l:dep)
  endfor
  let s:modules[a:name].loaded = 1
  if l:m.init isnot v:null
    call s:Mark(a:name . ' init start')
    call s:Safe(a:name . ':init', l:m.init)
    call s:Mark(a:name . ' init done')
  endif
endfunction

function! s:TeardownModule(name) abort
  if !has_key(s:modules, a:name) | return | endif
  let l:m = s:modules[a:name]
  if !l:m.loaded | return | endif
  if l:m.teardown isnot v:null
    call s:Safe(a:name . ':teardown', l:m.teardown)
  endif
  let s:modules[a:name].loaded = 0
endfunction

function! s:ReloadModule(name) abort
  call s:TeardownModule(a:name)
  call s:LoadModule(a:name)
endfunction

function! s:LoadModules() abort
  call s:Mark('LoadModules start')
  for l:name in keys(s:modules)
    let l:m = s:modules[l:name]
    if !l:m.enabled | continue | endif
    let l:strategy = g:vimide_lazy_aggressive ? l:m.lazy : ''
    if l:strategy ==# 'buf'
      call Augroup('LazyLoad_' . l:name, [
        \ 'BufReadPost * ++once call s:LoadModule(' . string(l:name) . ')',
        \ ])
    elseif l:strategy ==# 'cmd'
      call Augroup('LazyLoad_' . l:name, [
        \ 'CmdUndefined * call s:LoadModule(' . string(l:name) . ')',
        \ ])
    else
      call s:LoadModule(l:name)
    endif
  endfor
  call s:Mark('LoadModules done')
endfunction

" --- Map() -------------------------------------------------------------------

function! Map(mode, lhs, rhs, opts) abort
  let l:silent = get(a:opts, 'silent', 1) ? '<silent>' : ''
  let l:expr   = get(a:opts, 'expr',   0) ? '<expr>'   : ''
  let l:buf    = get(a:opts, 'buffer', 0) ? '<buffer>'  : ''
  let l:nore   = get(a:opts, 'noremap',1) ? 'noremap'  : 'map'
  execute printf('%s%s %s %s %s %s %s',
    \ a:mode, l:nore, l:silent, l:expr, l:buf, a:lhs, a:rhs)
endfunction

" --- Augroup() ---------------------------------------------------------------

function! Augroup(name, cmds) abort
  execute 'augroup ' . a:name
  autocmd!
  for l:cmd in a:cmds
    execute 'autocmd ' . l:cmd
  endfor
  augroup END
endfunction

" --- Runner registry ---------------------------------------------------------

let s:runners = {}

function! RegisterRunner(ft, actions) abort
  let s:runners[a:ft] = a:actions
endfunction

" --- Task system -------------------------------------------------------------

let s:tasks = {}

function! RegisterTask(name, config) abort
  let s:tasks[a:name] = a:config
endfunction

function! RunTask(name) abort
  if !has_key(s:tasks, a:name)
    call Log('Task not found: ' . a:name, 'error') | return
  endif
  let l:task = s:tasks[a:name]
  for l:dep in get(l:task, 'deps', [])
    call RunTask(l:dep)
  endfor
  if has_key(l:task, 'run')
    call call(l:task.run, [])
  endif
  call SetState('last_task', { 'name': a:name, 'time': localtime() })
endfunction

" =============================================================================
" LAYER 2: PERFORMANCE & CORE SETTINGS
" =============================================================================

set regexpengine=0 synmaxcol=300 lazyredraw
set updatetime=250 redrawtime=1500
set ttimeoutlen=10 timeoutlen=500

if has('mouse_sgr') | set ttymouse=sgr | endif
set mouse=a

if has('patch-9.0.0640')
  set smoothscroll
endif

filetype plugin indent on
syntax enable

set fileencodings=utf-8,ucs-bom,latin1
set backspace=indent,eol,start history=5000 undolevels=2000 undoreload=10000
set nrformats-=octal virtualedit=block nojoinspaces

set number relativenumber cursorline laststatus=2 scrolloff=8 sidescrolloff=5
set showcmd ruler showmatch matchtime=2 display+=lastline
set signcolumn=yes pumheight=14 cmdheight=1 noshowmode shortmess+=acFI

set incsearch hlsearch ignorecase smartcase wrapscan
if executable('rg')
  set grepprg=rg\ --vimgrep\ --smart-case\ --follow grepformat=%f:%l:%c:%m
endif

set autoindent expandtab tabstop=4 softtabstop=4 shiftwidth=4 shiftround smarttab
set hidden switchbuf=useopen,usetab splitbelow splitright winminheight=0 winminwidth=0
set wildmenu wildmode=longest:full,full
set wildignore+=*.o,*~,*.pyc,*.class,*.jar,*.lock
set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*/node_modules/*,*/bower_components/*
set wildignore+=*.DS_Store,*.log,*.tmp

set completeopt=menuone,noselect,noinsert

for s:d in ['swap', 'backup', 'undo', 'tags', 'sessions', 'fzf-history', 'db_ui']
  if !isdirectory(expand('~/.vim/' . s:d))
    call mkdir(expand('~/.vim/' . s:d), 'p', 0700)
  endif
endfor

set swapfile   directory=~/.vim/swap//
set backup     nowritebackup backupdir=~/.vim/backup//
set autoread

if has('persistent_undo')
  set undofile undodir=~/.vim/undo
endif

set foldmethod=indent foldlevel=99 foldlevelstart=99
set list listchars=tab:▸\ ,trail:·,extends:→,precedes:←,nbsp:⦸
set fillchars=vert:│,fold:─
set spelllang=en_us

if has('termguicolors') && ($COLORTERM =~# 'truecolor\|24bit')
  set termguicolors
endif

if has('gui_running')
  set guifont=JetBrainsMono\ Nerd\ Font\ Mono:h12 guioptions=ac columns=120 lines=35
endif

set clipboard=unnamed,unnamedplus
if has('wsl') && executable('clip.exe')
  let g:clipboard = {
    \ 'name':  'WSL',
    \ 'copy':  { '+': 'clip.exe', '*': 'clip.exe' },
    \ 'paste': { '+': 'powershell.exe -NoProfile -Command "Get-Clipboard"',
    \            '*': 'powershell.exe -NoProfile -Command "Get-Clipboard"' },
    \ 'cache_enabled': 0 }
endif

call Augroup('SensitiveFiles', [
  \ 'BufNewFile,BufRead .env,.env.*,*.env,*.pem,*.key,*.p12,id_rsa,id_ed25519,*.gpg'
  \   . ' setlocal noswapfile nobackup noundofile',
  \ ])

call Augroup('FormatOptions', [
  \ 'FileType * setlocal formatoptions-=cro',
  \ 'FileType gitcommit setlocal formatoptions+=cro',
  \ ])

" =============================================================================
" LAYER 3: PLUGIN MANAGER (vim-plug)
" =============================================================================

function! s:EnsureVimPlug() abort
  let l:path = expand('~/.vim/autoload/plug.vim')
  if !empty(glob(l:path)) | return 1 | endif
  try
    let l:url = 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
    if executable('curl')
      call system('curl -fsSL --tlsv1.2 --create-dirs -o ' . shellescape(l:path) . ' ' . l:url)
    elseif executable('wget')
      call system('wget --secure-protocol=TLSv1_2 -O ' . shellescape(l:path) . ' ' . l:url)
    else
      throw 'neither curl nor wget found'
    endif
    if v:shell_error != 0 | throw 'download failed' | endif
    return 1
  catch
    call Log('vim-plug bootstrap failed: ' . v:exception, 'error')
    return 0
  endtry
endfunction

if !s:EnsureVimPlug() | finish | endif

call plug#begin(expand('~/.vim/plugged'))

" APPEARANCE
Plug 'catppuccin/vim', { 'as': 'catppuccin' }
Plug 'morhetz/gruvbox'
Plug 'joshdick/onedark.vim'
Plug 'itchyny/lightline.vim'
Plug 'mengelbrecht/lightline-bufferline'
Plug 'ryanoasis/vim-devicons'

" FILE EXPLORER
Plug 'lambdalisue/fern.vim', { 'on': 'Fern' }
Plug 'lambdalisue/fern-git-status.vim', { 'on': 'Fern' }
Plug 'lambdalisue/nerdfont.vim', { 'on': 'Fern' }
Plug 'lambdalisue/fern-renderer-nerdfont.vim', { 'on': 'Fern' }
Plug 'LumaKernel/fern-mapping-fzf.vim', { 'on': 'Fern' }
Plug 'lambdalisue/vim-fern-hijack'

" NAVIGATION
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'christoomey/vim-tmux-navigator'
Plug 'preservim/tagbar', { 'on': 'TagbarToggle' }
Plug 'ludovicchabant/vim-gutentags'
Plug 'mhinz/vim-startify'
Plug 'andymass/vim-matchup'

" LSP / COMPLETION
Plug 'neoclide/coc.nvim', { 'branch': 'release' }

" DAP
Plug 'puremourning/vimspector', { 'on': [] }

" EDITING
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-sleuth'
Plug 'cohama/lexima.vim'
Plug 'wellle/targets.vim'
Plug 'kana/vim-textobj-user'
Plug 'kana/vim-textobj-indent'
Plug 'glts/vim-textobj-comment'
Plug 'matze/vim-move'
Plug 'mbbill/undotree', { 'on': 'UndotreeToggle' }
Plug 'jdhao/better-escape.vim'
Plug 'mg979/vim-visual-multi'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'wellle/context.vim'
Plug 'haya14busa/vim-asterisk'
Plug 'junegunn/vim-easy-align'

" SNIPPETS
Plug 'hrsh7th/vim-vsnip'
Plug 'honza/vim-snippets'

" LINTING
if g:vimide_diagnostics !=# 'coc'
  Plug 'dense-analysis/ale'
endif

" GIT
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
Plug 'airblade/vim-gitgutter'
Plug 'junegunn/gv.vim'
Plug 'whiteinge/diffconflicts'

" LANGUAGE SUPPORT
Plug 'rust-lang/rust.vim', { 'for': 'rust' }
Plug 'fatih/vim-go', { 'for': 'go', 'do': ':GoUpdateBinaries' }
Plug 'rhysd/vim-llvm', { 'for': ['c', 'cpp'] }
Plug 'ziglang/zig.vim', { 'for': 'zig' }
Plug 'pangloss/vim-javascript', { 'for': ['javascript', 'javascriptreact'] }
Plug 'leafgarland/typescript-vim', { 'for': ['typescript', 'typescriptreact'] }
Plug 'maxmellon/vim-jsx-pretty', { 'for': ['javascriptreact', 'typescriptreact'] }
Plug 'jparise/vim-graphql', { 'for': 'graphql' }
Plug 'mustache/vim-mustache-handlebars', { 'for': ['html.handlebars', 'mustache'] }
Plug 'Vimjas/vim-python-pep8-indent', { 'for': 'python' }
Plug 'vim-ruby/vim-ruby', { 'for': 'ruby' }
Plug 'tpope/vim-rails', { 'for': 'ruby' }
Plug 'udalov/kotlin-vim', { 'for': 'kotlin' }
Plug 'vim-scripts/fortran.vim', { 'for': 'fortran' }
Plug 'suoto/vim-hdl', { 'for': ['vhdl', 'verilog'] }
Plug 'hashivim/vim-terraform', { 'for': 'terraform' }
Plug 'pearofducks/ansible-vim', { 'for': ['yaml.ansible', 'yaml'] }
Plug 'chr4/nginx.vim', { 'for': 'nginx' }
Plug 'ekalinin/Dockerfile.vim', { 'for': 'Dockerfile' }
Plug 'towolf/vim-helm', { 'for': 'helm' }
Plug 'fladson/vim-kitty', { 'for': 'kitty' }
Plug 'elzr/vim-json', { 'for': 'json' }
Plug 'cespare/vim-toml', { 'for': 'toml' }
Plug 'stephpy/vim-yaml', { 'for': 'yaml' }
Plug 'chrisbra/csv.vim', { 'for': 'csv' }
Plug 'godlygeek/tabular'
Plug 'preservim/vim-markdown', { 'for': 'markdown' }
Plug 'iamcco/markdown-preview.nvim', { 'for': 'markdown', 'do': 'cd app && npm install' }
Plug 'lervag/vimtex', { 'for': 'tex' }
Plug 'vim-scripts/xml.vim', { 'for': ['xml', 'html'] }

" SQL
Plug 'tpope/vim-dadbod'
Plug 'kristijanhusak/vim-dadbod-ui'
Plug 'kristijanhusak/vim-dadbod-completion'

" TERMINAL
Plug 'voldikss/vim-floaterm'

" REST CLIENT
Plug 'diepm/vim-rest-console'

" TEST RUNNER
Plug 'vim-test/vim-test'

" SESSION / UTILITIES
Plug 'tpope/vim-obsession'
Plug 'editorconfig/editorconfig-vim'
Plug 'liuchengxu/vim-which-key'
Plug 'szw/vim-maximizer', { 'on': 'MaximizerToggle' }
Plug 'tpope/vim-eunuch'

call plug#end()

call Augroup('PlugAutoInstall', [
  \ 'VimEnter * ++once'
  \   . ' let s:missing = filter(values(g:plugs), "!isdirectory(v:val.dir)")'
  \   . ' | if len(s:missing)'
  \   . ' | echom len(s:missing) . " plugin(s) missing — run :PlugInstall"'
  \   . ' | endif',
  \ ])

" =============================================================================
" LAYER 4: COLORSCHEME & UI
" =============================================================================

let s:theme = !empty($VIM_THEME) ? $VIM_THEME : 'catppuccin_mocha'

function! s:ApplyTheme() abort
  set background=dark
  try
    execute 'colorscheme ' . s:theme
  catch
    colorscheme desert
    call Log('Theme "' . s:theme . '" not found, using desert', 'warn')
  endtry
endfunction

call Augroup('ThemeInit', [
  \ 'VimEnter * call s:ApplyTheme()',
  \ ])

function! ToggleBackground() abort
  let &background = (&background ==# 'dark') ? 'light' : 'dark'
endfunction

" =============================================================================
" LAYER 5: HELPER PREDICATES
" =============================================================================

function! s:IsSpecialBuffer() abort
  return &buftype !=# '' ||
    \ &filetype =~# '\v^(fern|floaterm|fugitive|dbui|qf|help)$'
endfunction

function! s:SafeBnext() abort
  if s:IsSpecialBuffer() | return | endif
  bnext
endfunction

function! s:SafeBprev() abort
  if s:IsSpecialBuffer() | return | endif
  bprevious
endfunction

" =============================================================================
" LAYER 6: KEYMAPS (core only — feature keymaps live in their module inits)
" =============================================================================

let mapleader      = ' '
let maplocalleader = ','

" Core navigation
call Map('n', '<C-h>', '<C-w>h', {})
call Map('n', '<C-j>', '<C-w>j', {})
call Map('n', '<C-k>', '<C-w>k', {})
call Map('n', '<C-l>', '<C-w>l', {})

nnoremap <silent> <Tab>   :call <SID>SafeBnext()<CR>
nnoremap <silent> <S-Tab> :call <SID>SafeBprev()<CR>

call Map('n', 'j', 'gj', {})
call Map('n', 'k', 'gk', {})
call Map('v', 'j', 'gj', {})
call Map('v', 'k', 'gk', {})

call Map('n', 'n', 'nzzzv', {})
call Map('n', 'N', 'Nzzzv', {})

" Config
call Map('n', '<leader><CR>', ':source $MYVIMRC \| echom "reloaded"<CR>', {})
call Map('n', '<leader>ec',   ':edit $MYVIMRC<CR>', {})

" Save / quit
call Map('n', '<C-s>',     ':write<CR>', {})
call Map('i', '<C-s>',     '<C-o>:write<CR>', {})
call Map('n', '<leader>w', ':write<CR>', {})
call Map('n', '<leader>q', ':quit<CR>', {})
call Map('n', '<leader>Q', ':quit!<CR>', {})
call Map('n', '<leader>qa',':qall<CR>', {})

" Window management
call Map('n', '<leader>wv', ':vsplit<CR>', {})
call Map('n', '<leader>ws', ':split<CR>', {})
call Map('n', '<leader>wc', ':close<CR>', {})
call Map('n', '<leader>wo', ':only<CR>', {})
call Map('n', '<leader>w=', '<C-w>=', {})
call Map('n', '<leader>wz', ':MaximizerToggle<CR>', {})

call Map('n', '<M-Up>',    ':resize +2<CR>', {})
call Map('n', '<M-Down>',  ':resize -2<CR>', {})
call Map('n', '<M-Left>',  ':vertical resize -2<CR>', {})
call Map('n', '<M-Right>', ':vertical resize +2<CR>', {})

" Buffer management
call Map('n', '<leader><Tab>', '<C-^>', {})
call Map('n', '<leader>bd',    ':bdelete<CR>', {})
call Map('n', '<leader>bD',    ':bdelete!<CR>', {})
call Map('n', '<leader>bn',    ':bnext<CR>', {})
call Map('n', '<leader>bp',    ':bprevious<CR>', {})
call Map('n', '<leader>bC',    ':CleanBuffers<CR>', {})

" Search
call Map('n', '<leader>sc', ':nohlsearch<CR>', {})
call Map('n', '<leader>sr', ':%s/\<<C-r><C-w>\>//gc<Left><Left><Left>', { 'silent': 0 })
call Map('v', '<leader>sr', '"hy:%s/<C-r>h//gc<Left><Left><Left>', { 'silent': 0 })

" Quickfix
call Map('n', '<leader>co', ':copen<CR>', {})
call Map('n', '<leader>cc', ':cclose<CR>', {})
call Map('n', '<leader>cn', ':cnext<CR>', {})
call Map('n', '<leader>cp', ':cprevious<CR>', {})

" Location list
call Map('n', '<leader>lo', ':lopen<CR>', {})
call Map('n', '<leader>lc', ':lclose<CR>', {})
call Map('n', '<leader>ln', ':lnext<CR>', {})
call Map('n', '<leader>lp', ':lprevious<CR>', {})

" FZF
call Map('n', '<leader>ff', ':Files<CR>', {})
call Map('n', '<leader>fg', ':GFiles<CR>', {})
call Map('n', '<leader>fb', ':Buffers<CR>', {})
call Map('n', '<leader>fh', ':History<CR>', {})
call Map('n', '<leader>fr', ':Rg<CR>', {})
call Map('n', '<leader>fl', ':Lines<CR>', {})
call Map('n', '<leader>ft', ':Tags<CR>', {})
call Map('n', '<leader>fc', ':Commands<CR>', {})
call Map('n', '<leader>fk', ':Maps<CR>', {})
call Map('n', '<leader>fm', ':Marks<CR>', {})
call Map('n', '<leader>fp', ':Files %:h<CR>', {})

" Diff mode
call Augroup('DiffMappings', [
  \ 'OptionSet diff if v:option_new'
  \   . ' | nnoremap <buffer> <leader>dg :diffget<CR>'
  \   . ' | nnoremap <buffer> <leader>dp :diffput<CR>'
  \   . ' | endif',
  \ ])

" File explorer
call Map('n', '<leader>e', ':Fern . -drawer -reveal=% -toggle<CR>', {})
call Map('n', '<leader>E', ':Fern . -drawer -reveal=%<CR>', {})

" Terminal
call Map('n', '<C-\>', ':FloatermToggle<CR>', {})
call Map('t', '<C-\>', '<C-\><C-n>:FloatermToggle<CR>', {})
call Map('n', '<leader>tn', ':FloatermNew<CR>', {})
call Map('n', '<leader>tk', ':FloatermKill<CR>', {})
call Map('n', '<leader>tl', ':FloatermNext<CR>', {})
call Map('n', '<leader>th', ':FloatermPrev<CR>', {})
call Map('v', '<leader>ts', ':FloatermSend<CR>', {})

" Markdown
call Map('n', '<leader>mp', ':MarkdownPreview<CR>', {})
call Map('n', '<leader>ms', ':MarkdownPreviewStop<CR>', {})

" Toggles
call Map('n', '<leader>un', ':set number!<CR>', {})
call Map('n', '<leader>ur', ':set relativenumber!<CR>', {})
call Map('n', '<leader>uw', ':set wrap!<CR>', {})
call Map('n', '<leader>us', ':set spell!<CR>', {})
call Map('n', '<leader>uh', ':set hlsearch!<CR>', {})
call Map('n', '<leader>ub', ':call ToggleBackground()<CR>', {})
call Map('n', '<leader>uc', ':set cursorline!<CR>', {})
call Map('n', '<leader>ul', ':set list!<CR>', {})
call Map('n', '<leader>ui', ':IndentGuidesToggle<CR>', {})
call Map('n', '<leader>ux', ':ContextToggle<CR>', {})

" Tools
call Map('n', '<F8>',      ':TagbarToggle<CR>', {})
call Map('n', '<leader>U', ':UndotreeToggle<CR>', {})

" Session
call Map('n', '<leader>SS', ':SSave<CR>', {})
call Map('n', '<leader>SL', ':SLoad<CR>', {})
call Map('n', '<leader>Sd', ':SDelete<CR>', {})
call Map('n', '<leader>Sc', ':SClose<CR>', {})
call Map('n', '<leader>st', ':Startify<CR>', {})

" Misc
call Map('n', '<leader>vi', ':VimInfo<CR>', {})
call Map('n', '<leader>PR', ':FindProjectRoot<CR>', {})
call Map('n', '<leader>yp', '"+p', {})
call Map('n', '<leader>yy', '"+yy', {})
call Map('v', '<leader>y',  '"+y', {})

" F5-F9 runners
call Map('n', '<F5>', ':call <SID>RunAction("run")<CR>', {})
call Map('n', '<F6>', ':call <SID>RunAction("compile")<CR>', {})
call Map('n', '<F7>', ':call <SID>RunAction("build")<CR>', {})
call Map('n', '<F9>', ':call <SID>RunAction("test")<CR>', {})

" which-key
nnoremap <silent> <leader> :<c-u>WhichKey '<Space>'<CR>

" =============================================================================
" LAYER 7: PLUGIN CONFIGURATION (non-feature — always active)
" =============================================================================

" --- Lightline ---------------------------------------------------------------

function! LightlineCocStatus() abort
  return get(g:, 'coc_status', '')
endfunction

let g:lightline = {
  \ 'colorscheme': 'catppuccin_mocha',
  \ 'active': {
  \   'left':  [['mode', 'paste'],
  \             ['gitbranch', 'readonly', 'filename', 'modified']],
  \   'right': [['lineinfo'], ['percent'],
  \             ['coc_status', 'filetype', 'fileencoding']]
  \ },
  \ 'tabline': {
  \   'left':  [['buffers']],
  \   'right': [['close']]
  \ },
  \ 'component_expand':   { 'buffers': 'lightline#bufferline#buffers' },
  \ 'component_type':     { 'buffers': 'tabsel' },
  \ 'component_function': {
  \   'gitbranch':  'FugitiveHead',
  \   'coc_status': 'LightlineCocStatus',
  \ },
  \ 'separator':    { 'left': '', 'right': '' },
  \ 'subseparator': { 'left': '', 'right': '' },
  \ }

set showtabline=2
let g:lightline#bufferline#show_number     = 1
let g:lightline#bufferline#unicode_symbols = 1
let g:lightline#bufferline#enable_devicons = 1

" --- context.vim -------------------------------------------------------------

let g:context_max_height       = 5
let g:context_enabled          = 1
let g:context_filetype_exclude = ['fern', 'startify', 'help', 'dbui', 'qf']

" --- better-escape -----------------------------------------------------------

let g:better_escape_shortcut = ['jk', 'kj']
let g:better_escape_interval = 200

" --- Fern --------------------------------------------------------------------

let g:fern#renderer       = 'nerdfont'
let g:fern#default_hidden = 1
let g:fern#default_exclude =
  \ '^\%(\.git\|__pycache__\|node_modules\|\.DS_Store\|\.cache\|dist\|build\)$'

function! s:FernInit() abort
  call Map('n', '<C-f>', '<Plug>(fern-mapping-fzf-select)',    { 'buffer': 1, 'noremap': 0 })
  call Map('n', '<CR>',  '<Plug>(fern-action-open-or-expand)', { 'buffer': 1, 'noremap': 0 })
  call Map('n', 'l',     '<Plug>(fern-action-open-or-expand)', { 'buffer': 1, 'noremap': 0 })
  call Map('n', 'h',     '<Plug>(fern-action-collapse)',       { 'buffer': 1, 'noremap': 0 })
  call Map('n', 'ma',    '<Plug>(fern-action-new-path)',       { 'buffer': 1, 'noremap': 0 })
  call Map('n', 'md',    '<Plug>(fern-action-remove)',         { 'buffer': 1, 'noremap': 0 })
  call Map('n', 'mc',    '<Plug>(fern-action-copy)',           { 'buffer': 1, 'noremap': 0 })
  call Map('n', 'mm',    '<Plug>(fern-action-move)',           { 'buffer': 1, 'noremap': 0 })
  call Map('n', 'mr',    '<Plug>(fern-action-rename)',         { 'buffer': 1, 'noremap': 0 })
  call Map('n', 'R',     '<Plug>(fern-action-reload)',         { 'buffer': 1, 'noremap': 0 })
  call Map('n', 'I',     '<Plug>(fern-action-hidden-toggle)',  { 'buffer': 1, 'noremap': 0 })
  call Map('n', 's',     '<Plug>(fern-action-open:split)',     { 'buffer': 1, 'noremap': 0 })
  call Map('n', 'v',     '<Plug>(fern-action-open:vsplit)',    { 'buffer': 1, 'noremap': 0 })
endfunction

call Augroup('FernEvents', [
  \ 'FileType fern call s:FernInit()',
  \ 'BufEnter * ++nested if winnr("$") == 1 && &filetype ==# "fern" | quit | endif',
  \ ])

" --- FZF ---------------------------------------------------------------------

let g:fzf_layout      = { 'window': { 'width': 0.92, 'height': 0.88, 'rounded': v:true } }
let g:fzf_history_dir = expand('~/.vim/fzf-history')

let $FZF_DEFAULT_OPTS =
  \ '--layout=reverse --border=rounded --info=inline ' .
  \ '--color=bg+:#313244,bg:#1e1e2e,spinner:#f5e0dc,hl:#f38ba8 ' .
  \ '--color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc ' .
  \ '--color=marker:#f5e0dc,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8'

if executable('bat')
  let $FZF_DEFAULT_OPTS .= ' --preview "bat --style=numbers --color=always {}"'
endif

" --- Floaterm ----------------------------------------------------------------

let g:floaterm_width       = 0.88
let g:floaterm_height      = 0.88
let g:floaterm_autoclose   = 1
let g:floaterm_position    = 'center'
let g:floaterm_borderchars = '─│─│╭╮╯╰'
let g:floaterm_title       = '  terminal ($1/$2) '
let g:floaterm_wintype     = 'float'

" --- vim-vsnip ---------------------------------------------------------------

imap <expr> <C-e> vsnip#expandable() ? '<Plug>(vsnip-expand)'    : '<C-e>'
smap <expr> <C-e> vsnip#expandable() ? '<Plug>(vsnip-expand)'    : '<C-e>'
imap <expr> <C-l> vsnip#jumpable(1)  ? '<Plug>(vsnip-jump-next)' : '<C-l>'
smap <expr> <C-l> vsnip#jumpable(1)  ? '<Plug>(vsnip-jump-next)' : '<C-l>'
imap <expr> <C-b> vsnip#jumpable(-1) ? '<Plug>(vsnip-jump-prev)' : '<C-b>'
smap <expr> <C-b> vsnip#jumpable(-1) ? '<Plug>(vsnip-jump-prev)' : '<C-b>'

" --- vim-move ----------------------------------------------------------------

let g:move_key_modifier = 'A'

" --- vim-matchup -------------------------------------------------------------

let g:matchup_matchparen_offscreen          = { 'method': 'status_manual' }
let g:matchup_matchparen_deferred           = 1
let g:matchup_matchparen_hi_surround_always = 1
let g:matchup_motion_enabled                = 1
let g:matchup_text_obj_enabled              = 1

" --- vim-indent-guides -------------------------------------------------------

let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_start_level           = 2
let g:indent_guides_guide_size            = 1
let g:indent_guides_auto_colors           = 0
let g:indent_guides_exclude_filetypes     = ['startify', 'help', 'fern', 'dbui', 'json', 'terminal']

call Augroup('IndentGuideColors', [
  \ 'VimEnter,Colorscheme *'
  \   . ' highlight IndentGuidesOdd  ctermbg=235 guibg=#2a2a37'
  \   . ' | highlight IndentGuidesEven ctermbg=236 guibg=#313244',
  \ ])

" --- Gutentags ---------------------------------------------------------------

let g:gutentags_cache_dir           = expand('~/.vim/tags')
let g:gutentags_generate_on_new     = 1
let g:gutentags_generate_on_missing = 1
let g:gutentags_generate_on_write   = 1
let g:gutentags_ctags_extra_args    = [
  \ '--tag-relative=yes', '--fields=+ailmnS',
  \ '--exclude=.git', '--exclude=.hg', '--exclude=.svn',
  \ '--exclude=node_modules', '--exclude=bower_components',
  \ '--exclude=dist', '--exclude=build', '--exclude=out',
  \ '--exclude=target', '--exclude=__pycache__', '--exclude=.cache',
  \ '--exclude=*.min.js', '--exclude=*.min.css',
  \ ]
let g:gutentags_enabled = 0

call Augroup('GutentagsSelectiveEnable', [
  \ 'FileType sh,vim,fortran,cobol,vhdl,verilog,make,cmake,zig,d,nim,crystal,lua,perl,r,julia'
  \   . ' let g:gutentags_enabled = 1',
  \ ])

if executable('rg')
  let g:gutentags_file_list_command = 'rg --files --follow'
endif
let g:gutentags_add_default_project_roots = 0
let g:gutentags_project_root =
  \ ['.git', '.svn', '.hg', 'package.json', 'Cargo.toml', 'go.mod', 'pyproject.toml']

" --- vim-easy-align ----------------------------------------------------------

xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" --- vim-go ------------------------------------------------------------------

let g:go_fmt_command            = 'goimports'
let g:go_highlight_types        = 1
let g:go_highlight_fields       = 1
let g:go_highlight_functions    = 1
let g:go_highlight_operators    = 1
let g:go_def_mapping_enabled    = 0
let g:go_doc_keywordprg_enabled = 0

" --- rust.vim ----------------------------------------------------------------

let g:rustfmt_autosave = 0

" --- vim-markdown ------------------------------------------------------------

let g:vim_markdown_folding_disabled = 1
let g:vim_markdown_conceal          = 0
let g:vim_markdown_frontmatter      = 1
let g:vim_markdown_fenced_languages = [
  \ 'python', 'javascript', 'typescript', 'go', 'rust', 'bash=sh',
  \ 'sh', 'vim', 'json', 'yaml', 'html', 'css', 'c', 'cpp', 'zig',
  \ 'sql', 'kotlin', 'ruby', 'lua', 'fortran', 'cobol'
  \ ]

" --- MarkdownPreview ---------------------------------------------------------

let g:mkdp_auto_close = 1
let g:mkdp_theme      = 'dark'
let g:mkdp_browser    = ''

" --- Startify ----------------------------------------------------------------

let g:startify_session_dir         = expand('~/.vim/sessions')
let g:startify_session_autoload    = 1
let g:startify_session_persistence = 1
let g:startify_change_to_vcs_root  = 1
let g:startify_fortune_use_unicode = 1
let g:startify_custom_header = startify#pad([
  \ '  ██╗   ██╗██╗███╗   ███╗',
  \ '  ██║   ██║██║████╗ ████║',
  \ '  ██║   ██║██║██╔████╔██║',
  \ '  ╚██╗ ██╔╝██║██║╚██╔╝██║',
  \ '   ╚████╔╝ ██║██║ ╚═╝ ██║',
  \ '    ╚═══╝  ╚═╝╚═╝     ╚═╝',
  \ '           your ide. your rules.',
  \ ])

" --- which-key ---------------------------------------------------------------

call which_key#register('<Space>', "g:which_key_map")
let g:which_key_map = {
  \ '<CR>': 'reload config',
  \ '<Tab>': 'last buffer',
  \ 'e': 'file explorer',
  \ 'E': 'reveal in tree',
  \ 'U': 'undo tree',
  \ 'w': { 'name': '+window' },
  \ 'b': { 'name': '+buffer' },
  \ 'f': { 'name': '+find/fzf' },
  \ 'g': { 'name': '+git' },
  \ 'h': { 'name': '+hunk' },
  \ 't': { 'name': '+terminal/test' },
  \ 'u': { 'name': '+toggle' },
  \ 's': { 'name': '+search' },
  \ 'y': { 'name': '+yank' },
  \ 'c': { 'name': '+lsp/coc' },
  \ 'l': { 'name': '+loclist' },
  \ 'r': { 'name': '+rename/rest' },
  \ 'd': { 'name': '+debug/diff' },
  \ 'D': { 'name': '+database' },
  \ 'S': { 'name': '+session' },
  \ 'm': { 'name': '+multicursor' },
  \ 'p': { 'name': '+markdown' },
  \ 'a': 'align (easy-align ga+motion)',
  \ }

" =============================================================================
" LAYER 8: MODULE DEFINITIONS
"
" Each module owns: config vars, autocmds, keymaps, and optionally teardown.
" Teardown note: Vim provides no native unmap API for non-buffer mappings.
" Teardown cleans g: vars and augroups. Normal-mode maps persist for the
" session — acceptable under the immutable session contract.
" =============================================================================

" --- Git ---------------------------------------------------------------------

function! s:git_init() abort
  let g:fern_git_status#disable_ignored    = 1
  let g:fern_git_status#disable_untracked  = 0
  let g:fern_git_status#disable_submodules = 1

  let g:gitgutter_enabled       = 1
  let g:gitgutter_map_keys      = 0
  let g:gitgutter_sign_added    = '▎'
  let g:gitgutter_sign_modified = '▎'
  let g:gitgutter_sign_removed  = '▎'

  call Map('n', '<leader>gg', ':Git<CR>', {})
  call Map('n', '<leader>gc', ':Git commit<CR>', {})
  call Map('n', '<leader>gp', ':Git push<CR>', {})
  call Map('n', '<leader>gl', ':GV<CR>', {})
  call Map('n', '<leader>gL', ':GV!<CR>', {})
  call Map('n', '<leader>gd', ':Gdiffsplit<CR>', {})
  call Map('n', '<leader>gb', ':Git blame<CR>', {})
  call Map('n', '<leader>gB', ':GBrowse<CR>', {})
  call Map('n', '<leader>gf', ':Git fetch<CR>', {})
  call Map('n', '<leader>gm', ':Git merge<CR>', {})
  call Map('n', '<leader>gR', ':Git rebase<CR>', {})
  call Map('n', '<leader>gA', ':Git add %<CR>', {})
  call Map('n', '<leader>gS', ':Git stash<CR>', {})
  call Map('n', '<leader>gP', ':Git stash pop<CR>', {})

  call Map('n', '<leader>hs', ':GitGutterStageHunk<CR>', {})
  call Map('n', '<leader>hu', ':GitGutterUndoHunk<CR>', {})
  call Map('n', '<leader>hp', ':GitGutterPreviewHunk<CR>', {})
  nmap ]h <Plug>(GitGutterNextHunk)
  nmap [h <Plug>(GitGutterPrevHunk)
endfunction

function! s:git_teardown() abort
  " Disables display. Maps persist for session (immutable contract).
  let g:gitgutter_enabled = 0
  silent! GitGutterDisable
endfunction

call s:RegisterModule('git', {
  \ 'init':     function('s:git_init'),
  \ 'teardown': function('s:git_teardown'),
  \ 'lazy':     '',
  \ })

" --- LSP (coc.nvim) ----------------------------------------------------------
" Always eager (lazy: ''): coc config vars must precede first BufReadPost.
" Teardown: CocRestart is the only clean reset coc supports.

function! s:lsp_init() abort
  let g:coc_disable_startup_warning = 1
  let g:coc_global_extensions = [
    \ 'coc-json',
    \ 'coc-yaml',
    \ 'coc-pyright',
    \ 'coc-clangd',
    \ 'coc-rust-analyzer',
    \ 'coc-go',
    \ 'coc-tsserver',
    \ 'coc-eslint',
    \ ]
  let g:coc_auto_copen          = 0
  let g:coc_enable_locationlist = 0

  inoremap <silent><expr> <TAB>
    \ coc#pum#visible() ? coc#pum#next(1) :
    \ col('.') > 1 && getline('.')[col('.')-2] !~# '\s' ? coc#refresh() : "\<Tab>"
  inoremap <expr><S-TAB> coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"
  inoremap <silent><expr> <CR>
    \ coc#pum#visible() ? coc#pum#confirm() : "\<C-g>u\<CR>\<C-r>=coc#on_enter()\<CR>"
  inoremap <silent><expr> <C-Space> coc#refresh()
  inoremap <silent> <C-k> <C-r>=CocActionAsync('showSignatureHelp')<CR>

  nmap <silent> gd <Plug>(coc-definition)
  nmap <silent> gD <Plug>(coc-declaration)
  nmap <silent> gy <Plug>(coc-type-definition)
  nmap <silent> gi <Plug>(coc-implementation)
  nmap <silent> gr <Plug>(coc-references)
  nnoremap <silent> K :call CocActionAsync('doHover')<CR>

  nmap <silent> <leader>rn <Plug>(coc-rename)
  nmap <silent> <leader>ca <Plug>(coc-codeaction-cursor)
  nmap <silent> <leader>cf <Plug>(coc-format)
  nmap <silent> <leader>cs :CocList outline<CR>
  nmap <silent> <leader>cS :CocList -I symbols<CR>

  nmap <silent> ]g <Plug>(coc-diagnostic-next)
  nmap <silent> [g <Plug>(coc-diagnostic-prev)
  nmap <silent> ]e <Plug>(coc-diagnostic-next-error)
  nmap <silent> [e <Plug>(coc-diagnostic-prev-error)
  nnoremap <silent> <leader>cd :CocList diagnostics<CR>

  call Augroup('LightlineCoc', [
    \ 'User CocStatusChange,CocDiagnosticChange silent call lightline#update()',
    \ ])
endfunction

function! s:lsp_teardown() abort
  call Augroup('LightlineCoc', [])
  if exists(':CocRestart') | silent! CocRestart | endif
endfunction

call s:RegisterModule('lsp', {
  \ 'init':     function('s:lsp_init'),
  \ 'teardown': function('s:lsp_teardown'),
  \ 'lazy':     '',
  \ })

" --- Debug (vimspector) ------------------------------------------------------
" lazy='cmd': deferred until any CmdUndefined fires (e.g. :VimspectorLaunch).
" Teardown: resets vimspector UI and removes the lazy trigger augroup.

function! s:debug_init() abort
  call plug#load('vimspector')

  call Map('n', '<F1>',       ':call vimspector#Continue()<CR>', {})
  call Map('n', '<F2>',       ':call vimspector#StepOver()<CR>', {})
  call Map('n', '<F3>',       ':call vimspector#StepInto()<CR>', {})
  call Map('n', '<F4>',       ':call vimspector#StepOut()<CR>', {})
  call Map('n', '<F10>',      ':call vimspector#ToggleBreakpoint()<CR>', {})
  call Map('n', '<F11>',      ':call vimspector#ToggleConditionalBreakpoint()<CR>', {})
  call Map('n', '<F12>',      ':call vimspector#RunToCursor()<CR>', {})
  call Map('n', '<leader>dx', ':call vimspector#Reset()<CR>', {})
  call Map('n', '<leader>dX', ':call vimspector#ClearBreakpoints()<CR>', {})
  call Map('n', '<leader>di', ':call vimspector#BalloonEval()<CR>', {})
  call Map('n', '<leader>dw', ':call vimspector#AddWatch()<CR>', {})
endfunction

function! s:debug_teardown() abort
  if exists('*vimspector#Reset') | silent! call vimspector#Reset() | endif
  call Augroup('LazyLoad_debug', [])
endfunction

call s:RegisterModule('debug', {
  \ 'init':     function('s:debug_init'),
  \ 'teardown': function('s:debug_teardown'),
  \ 'lazy':     'cmd',
  \ })

" --- Test (vim-test) ---------------------------------------------------------
" lazy='buf': deferred to first BufReadPost when g:vimide_lazy_aggressive=1.

function! s:test_init() abort
  let g:test#strategy          = 'floaterm'
  let g:test#python#runner     = 'pytest'
  let g:test#javascript#runner = 'jest'
  let g:test#go#runner         = 'gotest'
  let g:test#rust#runner       = 'cargotest'

  call Map('n', '<leader>tt', ':TestNearest<CR>', {})
  call Map('n', '<leader>tT', ':TestFile<CR>', {})
  call Map('n', '<leader>ta', ':TestSuite<CR>', {})
  call Map('n', '<leader>tR', ':TestLast<CR>', {})
  call Map('n', '<leader>tv', ':TestVisit<CR>', {})
endfunction

call s:RegisterModule('test', {
  \ 'init': function('s:test_init'),
  \ 'lazy': 'buf',
  \ })

" --- Database (vim-dadbod) ---------------------------------------------------
" lazy='buf': deferred to first BufReadPost when g:vimide_lazy_aggressive=1.

function! s:db_init() abort
  let g:db_ui_use_nerd_fonts = 1
  let g:db_ui_winwidth       = 40
  let g:db_ui_save_location  = expand('~/.vim/db_ui')
  let g:dbs = {}
  if !empty($DATABASE_DEV_URL)     | let g:dbs['dev']     = $DATABASE_DEV_URL     | endif
  if !empty($DATABASE_STAGING_URL) | let g:dbs['staging'] = $DATABASE_STAGING_URL | endif
  if !empty($DATABASE_LOCAL_URL)   | let g:dbs['local']   = $DATABASE_LOCAL_URL   | endif

  call Map('n', '<leader>Du', ':DBUIToggle<CR>', {})
  call Map('n', '<leader>Df', ':DBUIFindBuffer<CR>', {})

  call Augroup('DatabaseSQL', [
    \ 'FileType sql setlocal omnifunc=vim_dadbod_completion#omni',
    \ ])
endfunction

function! s:db_teardown() abort
  call Augroup('DatabaseSQL', [])
endfunction

call s:RegisterModule('db', {
  \ 'init':     function('s:db_init'),
  \ 'teardown': function('s:db_teardown'),
  \ 'lazy':     'buf',
  \ })

" --- REST client (vim-rest-console) ------------------------------------------
" lazy='buf': deferred to first BufReadPost when g:vimide_lazy_aggressive=1.

function! s:rest_init() abort
  let g:vrc_curl_opts = {
    \ '--include':    '',
    \ '--location':   '',
    \ '--show-error': '',
    \ '--silent':     '',
    \ }
  let g:vrc_auto_format_response_patterns = {
    \ 'json': 'python3 -m json.tool',
    \ 'xml':  'xmllint --format -',
    \ }
  let g:vrc_output_buffer_name = '__VRC_OUTPUT__'

  call Map('n', '<leader>rr', ':call VrcQuery()<CR>', {})
endfunction

call s:RegisterModule('rest', {
  \ 'init': function('s:rest_init'),
  \ 'lazy': 'buf',
  \ })

" --- Dispatch all modules ----------------------------------------------------

call s:LoadModules()

" =============================================================================
" LAYER 9: LANGUAGE RUNNERS
" =============================================================================

call RegisterRunner('python',      { 'run': 'python3 {fp}',                   'test': 'python3 -m pytest {dn}' })
call RegisterRunner('javascript',  { 'run': 'node {fp}',                       'test': 'npm test' })
call RegisterRunner('typescript',  { 'run': 'ts-node {fp}',                    'test': 'npm test', 'compile': 'tsc {fp}' })
call RegisterRunner('go',          { 'run': 'go run {fp}',                     'test': 'go test ./...', 'build': 'go build -o {bn} {fp}' })
call RegisterRunner('rust',        { 'run': './{bn}',                          'test': 'cargo test', 'build': 'cargo build --release', 'compile': 'rustc {fp}' })
call RegisterRunner('c',           { 'run': './{bn}',                          'compile': 'gcc -Wall -O2 -g {fp} -o {bn} -lm' })
call RegisterRunner('cpp',         { 'run': './{bn}',                          'compile': 'g++ -Wall -O2 -g -std=c++17 {fp} -o {bn} -lm' })
call RegisterRunner('java',        { 'compile': 'javac {fp}',                  'run': 'java -cp {dn} {bn}' })
call RegisterRunner('zig',         { 'run': 'zig run {fp}',                    'build': 'zig build', 'test': 'zig test {fp}' })
call RegisterRunner('kotlin',      { 'compile': 'kotlinc {fp} -include-runtime -d {bn}.jar', 'run': 'java -jar {bn}.jar' })
call RegisterRunner('sh',          { 'run': 'bash {fp}' })
call RegisterRunner('lua',         { 'run': 'lua {fp}' })
call RegisterRunner('ruby',        { 'run': 'ruby {fp}',                       'test': 'ruby -Itest {fp}' })
call RegisterRunner('perl',        { 'run': 'perl {fp}' })
call RegisterRunner('php',         { 'run': 'php {fp}' })
call RegisterRunner('elixir',      { 'run': 'elixir {fp}',                     'test': 'mix test' })
call RegisterRunner('haskell',     { 'run': 'runghc {fp}',                     'compile': 'ghc -O2 {fp} -o {bn}' })
call RegisterRunner('fortran',     { 'compile': 'gfortran -O2 {fp} -o {bn}',   'run': './{bn}' })
call RegisterRunner('cobol',       { 'compile': 'cobc -x {fp} -o {bn}',        'run': './{bn}' })
call RegisterRunner('julia',       { 'run': 'julia {fp}' })
call RegisterRunner('r',           { 'run': 'Rscript {fp}' })
call RegisterRunner('nim',         { 'run': 'nim c -r {fp}',                   'build': 'nim c -d:release {fp}' })
call RegisterRunner('crystal',     { 'run': 'crystal run {fp}',                'build': 'crystal build --release {fp}' })
call RegisterRunner('d',           { 'compile': 'dmd {fp} -of={bn}',           'run': './{bn}' })

function! s:RunAction(action) abort
  if &modified | write | endif
  let l:ft = &filetype
  let l:fp = expand('%:p')
  if !filereadable(l:fp)
    call Log('File not readable: ' . l:fp, 'error') | return
  endif
  let l:cmd = get(get(s:runners, l:ft, {}), a:action, '')
  if empty(l:cmd)
    call Log('No ' . a:action . ' for filetype: ' . l:ft, 'warn') | return
  endif
  let l:subs = {
    \ 'fp': shellescape(l:fp),
    \ 'dn': shellescape(fnamemodify(l:fp, ':h')),
    \ 'fn': shellescape(expand('%:t')),
    \ 'bn': shellescape(expand('%:t:r')),
    \ }
  let l:cmd = substitute(l:cmd, '{\(\w\+\)}', '\=get(l:subs, submatch(1), submatch(0))', 'g')
  execute 'FloatermNew --autoclose=0 ' . l:cmd
endfunction

" =============================================================================
" LAYER 10: EVENT HANDLERS
" =============================================================================

function! s:HandleLargeFile() abort
  let l:size = getfsize(expand('<afile>:p'))
  if l:size > 10485760
    setlocal eventignore+=FileType bufhidden=unload undolevels=-1
    setlocal noundofile noswapfile syntax=off nowrap nocursorline norelativenumber
    call Log('Large file: performance mode active', 'info')
    return
  endif
  if l:size > 500000
    let b:context_enabled            = 0
    let b:matchup_matchparen_enabled = 0
  endif
endfunction

function! s:StripTrailing() abort
  if &modifiable && &filetype !~# '\v^(markdown|diff|cobol)$'
    let l:view = winsaveview()
    silent! %s/\s\+$//e
    call winrestview(l:view)
  endif
endfunction

function! s:MkdirOnSave() abort
  let l:dir = expand('%:p:h')
  if !isdirectory(l:dir) | call mkdir(l:dir, 'p') | endif
endfunction

function! s:RestoreCursor() abort
  if line("'\"") > 0 && line("'\"") <= line("$") && &filetype !~# 'commit'
    execute "normal! g`\""
  endif
endfunction

function! s:FlashYank() abort
  if !exists('*matchaddpos') | return | endif
  if empty(v:event.regcontents) | return | endif
  let l:vstart = getpos("'[")
  let l:vend   = getpos("']")
  if l:vstart[1] <= 0 || l:vend[1] <= 0 | return | endif
  if (l:vend[1] - l:vstart[1]) > 50 | return | endif
  " For linewise yanks, col is v:maxcol — use line-only pattern instead
  if v:event.regtype ==# 'V' || l:vend[2] >= 2147483647
    let l:pat = '\%>' . (l:vstart[1] - 1) . 'l\%<' . (l:vend[1] + 1) . 'l'
  else
    let l:pat = '\%' . l:vstart[1] . 'l\%' . l:vstart[2] . 'c\_.*\%'
      \ . l:vend[1] . 'l\%' . l:vend[2] . 'c'
  endif
  call timer_start(0, {-> s:DoFlash(l:pat)})
endfunction


function! s:DoFlash(pat) abort
  let l:id = matchadd('IncSearch', a:pat)
  if l:id < 0 | return | endif
  call timer_start(250, {-> execute('silent! call matchdelete(' . l:id . ')', '')})
endfunction

call Augroup('VimrcEvents', [
  \ 'BufReadPre           * call s:HandleLargeFile()',
  \ 'BufWritePre          * call s:StripTrailing()',
  \ 'BufWritePre          * call s:MkdirOnSave()',
  \ 'BufReadPost          * call s:RestoreCursor()',
  \ 'TextYankPost         * silent! call s:FlashYank()',
  \ 'FocusGained,BufEnter * silent! checktime',
  \ 'VimResized           * wincmd =',
  \ 'TerminalOpen         * setlocal nonumber norelativenumber signcolumn=no',
  \ ])

" =============================================================================
" LAYER 11: FILETYPE INDENTATION
" =============================================================================

call Augroup('FileTypeIndent', [
  \ 'FileType python                           setlocal ts=4 sw=4 expandtab',
  \ 'FileType javascript,typescript            setlocal ts=2 sw=2 expandtab',
  \ 'FileType javascriptreact,typescriptreact  setlocal ts=2 sw=2 expandtab',
  \ 'FileType html,css,scss,sass,less          setlocal ts=2 sw=2 expandtab',
  \ 'FileType json,jsonc                       setlocal ts=2 sw=2 expandtab foldmethod=syntax',
  \ 'FileType graphql                          setlocal ts=2 sw=2 expandtab',
  \ 'FileType go,make                          setlocal ts=4 sw=4 noexpandtab',
  \ 'FileType c,cpp                            setlocal ts=4 sw=4 expandtab',
  \ 'FileType rust                             setlocal ts=4 sw=4 expandtab',
  \ 'FileType zig                              setlocal ts=4 sw=4 expandtab',
  \ 'FileType java,kotlin                      setlocal ts=4 sw=4 expandtab',
  \ 'FileType lua,vim,ruby                     setlocal ts=2 sw=2 expandtab',
  \ 'FileType sh,zsh,bash,fish                 setlocal ts=2 sw=2 expandtab',
  \ 'FileType perl,php                         setlocal ts=4 sw=4 expandtab',
  \ 'FileType elixir,erlang                    setlocal ts=2 sw=2 expandtab',
  \ 'FileType haskell,ocaml,elm                setlocal ts=2 sw=2 expandtab',
  \ 'FileType scala,clojure,lisp               setlocal ts=2 sw=2 expandtab',
  \ 'FileType fsharp,csharp                    setlocal ts=4 sw=4 expandtab',
  \ 'FileType yaml,toml                        setlocal ts=2 sw=2 expandtab',
  \ 'FileType xml,xhtml                        setlocal ts=2 sw=2 expandtab',
  \ 'FileType sql                              setlocal ts=2 sw=2 expandtab',
  \ 'FileType csv                              setlocal ts=4 sw=4 noexpandtab',
  \ 'FileType terraform                        setlocal ts=2 sw=2 expandtab',
  \ 'FileType dockerfile                       setlocal ts=2 sw=2 expandtab',
  \ 'FileType fortran                          setlocal ts=3 sw=3 expandtab',
  \ 'FileType cobol                            setlocal ts=4 sw=4 noexpandtab',
  \ 'FileType vhdl,verilog                     setlocal ts=2 sw=2 expandtab',
  \ 'FileType markdown,text'
  \   . ' setlocal ts=4 sw=4 expandtab spell textwidth=80 wrap linebreak foldmethod=expr foldexpr=0',
  \ 'FileType gitcommit                        setlocal spell textwidth=72',
  \ 'FileType tex                              setlocal ts=2 sw=2 expandtab spell',
  \ 'FileType rst                              setlocal ts=3 sw=3 expandtab spell',
  \ 'FileType swift                            setlocal ts=4 sw=4 expandtab',
  \ 'FileType dart                             setlocal ts=2 sw=2 expandtab',
  \ 'FileType r,rmd                            setlocal ts=2 sw=2 expandtab',
  \ 'FileType julia                            setlocal ts=4 sw=4 expandtab',
  \ 'FileType nim                              setlocal ts=2 sw=2 expandtab',
  \ 'FileType crystal                          setlocal ts=2 sw=2 expandtab',
  \ 'FileType d                                setlocal ts=4 sw=4 expandtab',
  \ ])

" =============================================================================
" LAYER 12: COMMANDS & UTILITIES
" =============================================================================

function! s:InitProjectRoot() abort
  let l:markers = ['.git', '.svn', '.hg', 'package.json', 'Cargo.toml',
    \ 'go.mod', 'Makefile', 'pyproject.toml', 'build.gradle', 'build.zig']
  let l:dir = expand('%:p:h')
  while l:dir !=# '/'
    for l:m in l:markers
      if isdirectory(l:dir . '/' . l:m) || filereadable(l:dir . '/' . l:m)
        call SetState('project_root', l:dir)
        execute 'cd ' . fnameescape(l:dir)
        return
      endif
    endfor
    let l:dir = fnamemodify(l:dir, ':h')
  endwhile
  call Log('Project root not found', 'warn')
endfunction

call Augroup('ProjectRoot', [
  \ 'VimEnter   * call s:InitProjectRoot()',
  \ 'DirChanged * call s:InitProjectRoot()',
  \ ])

function! s:LspRestart() abort
  if exists(':CocRestart') | CocRestart | endif
endfunction

call RegisterTask('project:root', { 'run': function('s:InitProjectRoot') })
call RegisterTask('lsp:restart',  { 'run': function('s:LspRestart') })
call RegisterTask('project:init', { 'deps': ['project:root'], 'run': function('s:InitProjectRoot') })

function! s:VimInfo() abort
  echo '========== Vim IDE Info =========='
  echo 'Version     : Vim ' . v:version/100 . '.' . v:version%100
  echo 'Config      : ' . $MYVIMRC
  echo 'Filetype    : ' . &filetype
  echo 'Theme       : ' . (exists('g:colors_name') ? g:colors_name : 'none') . ' (' . &background . ')'
  echo '$VIM_THEME  : ' . (!empty($VIM_THEME) ? $VIM_THEME : '(not set → catppuccin_mocha)')
  echo 'coc.nvim    : ' . (exists('g:did_coc_loaded') ? 'loaded' : 'NOT loaded')
  echo 'Diagnostics : ' . g:vimide_diagnostics
  echo 'smoothscroll: ' . (has('patch-9.0.0640') ? 'native' : 'unavailable')
  echo 'Lazy mode   : ' . (g:vimide_lazy_aggressive ? 'on' : 'off')
  echo 'Project root: ' . State('project_root')
  echo 'Last task   : ' . get(State('last_task'), 'name', '(none)')
  echo '--- modules ---'
  for [l:name, l:m] in items(s:modules)
    let l:status = !l:m.enabled        ? 'off'
      \          : l:m.loaded          ? 'loaded'
      \          : l:m.lazy !=# ''     ? 'lazy/' . l:m.lazy
      \          :                       'enabled/pending'
    echo printf('  %-10s: %s', l:name, l:status)
  endfor
  echo '--- tools (✓ = found in PATH) ---'
  for l:t in ['node', 'python3', 'git', 'rg', 'bat', 'gopls', 'tmux',
    \          'rustc', 'cargo', 'go', 'tsc', 'java', 'lua', 'ruby', 'elixir']
    echo printf('  %-12s: %s', l:t, executable(l:t) ? '✓' : '✗ not found')
  endfor
  echo '=================================='
endfunction

function! s:CleanBuffers() abort
  let l:cur     = bufnr('%')
  let l:closed  = 0
  let l:skipped = 0
  for i in range(1, bufnr('$'))
    if !buflisted(i) || i == l:cur | continue | endif
    if getbufvar(i, '&modified') || getbufvar(i, '&buftype') !=# ''
      let l:skipped += 1 | continue
    endif
    silent! execute 'bdelete ' . i
    let l:closed += 1
  endfor
  call Log(printf('Closed %d buffer(s), skipped %d (modified or special)', l:closed, l:skipped), 'info')
endfunction

function! s:RenameFile(new) abort
  let l:old = expand('%:p')
  if l:old ==# fnamemodify(a:new, ':p')
    call Log('Source and destination are the same', 'warn') | return
  endif
  let l:result = system('mv ' . shellescape(l:old) . ' ' . shellescape(a:new))
  if v:shell_error != 0
    call Log('Rename failed: ' . l:result, 'error') | return
  endif
  execute 'edit ' . fnameescape(a:new)
  execute 'bdelete ' . fnameescape(l:old)
  call Log('Renamed to ' . a:new, 'info')
endfunction

function! s:CheckCocSettings() abort
  let l:path = expand('~/.vim/coc-settings.json')
  if !filereadable(l:path)
    call Log('coc-settings.json not found at ' . l:path, 'warn') | return
  endif
  let l:required = [
    \ 'diagnostic.errorSign', 'diagnostic.warningSign',
    \ 'diagnostic.infoSign',  'diagnostic.hintSign',
    \ 'diagnostic.virtualText', 'signature.enable', 'hover.autoHide',
    \ 'snippets.enable', 'coc.preferences.formatOnSaveFiletypes'
    \ ]
  let l:content = join(readfile(l:path), "\n")
  let l:missing = filter(copy(l:required), 'l:content !~# v:val')
  if empty(l:missing)
    call Log('coc-settings.json looks complete', 'info')
  else
    call Log('coc-settings.json missing keys: ' . join(l:missing, ', '), 'warn')
  endif
endfunction

command! VimInfo            call s:VimInfo()
command! CleanBuffers       call s:CleanBuffers()
command! FindProjectRoot    call s:InitProjectRoot()
command! ReloadConfig       source $MYVIMRC | call Log('Config reloaded', 'info')
command! CheckCocSettings   call s:CheckCocSettings()
command! -nargs=1 -complete=file Rename         call s:RenameFile(<q-args>)
command! -nargs=1                ReloadModule   call s:ReloadModule(<q-args>)
command! -nargs=1                TeardownModule call s:TeardownModule(<q-args>)

" =============================================================================
" LAYER 13: TMUX INTEGRATION
" =============================================================================

if exists('$TMUX')
  function! s:TmuxRenameAsync() abort
    if !buflisted(bufnr('%')) | return | endif
    if s:IsSpecialBuffer() | return | endif
    let l:name = expand('%:t')
    if empty(l:name) | return | endif
    call job_start(['tmux', 'rename-window', l:name])
  endfunction

  call Augroup('TmuxRename', [
    \ 'BufEnter * call s:TmuxRenameAsync()',
    \ 'VimLeave * call job_start(["tmux", "set-window-option", "automatic-rename", "on"])',
    \ ])
endif

" =============================================================================
" LAYER 14: MACHINE-LOCAL OVERRIDES
" =============================================================================

if filereadable(expand('~/.vimrc.local'))
  source ~/.vimrc.local
endif

call s:Mark('vimrc done')

if g:vimide_debug
  call Log('Vim IDE loaded — :VimInfo for details', 'info')
endif
