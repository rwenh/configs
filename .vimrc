" =============================================================================
" VIM IDE
" =============================================================================

set nocompatible
if has('vim_starting')
  set encoding=utf-8
endif
scriptencoding utf-8

" =============================================================================
" LAYER 0: STARTUP PROFILING & FEATURE FLAGS
" =============================================================================

let g:vimide_debug           = 0
let g:vimide_minimal         = 0
let g:vimide_diagnostics     = 'coc'
let g:vimide_lazy_aggressive = 1

" =============================================================================
" LAYER 1: CORE ABSTRACTION APIs
" =============================================================================

let s:runners = {}

function! RegisterRunner(ft, actions) abort
  let s:runners[a:ft] = a:actions
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

" Sensitive files: no swap/backup/undo
augroup SensitiveFiles
  autocmd!
  autocmd BufNewFile,BufRead .env,.env.*,*.env,*.pem,*.key,*.p12,id_rsa,id_ed25519,*.gpg
    \ setlocal noswapfile nobackup noundofile
augroup END

" Format options
augroup FormatOptions
  autocmd!
  autocmd FileType * setlocal formatoptions-=cro
  autocmd FileType gitcommit setlocal formatoptions+=cro
augroup END

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
    echohl ErrorMsg | echom 'vim-plug bootstrap failed: ' . v:exception | echohl None
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

augroup PlugAutoInstall
  autocmd!
  autocmd VimEnter * ++once
    \ let s:missing = filter(values(g:plugs), '!isdirectory(v:val.dir)') |
    \ if len(s:missing) |
    \   echom len(s:missing) . ' plugin(s) missing — run :PlugInstall' |
    \ endif
augroup END

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
    echom 'Theme "' . s:theme . '" not found'
  endtry
endfunction

augroup ThemeInit
  autocmd!
  autocmd VimEnter * call s:ApplyTheme()
augroup END

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
" LAYER 6: KEYMAPS
" =============================================================================

let mapleader      = ' '
let maplocalleader = ','

" Core navigation
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

nnoremap <silent> <Tab>   :call <SID>SafeBnext()<CR>
nnoremap <silent> <S-Tab> :call <SID>SafeBprev()<CR>

nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk

nnoremap n nzzzv
nnoremap N Nzzzv

" Config
nnoremap <leader><CR> :source $MYVIMRC \| echom 'reloaded'<CR>
nnoremap <leader>ec   :edit $MYVIMRC<CR>

" Save / quit
nnoremap <C-s>      :write<CR>
inoremap <C-s>      <C-o>:write<CR>
nnoremap <leader>w  :write<CR>
nnoremap <leader>q  :quit<CR>
nnoremap <leader>Q  :quit!<CR>
nnoremap <leader>qa :qall<CR>

" Window management
nnoremap <leader>wv :vsplit<CR>
nnoremap <leader>ws :split<CR>
nnoremap <leader>wc :close<CR>
nnoremap <leader>wo :only<CR>
nnoremap <leader>w= <C-w>=
nnoremap <leader>wz :MaximizerToggle<CR>

nnoremap <M-Up>    :resize +2<CR>
nnoremap <M-Down>  :resize -2<CR>
nnoremap <M-Left>  :vertical resize -2<CR>
nnoremap <M-Right> :vertical resize +2<CR>

" Buffer management
nnoremap <leader><Tab> <C-^>
nnoremap <leader>bd    :bdelete<CR>
nnoremap <leader>bD    :bdelete!<CR>
nnoremap <leader>bn    :bnext<CR>
nnoremap <leader>bp    :bprevious<CR>
nnoremap <leader>bC    :CleanBuffers<CR>

" Search
nnoremap <leader>sc :nohlsearch<CR>
nnoremap <leader>sr :%s/\<<C-r><C-w>\>//gc<Left><Left><Left>
vnoremap <leader>sr "hy:%s/<C-r>h//gc<Left><Left><Left>

" Quickfix
nnoremap <leader>co :copen<CR>
nnoremap <leader>cc :cclose<CR>
nnoremap <leader>cn :cnext<CR>
nnoremap <leader>cp :cprevious<CR>

" Location list
nnoremap <leader>lo :lopen<CR>
nnoremap <leader>lc :lclose<CR>
nnoremap <leader>ln :lnext<CR>
nnoremap <leader>lp :lprevious<CR>

" FZF
nnoremap <leader>ff :Files<CR>
nnoremap <leader>fg :GFiles<CR>
nnoremap <leader>fb :Buffers<CR>
nnoremap <leader>fh :History<CR>
nnoremap <leader>fr :Rg<CR>
nnoremap <leader>fl :Lines<CR>
nnoremap <leader>ft :Tags<CR>
nnoremap <leader>fc :Commands<CR>
nnoremap <leader>fk :Maps<CR>
nnoremap <leader>fm :Marks<CR>
nnoremap <leader>fp :Files %:h<CR>

" Git
nnoremap <leader>gg :Git<CR>
nnoremap <leader>gc :Git commit<CR>
nnoremap <leader>gp :Git push<CR>
nnoremap <leader>gl :GV<CR>
nnoremap <leader>gL :GV!<CR>
nnoremap <leader>gd :Gdiffsplit<CR>
nnoremap <leader>gb :Git blame<CR>
nnoremap <leader>gB :GBrowse<CR>
nnoremap <leader>gf :Git fetch<CR>
nnoremap <leader>gm :Git merge<CR>
nnoremap <leader>gR :Git rebase<CR>
nnoremap <leader>gA :Git add %<CR>
nnoremap <leader>gS :Git stash<CR>
nnoremap <leader>gP :Git stash pop<CR>

" Git hunks
nnoremap <leader>hs :GitGutterStageHunk<CR>
nnoremap <leader>hu :GitGutterUndoHunk<CR>
nnoremap <leader>hp :GitGutterPreviewHunk<CR>
nmap     ]h         <Plug>(GitGutterNextHunk)
nmap     [h         <Plug>(GitGutterPrevHunk)

" Diff mode
augroup DiffMappings
  autocmd!
  autocmd OptionSet diff
    \ if v:option_new |
    \   nnoremap <buffer> <leader>dg :diffget<CR> |
    \   nnoremap <buffer> <leader>dp :diffput<CR> |
    \ endif
augroup END

" File explorer
nnoremap <silent> <leader>e :Fern . -drawer -reveal=% -toggle<CR>
nnoremap <silent> <leader>E :Fern . -drawer -reveal=%<CR>

" Terminal
nnoremap <silent> <C-\>  :FloatermToggle<CR>
tnoremap <silent> <C-\>  <C-\><C-n>:FloatermToggle<CR>
nnoremap <leader>tn :FloatermNew<CR>
nnoremap <leader>tk :FloatermKill<CR>
nnoremap <leader>tl :FloatermNext<CR>
nnoremap <leader>th :FloatermPrev<CR>
vnoremap <leader>ts :FloatermSend<CR>

" coc.nvim LSP
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

" DAP
nnoremap <silent> <F1>  :call vimspector#Continue()<CR>
nnoremap <silent> <F2>  :call vimspector#StepOver()<CR>
nnoremap <silent> <F3>  :call vimspector#StepInto()<CR>
nnoremap <silent> <F4>  :call vimspector#StepOut()<CR>
nnoremap <silent> <F10> :call vimspector#ToggleBreakpoint()<CR>
nnoremap <silent> <F11> :call vimspector#ToggleConditionalBreakpoint()<CR>
nnoremap <silent> <F12> :call vimspector#RunToCursor()<CR>
nnoremap <leader>dx     :call vimspector#Reset()<CR>
nnoremap <leader>dX     :call vimspector#ClearBreakpoints()<CR>
nnoremap <leader>di     :call vimspector#BalloonEval()<CR>
nnoremap <leader>dw     :call vimspector#AddWatch()<CR>

" Test runner
nnoremap <leader>tt :TestNearest<CR>
nnoremap <leader>tT :TestFile<CR>
nnoremap <leader>ta :TestSuite<CR>
nnoremap <leader>tR :TestLast<CR>
nnoremap <leader>tv :TestVisit<CR>

" REST
nnoremap <leader>rr :call VrcQuery()<CR>

" Database
nnoremap <leader>Du :DBUIToggle<CR>
nnoremap <leader>Df :DBUIFindBuffer<CR>

" Markdown
nnoremap <leader>mp :MarkdownPreview<CR>
nnoremap <leader>ms :MarkdownPreviewStop<CR>

" Toggles
nnoremap <leader>un :set number!<CR>
nnoremap <leader>ur :set relativenumber!<CR>
nnoremap <leader>uw :set wrap!<CR>
nnoremap <leader>us :set spell!<CR>
nnoremap <leader>uh :set hlsearch!<CR>
nnoremap <leader>ub :call ToggleBackground()<CR>
nnoremap <leader>uc :set cursorline!<CR>
nnoremap <leader>ul :set list!<CR>
nnoremap <leader>ui :IndentGuidesToggle<CR>
nnoremap <leader>ux :ContextToggle<CR>

" Tools
nnoremap <F8>      :TagbarToggle<CR>
nnoremap <leader>U :UndotreeToggle<CR>

" Session
nnoremap <leader>SS :SSave<CR>
nnoremap <leader>SL :SLoad<CR>
nnoremap <leader>Sd :SDelete<CR>
nnoremap <leader>Sc :SClose<CR>
nnoremap <leader>st :Startify<CR>

" Misc
nnoremap <leader>vi :VimInfo<CR>
nnoremap <leader>PR :FindProjectRoot<CR>
nnoremap <leader>yp "+p
nnoremap <leader>yy "+yy
vnoremap <leader>y  "+y

" F5-F9 runners
nnoremap <silent> <F5> :call <SID>RunAction('run')<CR>
nnoremap <silent> <F6> :call <SID>RunAction('compile')<CR>
nnoremap <silent> <F7> :call <SID>RunAction('build')<CR>
nnoremap <silent> <F9> :call <SID>RunAction('test')<CR>

" which-key
nnoremap <silent> <leader> :<c-u>WhichKey '<Space>'<CR>

" =============================================================================
" LAYER 7: PLUGIN CONFIGURATION
" =============================================================================

" Lightline
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

augroup LightlineCoc
  autocmd!
  autocmd User CocStatusChange,CocDiagnosticChange
    \ silent call lightline#update()
augroup END

" context.vim
let g:context_max_height       = 5
let g:context_enabled          = 1
let g:context_filetype_exclude = ['fern', 'startify', 'help', 'dbui', 'qf']

" better-escape
let g:better_escape_shortcut = ['jk', 'kj']
let g:better_escape_interval = 200

" Fern
let g:fern#renderer       = 'nerdfont'
let g:fern#default_hidden = 1
let g:fern#default_exclude =
  \ '^\%(\.git\|__pycache__\|node_modules\|\.DS_Store\|\.cache\|dist\|build\)$'

let g:fern_git_status#disable_ignored    = 1
let g:fern_git_status#disable_untracked  = 0
let g:fern_git_status#disable_submodules = 1

function! s:FernInit() abort
  nmap <buffer> <C-f> <Plug>(fern-mapping-fzf-select)
  nmap <buffer> <CR>  <Plug>(fern-action-open-or-expand)
  nmap <buffer> l     <Plug>(fern-action-open-or-expand)
  nmap <buffer> h     <Plug>(fern-action-collapse)
  nmap <buffer> ma    <Plug>(fern-action-new-path)
  nmap <buffer> md    <Plug>(fern-action-remove)
  nmap <buffer> mc    <Plug>(fern-action-copy)
  nmap <buffer> mm    <Plug>(fern-action-move)
  nmap <buffer> mr    <Plug>(fern-action-rename)
  nmap <buffer> R     <Plug>(fern-action-reload)
  nmap <buffer> I     <Plug>(fern-action-hidden-toggle)
  nmap <buffer> s     <Plug>(fern-action-open:split)
  nmap <buffer> v     <Plug>(fern-action-open:vsplit)
endfunction

augroup FernEvents
  autocmd!
  autocmd FileType fern call s:FernInit()
  autocmd BufEnter * ++nested
    \ if winnr('$') == 1 && &filetype ==# 'fern' | quit | endif
augroup END

" FZF
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

" coc.nvim
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

let g:coc_auto_copen = 0
let g:coc_enable_locationlist = 0

" Floaterm
let g:floaterm_width       = 0.88
let g:floaterm_height      = 0.88
let g:floaterm_autoclose   = 1
let g:floaterm_position    = 'center'
let g:floaterm_borderchars = '─│─│╭╮╯╰'
let g:floaterm_title       = '  terminal ($1/$2) '
let g:floaterm_wintype     = 'float'

" vim-vsnip
imap <expr> <C-e> vsnip#expandable() ? '<Plug>(vsnip-expand)'    : '<C-e>'
smap <expr> <C-e> vsnip#expandable() ? '<Plug>(vsnip-expand)'    : '<C-e>'
imap <expr> <C-l> vsnip#jumpable(1)  ? '<Plug>(vsnip-jump-next)' : '<C-l>'
smap <expr> <C-l> vsnip#jumpable(1)  ? '<Plug>(vsnip-jump-next)' : '<C-l>'
imap <expr> <C-b> vsnip#jumpable(-1) ? '<Plug>(vsnip-jump-prev)' : '<C-b>'
smap <expr> <C-b> vsnip#jumpable(-1) ? '<Plug>(vsnip-jump-prev)' : '<C-b>'

" vim-test
let g:test#strategy          = 'floaterm'
let g:test#python#runner     = 'pytest'
let g:test#javascript#runner = 'jest'
let g:test#go#runner         = 'gotest'
let g:test#rust#runner       = 'cargotest'

" vim-go
let g:go_fmt_command            = 'goimports'
let g:go_highlight_types        = 1
let g:go_highlight_fields       = 1
let g:go_highlight_functions    = 1
let g:go_highlight_operators    = 1
let g:go_def_mapping_enabled    = 0
let g:go_doc_keywordprg_enabled = 0

" rust.vim
let g:rustfmt_autosave = 0

" vim-move
let g:move_key_modifier = 'A'

" vim-matchup
let g:matchup_matchparen_offscreen          = { 'method': 'status_manual' }
let g:matchup_matchparen_deferred           = 1
let g:matchup_matchparen_hi_surround_always = 1
let g:matchup_motion_enabled                = 1
let g:matchup_text_obj_enabled              = 1

" vim-indent-guides
let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_start_level           = 2
let g:indent_guides_guide_size            = 1
let g:indent_guides_auto_colors           = 0
let g:indent_guides_exclude_filetypes     = ['startify', 'help', 'fern', 'dbui', 'json', 'terminal']

augroup IndentGuideColors
  autocmd!
  autocmd VimEnter,Colorscheme *
    \ highlight IndentGuidesOdd  ctermbg=235 guibg=#2a2a37 |
    \ highlight IndentGuidesEven ctermbg=236 guibg=#313244
augroup END

" Gitgutter
let g:gitgutter_enabled       = 1
let g:gitgutter_map_keys      = 0
let g:gitgutter_sign_added    = '▎'
let g:gitgutter_sign_modified = '▎'
let g:gitgutter_sign_removed  = '▎'

" Gutentags
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

augroup GutentagsSelectiveEnable
  autocmd!
  autocmd FileType sh,vim,fortran,cobol,vhdl,verilog,make,cmake,zig,d,nim,crystal,lua,perl,r,julia
    \ let g:gutentags_enabled = 1
augroup END

if executable('rg')
  let g:gutentags_file_list_command = 'rg --files --follow'
endif
let g:gutentags_add_default_project_roots = 0
let g:gutentags_project_root =
  \ ['.git', '.svn', '.hg', 'package.json', 'Cargo.toml', 'go.mod', 'pyproject.toml']

" vim-easy-align
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" REST console
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

" Database
let g:db_ui_use_nerd_fonts = 1
let g:db_ui_winwidth       = 40
let g:db_ui_save_location  = expand('~/.vim/db_ui')
let g:dbs = {}
if !empty($DATABASE_DEV_URL)     | let g:dbs['dev']     = $DATABASE_DEV_URL     | endif
if !empty($DATABASE_STAGING_URL) | let g:dbs['staging'] = $DATABASE_STAGING_URL | endif
if !empty($DATABASE_LOCAL_URL)   | let g:dbs['local']   = $DATABASE_LOCAL_URL   | endif

augroup DatabaseSQL
  autocmd!
  autocmd FileType sql setlocal omnifunc=vim_dadbod_completion#omni
augroup END

" vim-markdown
let g:vim_markdown_folding_disabled = 1
let g:vim_markdown_conceal          = 0
let g:vim_markdown_frontmatter      = 1
let g:vim_markdown_fenced_languages = [
  \ 'python', 'javascript', 'typescript', 'go', 'rust', 'bash=sh',
  \ 'sh', 'vim', 'json', 'yaml', 'html', 'css', 'c', 'cpp', 'zig',
  \ 'sql', 'kotlin', 'ruby', 'lua', 'fortran', 'cobol'
  \ ]

" MarkdownPreview
let g:mkdp_auto_close = 1
let g:mkdp_theme      = 'dark'
let g:mkdp_browser    = ''

" Startify
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

" which-key
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
" LAYER 8: LANGUAGE RUNNERS
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
    echohl ErrorMsg | echom 'File not readable: ' . l:fp | echohl None | return
  endif
  let l:cmd = get(get(s:runners, l:ft, {}), a:action, '')
  if empty(l:cmd)
    echohl WarningMsg | echom 'No ' . a:action . ' for filetype: ' . l:ft | echohl None | return
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
" LAYER 9: EVENT HANDLERS
" =============================================================================

function! s:HandleLargeFile() abort
  let l:size = getfsize(expand('<afile>:p'))
  if l:size > 10485760
    setlocal eventignore+=FileType bufhidden=unload undolevels=-1
    setlocal noundofile noswapfile syntax=off nowrap nocursorline norelativenumber
    echom 'Large file: performance mode active'
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
  let l:pat = '\%' . l:vstart[1] . 'l\%' . l:vstart[2] . 'c\_.*\%'
    \ . l:vend[1] . 'l\%' . l:vend[2] . 'c'
  call timer_start(0, {-> s:DoFlash(l:pat)})
endfunction

function! s:DoFlash(pat) abort
  let l:id = matchadd('IncSearch', a:pat)
  if l:id < 0 | return | endif
  call timer_start(250, {-> execute('silent! call matchdelete(' . l:id . ')', '')})
endfunction

augroup VimrcEvents
  autocmd!
  autocmd BufReadPre          * call s:HandleLargeFile()
  autocmd BufWritePre         * call s:StripTrailing()
  autocmd BufWritePre         * call s:MkdirOnSave()
  autocmd BufReadPost         * call s:RestoreCursor()
  autocmd TextYankPost        * silent! call s:FlashYank()
  autocmd FocusGained,BufEnter * silent! checktime
  autocmd VimResized          * wincmd =
  autocmd TerminalOpen        * setlocal nonumber norelativenumber signcolumn=no
augroup END

" =============================================================================
" LAYER 10: FILETYPE INDENTATION
" =============================================================================

augroup FileTypeIndent
  autocmd!
  autocmd FileType python                           setlocal ts=4 sw=4 expandtab
  autocmd FileType javascript,typescript            setlocal ts=2 sw=2 expandtab
  autocmd FileType javascriptreact,typescriptreact  setlocal ts=2 sw=2 expandtab
  autocmd FileType html,css,scss,sass,less          setlocal ts=2 sw=2 expandtab
  autocmd FileType json,jsonc                       setlocal ts=2 sw=2 expandtab foldmethod=syntax
  autocmd FileType graphql                          setlocal ts=2 sw=2 expandtab
  autocmd FileType go,make                          setlocal ts=4 sw=4 noexpandtab
  autocmd FileType c,cpp                            setlocal ts=4 sw=4 expandtab
  autocmd FileType rust                             setlocal ts=4 sw=4 expandtab
  autocmd FileType zig                              setlocal ts=4 sw=4 expandtab
  autocmd FileType java,kotlin                      setlocal ts=4 sw=4 expandtab
  autocmd FileType lua,vim,ruby                     setlocal ts=2 sw=2 expandtab
  autocmd FileType sh,zsh,bash,fish                 setlocal ts=2 sw=2 expandtab
  autocmd FileType perl,php                         setlocal ts=4 sw=4 expandtab
  autocmd FileType elixir,erlang                    setlocal ts=2 sw=2 expandtab
  autocmd FileType haskell,ocaml,elm                setlocal ts=2 sw=2 expandtab
  autocmd FileType scala,clojure,lisp               setlocal ts=2 sw=2 expandtab
  autocmd FileType fsharp,csharp                    setlocal ts=4 sw=4 expandtab
  autocmd FileType yaml,toml                        setlocal ts=2 sw=2 expandtab
  autocmd FileType xml,xhtml                        setlocal ts=2 sw=2 expandtab
  autocmd FileType sql                              setlocal ts=2 sw=2 expandtab
  autocmd FileType csv                              setlocal ts=4 sw=4 noexpandtab
  autocmd FileType terraform                        setlocal ts=2 sw=2 expandtab
  autocmd FileType dockerfile                       setlocal ts=2 sw=2 expandtab
  autocmd FileType fortran                          setlocal ts=3 sw=3 expandtab
  autocmd FileType cobol                            setlocal ts=4 sw=4 noexpandtab
  autocmd FileType vhdl,verilog                     setlocal ts=2 sw=2 expandtab
  autocmd FileType markdown,text
    \ setlocal ts=4 sw=4 expandtab spell textwidth=80 wrap linebreak foldmethod=expr foldexpr=0
  autocmd FileType gitcommit                        setlocal spell textwidth=72
  autocmd FileType tex                              setlocal ts=2 sw=2 expandtab spell
  autocmd FileType rst                              setlocal ts=3 sw=3 expandtab spell
  autocmd FileType swift                            setlocal ts=4 sw=4 expandtab
  autocmd FileType dart                             setlocal ts=2 sw=2 expandtab
  autocmd FileType r,rmd                            setlocal ts=2 sw=2 expandtab
  autocmd FileType julia                            setlocal ts=4 sw=4 expandtab
  autocmd FileType nim                              setlocal ts=2 sw=2 expandtab
  autocmd FileType crystal                          setlocal ts=2 sw=2 expandtab
  autocmd FileType d                                setlocal ts=4 sw=4 expandtab
augroup END

" =============================================================================
" LAYER 11: COMMANDS & UTILITIES
" =============================================================================

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
  echo '--- tools (✓ = found in PATH) ---'
  for l:t in ['node', 'python3', 'git', 'rg', 'bat', 'gopls', 'tmux',
    \          'rustc', 'cargo', 'go', 'tsc', 'java', 'lua', 'ruby', 'elixir']
    echo printf('%-12s: %s', l:t, executable(l:t) ? '✓' : '✗ not found')
  endfor
  echo '=================================='
endfunction

function! s:CleanBuffers() abort
  let l:cur     = bufnr('%')
  let l:closed  = 0
  let l:skipped = 0
  for i in range(1, bufnr('$'))
    if !buflisted(i) || i == l:cur
      continue
    endif
    if getbufvar(i, '&modified') || getbufvar(i, '&buftype') !=# ''
      let l:skipped += 1
      continue
    endif
    silent! execute 'bdelete ' . i
    let l:closed += 1
  endfor
  echom printf('Closed %d buffer(s), skipped %d (modified or special)', l:closed, l:skipped)
endfunction

function! s:FindProjectRoot() abort
  let l:markers = ['.git', '.svn', '.hg', 'package.json', 'Cargo.toml',
    \ 'go.mod', 'Makefile', 'pyproject.toml', 'build.gradle', 'build.zig']
  let l:dir = expand('%:p:h')
  while l:dir !=# '/'
    for l:m in l:markers
      if isdirectory(l:dir . '/' . l:m) || filereadable(l:dir . '/' . l:m)
        execute 'cd ' . fnameescape(l:dir)
        echom 'Project root: ' . l:dir | return
      endif
    endfor
    let l:dir = fnamemodify(l:dir, ':h')
  endwhile
  echom 'Project root not found'
endfunction

function! s:RenameFile(new) abort
  let l:old = expand('%:p')
  if l:old ==# fnamemodify(a:new, ':p')
    echohl WarningMsg | echom 'Source and destination are the same' | echohl None
    return
  endif
  let l:result = system('mv ' . shellescape(l:old) . ' ' . shellescape(a:new))
  if v:shell_error != 0
    echohl ErrorMsg | echom 'Rename failed: ' . l:result | echohl None
    return
  endif
  execute 'edit ' . fnameescape(a:new)
  execute 'bdelete ' . fnameescape(l:old)
  echom 'Renamed to ' . a:new
endfunction

function! s:CheckCocSettings() abort
  let l:path = expand('~/.vim/coc-settings.json')
  if !filereadable(l:path)
    echohl WarningMsg
    echom 'coc-settings.json not found at ' . l:path
    echohl None
    return
  endif
  let l:required = [
    \ 'diagnostic.errorSign', 'diagnostic.warningSign',
    \ 'diagnostic.infoSign',  'diagnostic.hintSign',
    \ 'diagnostic.virtualText', 'signature.enable', 'hover.autoHide',
    \ 'snippets.enable', 'coc.preferences.formatOnSaveFiletypes'
    \ ]
  let l:content = join(readfile(l:path), "\n")
  let l:missing = []
  for l:key in l:required
    if l:content !~# l:key
      call add(l:missing, l:key)
    endif
  endfor
  if empty(l:missing)
    echom 'coc-settings.json looks complete — all expected keys present.'
  else
    echohl WarningMsg
    echom 'coc-settings.json is missing keys: ' . join(l:missing, ', ')
    echohl None
  endif
endfunction

command! VimInfo          call s:VimInfo()
command! CleanBuffers     call s:CleanBuffers()
command! FindProjectRoot  call s:FindProjectRoot()
command! ReloadConfig     source $MYVIMRC | echom 'Config reloaded'
command! CheckCocSettings call s:CheckCocSettings()
command! -nargs=1 -complete=file Rename call s:RenameFile(<q-args>)

" =============================================================================
" LAYER 12: TMUX INTEGRATION
" =============================================================================

if exists('$TMUX')
  function! s:TmuxRenameAsync() abort
    if !buflisted(bufnr('%')) | return | endif
    if s:IsSpecialBuffer() | return | endif
    let l:name = expand('%:t')
    if empty(l:name) | return | endif
    call job_start(['tmux', 'rename-window', l:name])
  endfunction

  augroup TmuxRename
    autocmd!
    autocmd BufEnter * call s:TmuxRenameAsync()
    autocmd VimLeave * call job_start(['tmux', 'set-window-option', 'automatic-rename', 'on'])
  augroup END
endif

" =============================================================================
" LAYER 13: MACHINE-LOCAL OVERRIDES
" =============================================================================

if filereadable(expand('~/.vimrc.local'))
  source ~/.vimrc.local
endif

if g:vimide_debug
  echom 'Vim IDE loaded — :VimInfo for details'
endif
