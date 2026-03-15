" =============================================================================
" VIM FULL-SCALE IDE — Vim 8.2+ / openSUSE
" Philosophy: Fast. Self-contained. Professional. No AI. No cloud. No bloat.
" 40+ languages | LSP | DAP | Git | REST | SQL | Markdown | tmux-native
" Lazy-loaded per filetype — startup stays under 80ms regardless of stack size.
" =============================================================================

" -----------------------------------------------------------------------------
" IMPORTANT — openSUSE specific:
" /usr/share/vim/vim91/suse.vimrc loads BEFORE ~/.vimrc and resets g:loaded_*
" flags. To actually disable built-in plugins, run this ONCE:
"
"   mkdir -p ~/.vim/plugin
"   cat > ~/.vim/plugin/disable_builtins.vim << 'EOF'
"   let g:loaded_gzip=1 | let g:loaded_tar=1 | let g:loaded_tarPlugin=1
"   let g:loaded_zip=1  | let g:loaded_zipPlugin=1
"   let g:loaded_getscript=1 | let g:loaded_getscriptPlugin=1
"   let g:loaded_vimball=1   | let g:loaded_vimballPlugin=1
"   let g:loaded_2html_plugin=1 | let g:loaded_logiPat=1
"   let g:loaded_rrhelper=1
"   let g:loaded_netrw=1 | let g:loaded_netrwPlugin=1
"   let g:loaded_netrwSettings=1 | let g:loaded_netrwFileHandlers=1
"   EOF
"
" Also remove old LSP plugin dirs if they still exist:
"   rm -rf ~/.vim/plugged/vim-lsp ~/.vim/plugged/vim-lsp-settings
" -----------------------------------------------------------------------------

" -----------------------------------------------------------------------------
" 0. Must be first
" -----------------------------------------------------------------------------
set nocompatible
if has('vim_starting')
  set encoding=utf-8
endif
scriptencoding utf-8

set nomodeline
set modelines=0
set secure

" -----------------------------------------------------------------------------
" 1. Performance
" -----------------------------------------------------------------------------
set regexpengine=0
set synmaxcol=300
set lazyredraw
set updatetime=150
set redrawtime=1500
set ttimeoutlen=10
set timeoutlen=500

if has('mouse_sgr')
  set ttymouse=sgr
endif
set mouse=a

" -----------------------------------------------------------------------------
" 2. Core Settings
" -----------------------------------------------------------------------------
filetype plugin indent on
syntax enable

set fileencodings=utf-8,ucs-bom,latin1

" Editing
set backspace=indent,eol,start
set history=5000
set undolevels=2000
set undoreload=10000
set nrformats-=octal
set virtualedit=block
set nojoinspaces

" Display
set number
set relativenumber
set cursorline
set laststatus=2
set scrolloff=8
set sidescrolloff=5
set showcmd
set ruler
set showmatch
set matchtime=2
set display+=lastline
set signcolumn=yes
set pumheight=14
set cmdheight=1
set noshowmode
set shortmess+=acFI

" Search
set incsearch
set hlsearch
set ignorecase
set smartcase
set wrapscan
if executable('rg')
  set grepprg=rg\ --vimgrep\ --smart-case\ --follow
  set grepformat=%f:%l:%c:%m
endif

" Indentation defaults
set autoindent
set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4
set shiftround
set smarttab

" formatoptions
augroup FormatOptions
  autocmd!
  autocmd FileType * setlocal formatoptions-=cro
augroup END

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
set completeopt=menuone,noselect,noinsert

" File handling
if !isdirectory(expand('~/.vim/swap'))
  call mkdir(expand('~/.vim/swap'), 'p', 0700)
endif
set swapfile
set directory=~/.vim/swap//

if !isdirectory(expand('~/.vim/backup'))
  call mkdir(expand('~/.vim/backup'), 'p', 0700)
endif
set backup
set nowritebackup
set backupdir=~/.vim/backup//

set autoread

" Folding
set foldmethod=indent
set foldlevel=99
set foldlevelstart=99

" Visible whitespace
set list
set listchars=tab:▸\ ,trail:·,extends:→,precedes:←,nbsp:⦸
set fillchars=vert:│,fold:─

" Spell
set spelllang=en_us

" True colour
if has('termguicolors') && ($COLORTERM ==# 'truecolor' || $COLORTERM ==# '24bit')
  set termguicolors
endif

" GUI
if has('gui_running')
  set guifont=JetBrainsMono\ Nerd\ Font\ Mono:h12
  set guioptions=ac
  set columns=120
  set lines=35
endif

" Persistent undo
if has('persistent_undo')
  let s:undodir = expand('~/.vim/undo')
  if !isdirectory(s:undodir)
    call mkdir(s:undodir, 'p', 0700)
  endif
  if isdirectory(s:undodir)
    set undofile
    let &undodir = s:undodir
  endif
endif

" Clipboard
set clipboard=unnamed,unnamedplus
if has('wsl') && executable('clip.exe')
  let g:clipboard = {
    \ 'name':  'WSL',
    \ 'copy':  { '+': 'clip.exe', '*': 'clip.exe' },
    \ 'paste': { '+': 'powershell.exe -NoProfile -Command "Get-Clipboard"',
    \            '*': 'powershell.exe -NoProfile -Command "Get-Clipboard"' },
    \ 'cache_enabled': 0 }
endif

" -----------------------------------------------------------------------------
" 3. Plugin Manager — vim-plug
" -----------------------------------------------------------------------------
function! s:EnsureVimPlug()
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
    if v:shell_error != 0 | throw 'download failed (exit ' . v:shell_error . ')' | endif
    return 1
  catch
    echohl ErrorMsg
    echom 'vim-plug bootstrap failed: ' . v:exception
    echohl None
    return 0
  endtry
endfunction

if !s:EnsureVimPlug() | finish | endif

call plug#begin(expand('~/.vim/plugged'))

" ---------------------------------------------------------------------------
" APPEARANCE
" Modern themes — set $VIM_THEME in shell or ~/.vimrc.local
" ---------------------------------------------------------------------------
Plug 'catppuccin/vim', { 'as': 'catppuccin' }        " Mocha = default (recommended)
Plug 'rebelot/kanagawa.nvim'                          " deep ink-dark
Plug 'EdenEast/nightfox.nvim'                         " carbonfox variant
Plug 'morhetz/gruvbox'                                " fallback classic
Plug 'joshdick/onedark.vim'                           " fallback classic

" Statusline — lightline is 3× faster than airline, cleaner look
Plug 'itchyny/lightline.vim'
Plug 'mengelbrecht/lightline-bufferline'
Plug 'ryanoasis/vim-devicons'                         " filetype icons everywhere

" ---------------------------------------------------------------------------
" FILE EXPLORER — fern (Vim 8 native, async, full-featured)
" Replaces NERDTree completely. Lazy — only loads when invoked.
" ---------------------------------------------------------------------------
Plug 'lambdalisue/fern.vim',                    { 'on': ['Fern'] }
Plug 'lambdalisue/fern-git-status.vim',         { 'on': ['Fern'] }
Plug 'lambdalisue/nerdfont.vim',                { 'on': ['Fern'] }
Plug 'lambdalisue/fern-renderer-nerdfont.vim',  { 'on': ['Fern'] }
Plug 'LumaKernel/fern-mapping-fzf.vim',         { 'on': ['Fern'] }
Plug 'lambdalisue/vim-fern-hijack'              " replaces netrw silently

" ---------------------------------------------------------------------------
" NAVIGATION
" ---------------------------------------------------------------------------
Plug 'junegunn/fzf',     { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'christoomey/vim-tmux-navigator'           " C-h/j/k/l crosses tmux panes
Plug 'preservim/tagbar',  { 'on': 'TagbarToggle' }
Plug 'ludovicchabant/vim-gutentags'             " ctags auto-regeneration
Plug 'mhinz/vim-startify'                       " project dashboard

" ---------------------------------------------------------------------------
" LSP — full language intelligence on Vim 8
" ---------------------------------------------------------------------------
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/asyncomplete-lsp.vim'

" ---------------------------------------------------------------------------
" DAP — debugger protocol for Vim 8
" vimspector supports: Python, JS/TS, Go, C/C++, Rust, Java, Kotlin, Ruby
" Config lives in .vimspector.json at project root (see docs)
" ---------------------------------------------------------------------------
Plug 'puremourning/vimspector'

" ---------------------------------------------------------------------------
" EDITING — core power tools
" ---------------------------------------------------------------------------
Plug 'tpope/vim-commentary'                     " gc to comment
Plug 'tpope/vim-surround'                       " cs/ds/ys surround motions
Plug 'tpope/vim-repeat'                         " . repeats plugin actions
Plug 'tpope/vim-unimpaired'                     " [b ]b [q ]q bracket pairs
Plug 'tpope/vim-sleuth'                         " auto-detect indent
Plug 'jiangmiao/auto-pairs'                     " bracket/quote auto-close
Plug 'wellle/targets.vim'                       " extra text objects (cin, da,)
Plug 'matze/vim-move'                           " Alt+j/k move lines/blocks
Plug 'mbbill/undotree',   { 'on': 'UndotreeToggle' }
Plug 'jdhao/better-escape.vim'                  " jk/kj to escape insert mode
Plug 'mg979/vim-visual-multi'                   " MULTI-CURSOR (Ctrl+N)
Plug 'psliwka/vim-smoothie'                     " smooth C-d/C-u scrolling
Plug 'Yggdroot/indentLine'                      " indent guides ▏
Plug 'wellle/context.vim'                       " sticky function context at top

" ---------------------------------------------------------------------------
" LINTING / FORMATTING
" ---------------------------------------------------------------------------
Plug 'dense-analysis/ale'

" ---------------------------------------------------------------------------
" SNIPPETS
" ---------------------------------------------------------------------------
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'prabirshrestha/asyncomplete-ultisnips.vim'

" ---------------------------------------------------------------------------
" GIT
" ---------------------------------------------------------------------------
Plug 'tpope/vim-fugitive'                       " :Git everything
Plug 'tpope/vim-rhubarb'                        " GitHub integration
Plug 'airblade/vim-gitgutter'                   " live hunk signs
Plug 'junegunn/gv.vim'                          " beautiful git log graph (:GV)
Plug 'whiteinge/diffconflicts'                  " 2-way merge conflict resolution

" ---------------------------------------------------------------------------
" LANGUAGE SUPPORT — lazy by filetype
" vim-polyglot covers ~70 languages syntax/indent in one bundle.
" Individual plugins fill gaps or provide deeper support.
" ---------------------------------------------------------------------------
Plug 'sheerun/vim-polyglot'                     " base: 70+ languages

" Systems / compiled
Plug 'rust-lang/rust.vim',        { 'for': 'rust' }
Plug 'fatih/vim-go',              { 'for': 'go', 'do': ':GoUpdateBinaries' }
Plug 'rhysd/vim-llvm',            { 'for': ['c', 'cpp'] }
Plug 'ziglang/zig.vim',           { 'for': 'zig' }

" Web
Plug 'pangloss/vim-javascript',   { 'for': ['javascript', 'javascriptreact'] }
Plug 'leafgarland/typescript-vim', { 'for': ['typescript', 'typescriptreact'] }
Plug 'maxmellon/vim-jsx-pretty',  { 'for': ['javascriptreact', 'typescriptreact'] }
Plug 'jparise/vim-graphql',       { 'for': 'graphql' }
Plug 'mustache/vim-mustache-handlebars', { 'for': ['html.handlebars', 'mustache'] }

" Data / scripting
Plug 'Vimjas/vim-python-pep8-indent', { 'for': 'python' }
Plug 'vim-ruby/vim-ruby',         { 'for': 'ruby' }
Plug 'tpope/vim-rails',           { 'for': 'ruby' }
Plug 'udalov/kotlin-vim',         { 'for': 'kotlin' }

" Legacy / scientific
Plug 'vim-scripts/fortran.vim',   { 'for': 'fortran' }
Plug 'suoto/vim-hdl',             { 'for': ['vhdl', 'verilog'] }
" cobol — covered by vim-polyglot, vim-scripts/cobol.vim repo is dead

" DevOps / config
Plug 'hashivim/vim-terraform',    { 'for': 'terraform' }
Plug 'pearofducks/ansible-vim',   { 'for': ['yaml.ansible', 'yaml'] }
Plug 'chr4/nginx.vim',            { 'for': 'nginx' }
Plug 'ekalinin/Dockerfile.vim',   { 'for': 'Dockerfile' }
Plug 'towolf/vim-helm',           { 'for': 'helm' }
Plug 'fladson/vim-kitty',         { 'for': 'kitty' }

" Data formats
Plug 'elzr/vim-json',             { 'for': 'json' }
Plug 'cespare/vim-toml',          { 'for': 'toml' }
Plug 'stephpy/vim-yaml',          { 'for': 'yaml' }
Plug 'chrisbra/csv.vim',          { 'for': 'csv' }
Plug 'gennaro-tedesco/nvim-jqx',  { 'for': 'json' }  " JSON explorer :JqxList

" Markup / docs
Plug 'godlygeek/tabular'
Plug 'preservim/vim-markdown',    { 'for': 'markdown' }
Plug 'iamcco/markdown-preview.nvim', { 'for': 'markdown',
  \ 'do': { -> mkdp#util#install() } }             " :MarkdownPreview in browser
Plug 'lervag/vimtex',             { 'for': 'tex' }
Plug 'vim-scripts/xml.vim',       { 'for': ['xml', 'html'] }

" SQL
Plug 'tpope/vim-dadbod'
Plug 'kristijanhusak/vim-dadbod-ui'
Plug 'kristijanhusak/vim-dadbod-completion'

" ---------------------------------------------------------------------------
" TERMINAL
" ---------------------------------------------------------------------------
Plug 'voldikss/vim-floaterm'                    " floating terminal overlay

" ---------------------------------------------------------------------------
" REST CLIENT — :Http / RestConsole in a buffer
" ---------------------------------------------------------------------------
Plug 'diepm/vim-rest-console'                   " .rest files with REST calls
Plug 'baverman/vial-http',        { 'for': 'http' }

" ---------------------------------------------------------------------------
" TEST RUNNER
" ---------------------------------------------------------------------------
Plug 'vim-test/vim-test'                        " :TestNearest :TestFile :TestSuite
Plug 'tpope/vim-dispatch'                       " async test dispatch backend

" ---------------------------------------------------------------------------
" SESSION / UTILITIES
" ---------------------------------------------------------------------------
Plug 'tpope/vim-obsession'                      " session persistence
Plug 'editorconfig/editorconfig-vim'            " respect .editorconfig
Plug 'liuchengxu/vim-which-key'                 " leader key popup hints
Plug 'liuchengxu/vim-clap',       { 'do': ':Clap install-binary' } " universal picker
Plug 'szw/vim-maximizer',         { 'on': 'MaximizerToggle' }      " zoom a window
Plug 'tpope/vim-eunuch'                         " :Rename :Move :SudoWrite etc.

call plug#end()

" Auto-install missing plugins on startup
function! s:PlugAutoInstall()
  let l:missing = filter(values(g:plugs), '!isdirectory(v:val.dir)')
  if len(l:missing) > 0
    echom len(l:missing) . ' plugin(s) missing — installing... restart Vim when done.'
    PlugInstall --sync
  endif
endfunction
augroup PlugAutoInstall
  autocmd!
  autocmd VimEnter * call s:PlugAutoInstall()
augroup END

" -----------------------------------------------------------------------------
" 4. Colorscheme
" -----------------------------------------------------------------------------
" Set $VIM_THEME in shell or ~/.vimrc.local
" Available: catppuccin_mocha | catppuccin_frappe | kanagawa | carbonfox | gruvbox | onedark
let s:theme = !empty($VIM_THEME) ? $VIM_THEME : 'catppuccin_mocha'

function! s:ApplyTheme()
  try
    set background=dark
    execute 'colorscheme ' . s:theme
  catch
    colorscheme desert
    echom 'Theme "' . s:theme . '" not found — falling back to desert'
  endtry
endfunction

augroup ThemeInit
  autocmd!
  autocmd VimEnter * call s:ApplyTheme()
augroup END

function! ToggleBackground()
  let &background = (&background ==# 'dark') ? 'light' : 'dark'
endfunction

" -----------------------------------------------------------------------------
" 5. Leader and Key Mappings
" -----------------------------------------------------------------------------
let mapleader      = ' '
let maplocalleader = ','

" --- Config ---
nnoremap <leader><CR> :source $MYVIMRC \| echom 'Config reloaded'<CR>
nnoremap <leader>ec   :edit $MYVIMRC<CR>

" --- Save / quit ---
nnoremap <C-s>      :write<CR>
inoremap <C-s>      <C-o>:write<CR>
nnoremap <leader>w  :write<CR>
nnoremap <leader>q  :quit<CR>
nnoremap <leader>Q  :quit!<CR>
nnoremap <leader>qa :qall<CR>

" --- Window navigation (works across tmux panes via vim-tmux-navigator) ---
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" --- Window management ---
nnoremap <leader>wv :vsplit<CR>
nnoremap <leader>ws :split<CR>
nnoremap <leader>wc :close<CR>
nnoremap <leader>wo :only<CR>
nnoremap <leader>w= <C-w>=
nnoremap <leader>wz :MaximizerToggle<CR>

" --- Window resize ---
nnoremap <C-w>, :vertical resize -5<CR>
nnoremap <C-w>. :vertical resize +5<CR>
nnoremap <C-w>- :resize -3<CR>
nnoremap <C-w>+ :resize +3<CR>
nnoremap <M-Up>    :resize +2<CR>
nnoremap <M-Down>  :resize -2<CR>
nnoremap <M-Left>  :vertical resize -2<CR>
nnoremap <M-Right> :vertical resize +2<CR>

" --- Buffers ---
function! s:SafeBnext()
  if &buftype !=# '' || &filetype =~# '\v^(fern|floaterm|fugitive|dbui|qf|help)$'
    return
  endif
  bnext
endfunction
function! s:SafeBprev()
  if &buftype !=# '' || &filetype =~# '\v^(fern|floaterm|fugitive|dbui|qf|help)$'
    return
  endif
  bprevious
endfunction
nnoremap <silent> <Tab>   :call <SID>SafeBnext()<CR>
nnoremap <silent> <S-Tab> :call <SID>SafeBprev()<CR>
nnoremap <leader><Tab> <C-^>
nnoremap <leader>bd   :bdelete<CR>
nnoremap <leader>bD   :bdelete!<CR>
nnoremap <leader>bn   :bnext<CR>
nnoremap <leader>bp   :bprevious<CR>
nnoremap <leader>bC   :CleanBuffers<CR>

" --- Search ---
nnoremap <leader>sc :nohlsearch<CR>
nnoremap <leader>sr :%s/\<<C-r><C-w>\>//gc<Left><Left><Left>
vnoremap <leader>sr "hy:%s/<C-r>h//gc<Left><Left><Left>

" --- Quickfix ---
nnoremap <leader>co :copen<CR>
nnoremap <leader>cc :cclose<CR>
nnoremap <leader>cn :cnext<CR>
nnoremap <leader>cp :cprevious<CR>

" --- Location list ---
nnoremap <leader>lo :lopen<CR>
nnoremap <leader>lc :lclose<CR>
nnoremap <leader>ln :lnext<CR>
nnoremap <leader>lp :lprevious<CR>

" --- Movement ---
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk
nnoremap n     nzzzv
nnoremap N     Nzzzv
nnoremap *     *zzzv
nnoremap #     #zzzv
nnoremap <C-d> <C-d>zz
nnoremap <C-u> <C-u>zz

" --- Visual ---
vnoremap < <gv
vnoremap > >gv
vnoremap . :norm .<CR>

" --- Yank / paste ---
nnoremap Y          y$
nnoremap <leader>p  "+p
nnoremap <leader>P  "+P
vnoremap <leader>y  "+y
nnoremap <leader>yy "+yy
vnoremap <leader>vp "_dP

" --- FZF find ---
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

" --- Git (fugitive) ---
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
nnoremap <leader>gr :Git rebase<CR>

" --- Gitgutter hunks ---
nnoremap <leader>hs :GitGutterStageHunk<CR>
nnoremap <leader>hu :GitGutterUndoHunk<CR>
nnoremap <leader>hp :GitGutterPreviewHunk<CR>
nmap     ]h         <Plug>(GitGutterNextHunk)
nmap     [h         <Plug>(GitGutterPrevHunk)

" --- File explorer (fern) ---
nnoremap <silent> <leader>e  :Fern . -drawer -reveal=% -toggle<CR>
nnoremap <silent> <leader>E  :Fern . -drawer -reveal=%<CR>

" --- Terminal ---
nnoremap <silent> <C-\>     :FloatermToggle<CR>
tnoremap <silent> <C-\>     <C-\><C-n>:FloatermToggle<CR>
nnoremap <leader>tn :FloatermNew<CR>
nnoremap <leader>tk :FloatermKill<CR>
nnoremap <leader>tl :FloatermNext<CR>
nnoremap <leader>th :FloatermPrev<CR>
" Send selection to terminal
vnoremap <leader>ts :FloatermSend<CR>

" --- LSP ---
nnoremap <silent> gd        :LspDefinition<CR>
nnoremap <silent> gD        :LspDeclaration<CR>
nnoremap <silent> gr        :LspReferences<CR>
nnoremap <silent> gi        :LspImplementation<CR>
nnoremap <silent> gt        :LspTypeDefinition<CR>
nnoremap <silent> K         :LspHover<CR>
nnoremap <silent> <leader>rn :LspRename<CR>
nnoremap <silent> <leader>ca :LspCodeAction<CR>
nnoremap <silent> <leader>cf :LspDocumentFormat<CR>
nnoremap <silent> <leader>cs :LspDocumentSymbol<CR>
nnoremap <silent> <leader>cS :LspWorkspaceSymbol<CR>
nnoremap <silent> ]e        :LspNextError<CR>
nnoremap <silent> [e        :LspPreviousError<CR>
nnoremap <silent> ]w        :LspNextWarning<CR>
nnoremap <silent> [w        :LspPreviousWarning<CR>

" --- DAP / Vimspector ---
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

" --- Test runner ---
nnoremap <leader>Tt :TestNearest<CR>
nnoremap <leader>TT :TestFile<CR>
nnoremap <leader>Ta :TestSuite<CR>
nnoremap <leader>Tl :TestLast<CR>
nnoremap <leader>Tv :TestVisit<CR>

" --- REST client ---
nnoremap <leader>Rr :call VrcQuery()<CR>

" --- Database ---
nnoremap <leader>Du :DBUIToggle<CR>
nnoremap <leader>Df :DBUIFindBuffer<CR>

" --- Markdown preview ---
nnoremap <leader>mp :MarkdownPreview<CR>
nnoremap <leader>ms :MarkdownPreviewStop<CR>

" --- Toggles ---
nnoremap <leader>un :set number!<CR>
nnoremap <leader>ur :set relativenumber!<CR>
nnoremap <leader>uw :set wrap!<CR>
nnoremap <leader>us :set spell!<CR>
nnoremap <leader>uh :set hlsearch!<CR>
nnoremap <leader>ub :call ToggleBackground()<CR>
nnoremap <leader>uc :set cursorline!<CR>
nnoremap <leader>ul :set list!<CR>
nnoremap <leader>ui :IndentLinesToggle<CR>
nnoremap <leader>ux :ContextToggle<CR>

" --- Tools ---
nnoremap <F8>      :TagbarToggle<CR>
nnoremap <leader>U :UndotreeToggle<CR>

" --- Session ---
nnoremap <leader>SS :SSave<CR>
nnoremap <leader>SL :SLoad<CR>
nnoremap <leader>Sd :SDelete<CR>
nnoremap <leader>Sc :SClose<CR>
nnoremap <leader>st :Startify<CR>

" --- Misc ---
nnoremap <leader>vi :VimInfo<CR>
nnoremap <leader>PR :FindProjectRoot<CR>
nnoremap <leader>wZ :MaximizerToggle<CR>

" which-key popup — must be last leader mapping
nnoremap <silent> <leader> :<c-u>WhichKey '<Space>'<CR>

" Run / compile / build / test — F5–F9
nnoremap <silent> <F5> :call <SID>RunAction('run')<CR>
nnoremap <silent> <F6> :call <SID>RunAction('compile')<CR>
nnoremap <silent> <F7> :call <SID>RunAction('build')<CR>
nnoremap <silent> <F9> :call <SID>RunAction('test')<CR>

" -----------------------------------------------------------------------------
" 6. Plugin Configuration
" -----------------------------------------------------------------------------

" --- better-escape ---
let g:better_escape_shortcut = ['jk', 'kj']
let g:better_escape_interval = 200

" --- Lightline ---
let s:ll_theme_map = {
  \ 'catppuccin_mocha':  'catppuccin_mocha',
  \ 'catppuccin_frappe': 'catppuccin_frappe',
  \ 'kanagawa':          'one',
  \ 'carbonfox':         'one',
  \ 'gruvbox':           'gruvbox',
  \ 'onedark':           'onedark',
  \ }
let g:lightline = {
  \ 'colorscheme': get(s:ll_theme_map, s:theme, 'one'),
  \ 'active': {
  \   'left':  [['mode','paste'],
  \             ['gitbranch','readonly','filename','modified']],
  \   'right': [['lineinfo'], ['percent'],
  \             ['lsp_status', 'filetype', 'fileencoding']]
  \ },
  \ 'tabline': {
  \   'left':  [['buffers']],
  \   'right': [['close']]
  \ },
  \ 'component_expand':   { 'buffers': 'lightline#bufferline#buffers' },
  \ 'component_type':     { 'buffers': 'tabsel' },
  \ 'component_function': { 'gitbranch': 'FugitiveHead' },
  \ 'separator':    { 'left': '', 'right': '' },
  \ 'subseparator': { 'left': '', 'right': '' },
  \ }
set showtabline=2
let g:lightline#bufferline#show_number      = 1
let g:lightline#bufferline#unicode_symbols  = 1
let g:lightline#bufferline#enable_devicons  = 1

" --- Fern ---
let g:fern#renderer                         = 'nerdfont'
let g:fern#default_hidden                   = 1
let g:fern#default_exclude =
  \ '^\%(\.git\|__pycache__\|node_modules\|\.DS_Store\|\.cache\|dist\|build\)$'

let g:fern_git_status#disable_ignored      = 1
let g:fern_git_status#disable_untracked    = 0
let g:fern_git_status#disable_submodules   = 1

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

" --- FZF ---
" Floating popup layout — feels like VS Code command palette
let g:fzf_layout = {
  \ 'window': { 'width': 0.92, 'height': 0.88, 'rounded': v:true }
  \ }
let g:fzf_history_dir = expand('~/.vim/fzf-history')

" Catppuccin Mocha colour palette for FZF
let $FZF_DEFAULT_OPTS =
  \ '--layout=reverse --border=rounded --info=inline ' .
  \ '--color=bg+:#313244,bg:#1e1e2e,spinner:#f5e0dc,hl:#f38ba8 ' .
  \ '--color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc ' .
  \ '--color=marker:#f5e0dc,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8'

" Use bat for syntax-highlighted preview (install: sudo zypper in bat)
if executable('bat')
  let $FZF_DEFAULT_OPTS .= ' --preview "bat --style=numbers --color=always {}"'
endif

" --- vim-lsp ---
let g:lsp_diagnostics_signs_error         = { 'text': '󰅚 ' }
let g:lsp_diagnostics_signs_warning       = { 'text': '󰀪 ' }
let g:lsp_diagnostics_signs_information   = { 'text': '󰋽 ' }
let g:lsp_diagnostics_signs_hint          = { 'text': '󰌶 ' }
let g:lsp_diagnostics_virtual_text_enabled = 0
let g:lsp_document_highlight_enabled      = 1
let g:lsp_signature_help_enabled          = 1
let g:lsp_completion_documentation_delay  = 0
let g:lsp_fold_enabled                    = 0
let g:lsp_log_verbose                     = 0
let g:lsp_log_file                        = expand('~/.vim/vim-lsp.log')
let g:lsp_settings_servers_dir            = expand('~/.local/share/vim-lsp-settings/servers')

" Language server bindings — all lazy, auto-installed by vim-lsp-settings
let g:lsp_settings_filetype_go            = ['gopls']
let g:lsp_settings_filetype_rust          = ['rust-analyzer']
let g:lsp_settings_filetype_python        = ['pylsp']
let g:lsp_settings_filetype_typescript    = ['typescript-language-server']
let g:lsp_settings_filetype_javascript    = ['typescript-language-server']
let g:lsp_settings_filetype_lua           = ['sumneko-lua-language-server']
let g:lsp_settings_filetype_vim           = ['vim-language-server']
let g:lsp_settings_filetype_sh            = ['bash-language-server']
let g:lsp_settings_filetype_c             = ['clangd']
let g:lsp_settings_filetype_cpp           = ['clangd']
let g:lsp_settings_filetype_kotlin        = ['kotlin-language-server']
let g:lsp_settings_filetype_ruby          = ['solargraph']
let g:lsp_settings_filetype_yaml          = ['yaml-language-server']
let g:lsp_settings_filetype_json          = ['json-languageserver']
let g:lsp_settings_filetype_html          = ['html-languageserver']
let g:lsp_settings_filetype_css           = ['css-languageserver']
let g:lsp_settings_filetype_dockerfile    = ['dockerfile-language-server']
let g:lsp_settings_filetype_terraform     = ['terraform-ls']
let g:lsp_settings_filetype_zig           = ['zls']

" --- asyncomplete ---
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <CR>    pumvisible() ? asyncomplete#close_popup() : "\<CR>"
imap <C-Space> <Plug>(asyncomplete_force_refresh)
let g:asyncomplete_auto_popup       = 1
let g:asyncomplete_auto_completeopt = 0
let g:asyncomplete_popup_delay      = 50

augroup AsyncompleteUltisnips
  autocmd!
  autocmd User asyncomplete_setup
    \ call asyncomplete#register_source(
    \   asyncomplete#sources#ultisnips#get_source_options({
    \     'name':      'ultisnips',
    \     'whitelist': ['*'],
    \     'completor': function('asyncomplete#sources#ultisnips#completor'),
    \ }))
augroup END

" --- ALE ---
let g:ale_linters_explicit     = 1
let g:ale_linters = {
  \ 'python':     ['ruff', 'mypy'],
  \ 'javascript': ['eslint'],
  \ 'typescript': ['eslint'],
  \ 'sh':         ['shellcheck'],
  \ 'vim':        ['vint'],
  \ 'go':         [],
  \ 'rust':       [],
  \ 'c':          [],
  \ 'cpp':        [],
  \ }
let g:ale_fixers = {
  \ '*':          ['remove_trailing_lines', 'trim_whitespace'],
  \ 'python':     ['black', 'isort'],
  \ 'javascript': ['prettier'],
  \ 'typescript': ['prettier'],
  \ 'go':         ['gofmt', 'goimports'],
  \ 'rust':       ['rustfmt'],
  \ 'sh':         ['shfmt'],
  \ 'c':          ['clang-format'],
  \ 'cpp':        ['clang-format'],
  \ 'kotlin':     ['ktlint'],
  \ 'ruby':       ['rubocop'],
  \ 'yaml':       ['prettier'],
  \ 'json':       ['prettier'],
  \ 'html':       ['prettier'],
  \ 'css':        ['prettier'],
  \ 'zig':        ['zigfmt'],
  \ }
let g:ale_fix_on_save          = 1
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_insert_leave = 0
let g:ale_lint_on_enter        = 0
let g:ale_sign_error           = '󰅚 '
let g:ale_sign_warning         = '󰀪 '
let g:ale_echo_msg_format      = '[%linter%] %s [%severity%]'
let g:ale_virtualtext_cursor   = 'disabled'
let g:ale_disable_lsp          = 1

" --- Vimspector (DAP) ---
" Layout: code top-left, terminal right, variables bottom-left, watches bottom-right
let g:vimspector_enable_mappings = 'NONE'   " we use custom bindings above
let g:vimspector_sign_priority = {
  \ 'vimspectorBP':          20,
  \ 'vimspectorBPCond':      19,
  \ 'vimspectorBPDisabled':  18,
  \ 'vimspectorPC':          999,
  \ }
" Gadgets auto-install per filetype — :VimspectorInstall <gadget>
" Supported: debugpy(python) vscode-go(go) CodeLLDB(c/cpp/rust)
"            vscode-node(js/ts) java-debug-adapter kotlin-debug-adapter

" --- vim-go ---
let g:go_fmt_command          = 'goimports'
let g:go_highlight_types      = 1
let g:go_highlight_fields     = 1
let g:go_highlight_functions  = 1
let g:go_highlight_operators  = 1
let g:go_def_mapping_enabled  = 0   " let vim-lsp handle gd
let g:go_doc_keywordprg_enabled = 0 " let vim-lsp handle K

" --- rust.vim ---
let g:rustfmt_autosave = 0   " ALE handles formatting

" --- UltiSnips ---
let g:UltiSnipsExpandTrigger       = '<C-e>'
let g:UltiSnipsJumpForwardTrigger  = '<C-l>'
let g:UltiSnipsJumpBackwardTrigger = '<C-b>'
let g:UltiSnipsEditSplit           = 'vertical'

" --- Floaterm ---
let g:floaterm_width       = 0.88
let g:floaterm_height      = 0.88
let g:floaterm_autoclose   = 1
let g:floaterm_position    = 'center'
let g:floaterm_borderchars = '─│─│╭╮╯╰'
let g:floaterm_title       = '  terminal ($1/$2) '
let g:floaterm_wintype     = 'float'

" --- vim-tmux-navigator ---
" Disable Vim→tmux navigation when zoomed (tmux zoom stays intact)
let g:tmux_navigator_disable_when_zoomed = 1
let g:tmux_navigator_save_on_switch      = 2  " :update before switching

" --- vim-smoothie ---
let g:smoothie_speed_constant_factor = 30
let g:smoothie_speed_linear_factor   = 30

" --- indentLine ---
let g:indentLine_char                   = '▏'
let g:indentLine_first_char             = '▏'
let g:indentLine_showFirstIndentLevel   = 1
let g:indentLine_fileTypeExclude        = ['startify', 'help', 'fern', 'dbui', 'json']
let g:indentLine_bufTypeExclude         = ['terminal']

" --- context.vim (sticky scroll) ---
let g:context_enabled          = 1
let g:context_max_height       = 5
let g:context_filetype_exclude = ['fern', 'startify', 'help', 'dbui']

" --- Gitgutter ---
let g:gitgutter_enabled        = 1
let g:gitgutter_map_keys       = 0
let g:gitgutter_sign_added     = '▎'
let g:gitgutter_sign_modified  = '▎'
let g:gitgutter_sign_removed   = '▎'

" --- Gutentags ---
let g:gutentags_cache_dir                 = expand('~/.vim/tags')
let g:gutentags_generate_on_new           = 1
let g:gutentags_generate_on_missing       = 1
let g:gutentags_generate_on_write         = 1
let g:gutentags_ctags_extra_args          = ['--tag-relative=yes', '--fields=+ailmnS']
let g:gutentags_add_default_project_roots = 0
let g:gutentags_project_root              =
  \ ['.git', '.svn', '.hg', 'package.json', 'Cargo.toml', 'go.mod', 'pyproject.toml']

" --- vim-move ---
let g:move_key_modifier = 'A'

" --- vim-visual-multi (multi-cursor) ---
let g:VM_maps                       = {}
let g:VM_maps['Find Under']         = '<C-n>'
let g:VM_maps['Find Subword Under'] = '<C-n>'
let g:VM_maps['Select All']         = '<leader>ma'
let g:VM_maps['Skip Region']        = '<C-x>'

" --- vim-test ---
let g:test#strategy            = 'floaterm'
let g:test#python#runner       = 'pytest'
let g:test#javascript#runner   = 'jest'
let g:test#go#runner           = 'gotest'
let g:test#rust#runner         = 'cargotest'

" --- REST console ---
let g:vrc_curl_opts = {
  \ '--include':     '',
  \ '--location':    '',
  \ '--show-error':  '',
  \ '--silent':      '',
  \ }
let g:vrc_auto_format_response_patterns = {
  \ 'json': 'python3 -m json.tool',
  \ 'xml':  'xmllint --format -',
  \ }
let g:vrc_output_buffer_name = '__VRC_OUTPUT__'

" --- vim-markdown ---
let g:vim_markdown_folding_disabled = 1
let g:vim_markdown_conceal          = 0
let g:vim_markdown_frontmatter      = 1
let g:vim_markdown_fenced_languages = [
  \ 'python', 'javascript', 'typescript', 'go', 'rust', 'bash=sh',
  \ 'sh', 'vim', 'json', 'yaml', 'html', 'css', 'c', 'cpp', 'zig',
  \ 'sql', 'kotlin', 'ruby', 'lua', 'fortran', 'cobol'
  \ ]

" --- MarkdownPreview ---
let g:mkdp_auto_close  = 1
let g:mkdp_theme       = 'dark'
let g:mkdp_browser     = ''  " use system default

" --- Database ---
let g:db_ui_use_nerd_fonts = 1
let g:db_ui_winwidth       = 40
let g:db_ui_save_location  = expand('~/.vim/db_ui')
let g:dbs = {}
if !empty($DATABASE_DEV_URL)     | let g:dbs['dev']     = $DATABASE_DEV_URL     | endif
if !empty($DATABASE_STAGING_URL) | let g:dbs['staging'] = $DATABASE_STAGING_URL | endif
if !empty($DATABASE_LOCAL_URL)   | let g:dbs['local']   = $DATABASE_LOCAL_URL   | endif

" --- Startify ---
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
let g:startify_lists = [
  \ { 'type': 'sessions',  'header': ['   Sessions']        },
  \ { 'type': 'files',     'header': ['   Recent files']    },
  \ { 'type': 'dir',       'header': ['   ' . getcwd()]     },
  \ { 'type': 'bookmarks', 'header': ['   Bookmarks']       },
  \ ]
let g:startify_bookmarks = [
  \ { 'v': '~/.vimrc'  },
  \ { 'z': '~/.zshrc'  },
  \ { 'b': '~/.bashrc' },
  \ ]

" --- which-key ---
call which_key#register('<Space>', "g:which_key_map")
let g:which_key_map                  = {}
let g:which_key_map['<CR>']          = 'reload config'
let g:which_key_map['<Tab>']         = 'last buffer'
let g:which_key_map.e                = 'file explorer'
let g:which_key_map.E                = 'reveal in tree'
let g:which_key_map.U                = 'undo tree'
let g:which_key_map.w                = { 'name': '+window'    }
let g:which_key_map.b                = { 'name': '+buffer'    }
let g:which_key_map.f                = { 'name': '+find/fzf'  }
let g:which_key_map.g                = { 'name': '+git'       }
let g:which_key_map.h                = { 'name': '+hunk'      }
let g:which_key_map.t                = { 'name': '+terminal'  }
let g:which_key_map.a                = { 'name': '+ale'       }
let g:which_key_map.u                = { 'name': '+toggle'    }
let g:which_key_map.s                = { 'name': '+search'    }
let g:which_key_map.y                = { 'name': '+yank'      }
let g:which_key_map.c                = { 'name': '+lsp/qf'    }
let g:which_key_map.l                = { 'name': '+loclist'   }
let g:which_key_map.r                = { 'name': '+lsp'       }
let g:which_key_map.d                = { 'name': '+debug/dap' }
let g:which_key_map.D                = { 'name': '+database'  }
let g:which_key_map.S                = { 'name': '+session'   }
let g:which_key_map.T                = { 'name': '+test'      }
let g:which_key_map.R                = { 'name': '+rest'      }
let g:which_key_map.m                = { 'name': '+multicursor'}
let g:which_key_map.p                = { 'name': '+markdown'  }

" -----------------------------------------------------------------------------
" 7. Autocommands
" -----------------------------------------------------------------------------
function! s:HandleLargeFile()
  if getfsize(expand('%')) > 10485760
    setlocal eventignore+=FileType
    setlocal bufhidden=unload
    setlocal undolevels=-1
    setlocal noundofile noswapfile
    setlocal syntax=off nowrap nocursorline norelativenumber
    echom 'Large file: performance mode active'
  endif
endfunction

function! s:StripTrailing()
  if &modifiable && &filetype !~# '\v^(markdown|diff|cobol)$'
    let l:view = winsaveview()
    silent! %s/\s\+$//e
    call winrestview(l:view)
  endif
endfunction

function! s:MkdirOnSave()
  let l:dir = expand('%:p:h')
  if !isdirectory(l:dir) | call mkdir(l:dir, 'p') | endif
endfunction

function! s:RestoreCursor()
  if line("'\"") > 0 && line("'\"") <= line("$") && &filetype !~# 'commit'
    execute "normal! g`\""
  endif
endfunction

function! s:FlashYank()
  let l:pos = getpos("'[")
  let l:end = getpos("']")
  if l:pos[1] > 0 && l:end[1] > 0
    let l:id = matchadd('IncSearch',
      \ '\%' . l:pos[1] . 'l\%' . l:pos[2] . 'c\_.*\%'
      \ . l:end[1] . 'l\%' . l:end[2] . 'c')
    call timer_start(250, {t -> l:id >= 1 ? matchdelete(l:id) : 0})
  endif
endfunction

augroup VimrcEvents
  autocmd!
  autocmd BufReadPre   * call s:HandleLargeFile()
  autocmd BufWritePre  * call s:StripTrailing()
  autocmd BufWritePre  * call s:MkdirOnSave()
  autocmd BufReadPost  * call s:RestoreCursor()
  autocmd TextYankPost * silent! call s:FlashYank()
  autocmd FocusGained,BufEnter * silent! checktime
  autocmd VimResized           * wincmd =
  autocmd TerminalOpen         * setlocal nonumber norelativenumber signcolumn=no
  autocmd FileType sql         setlocal omnifunc=vim_dadbod_completion#omni
augroup END

" -----------------------------------------------------------------------------
" 8. Filetype indent — 40+ languages
" -----------------------------------------------------------------------------
augroup FileTypeIndent
  autocmd!
  " Python
  autocmd FileType python                  setlocal ts=4 sw=4 expandtab
  " Web
  autocmd FileType javascript,typescript   setlocal ts=2 sw=2 expandtab
  autocmd FileType javascriptreact,typescriptreact setlocal ts=2 sw=2 expandtab
  autocmd FileType html,css,scss,sass,less setlocal ts=2 sw=2 expandtab
  autocmd FileType json,jsonc              setlocal ts=2 sw=2 expandtab
  autocmd FileType graphql                 setlocal ts=2 sw=2 expandtab
  " Systems
  autocmd FileType go,make                 setlocal ts=4 sw=4 noexpandtab
  autocmd FileType c,cpp                   setlocal ts=4 sw=4 expandtab
  autocmd FileType rust                    setlocal ts=4 sw=4 expandtab
  autocmd FileType zig                     setlocal ts=4 sw=4 expandtab
  autocmd FileType java,kotlin             setlocal ts=4 sw=4 expandtab
  " Scripting
  autocmd FileType lua,vim,ruby            setlocal ts=2 sw=2 expandtab
  autocmd FileType sh,zsh,bash,fish        setlocal ts=2 sw=2 expandtab
  autocmd FileType perl,php                setlocal ts=4 sw=4 expandtab
  autocmd FileType elixir,erlang           setlocal ts=2 sw=2 expandtab
  autocmd FileType haskell,ocaml,elm       setlocal ts=2 sw=2 expandtab
  autocmd FileType scala,clojure,lisp      setlocal ts=2 sw=2 expandtab
  autocmd FileType fsharp,csharp           setlocal ts=4 sw=4 expandtab
  " Data / config
  autocmd FileType yaml,toml               setlocal ts=2 sw=2 expandtab
  autocmd FileType xml,xhtml               setlocal ts=2 sw=2 expandtab
  autocmd FileType sql                     setlocal ts=2 sw=2 expandtab
  autocmd FileType csv                     setlocal ts=4 sw=4 noexpandtab
  autocmd FileType terraform               setlocal ts=2 sw=2 expandtab
  autocmd FileType dockerfile              setlocal ts=2 sw=2 expandtab
  " Legacy / scientific
  autocmd FileType fortran                 setlocal ts=3 sw=3 expandtab
  autocmd FileType cobol                   setlocal ts=4 sw=4 noexpandtab
  autocmd FileType vhdl,verilog            setlocal ts=2 sw=2 expandtab
  " Docs
  autocmd FileType markdown,text           setlocal ts=4 sw=4 expandtab spell textwidth=80 wrap linebreak
  autocmd FileType gitcommit               setlocal spell textwidth=72
  autocmd FileType tex                     setlocal ts=2 sw=2 expandtab spell
  autocmd FileType rst                     setlocal ts=3 sw=3 expandtab spell
  " Niche but commonly needed
  autocmd FileType swift                   setlocal ts=4 sw=4 expandtab
  autocmd FileType dart                    setlocal ts=2 sw=2 expandtab
  autocmd FileType r,rmd                   setlocal ts=2 sw=2 expandtab
  autocmd FileType julia                   setlocal ts=4 sw=4 expandtab
  autocmd FileType nim                     setlocal ts=2 sw=2 expandtab
  autocmd FileType crystal                 setlocal ts=2 sw=2 expandtab
  autocmd FileType d                       setlocal ts=4 sw=4 expandtab
augroup END

" -----------------------------------------------------------------------------
" 9. Terminal execution — F5/F6/F7/F9 run/compile/build/test
" -----------------------------------------------------------------------------
let s:runners = {
  \ 'python':     { 'run': 'python3 "{filepath}"',        'test': 'python3 -m pytest "{dirname}"' },
  \ 'javascript': { 'run': 'node "{filepath}"',            'test': 'npm test' },
  \ 'typescript': { 'run': 'ts-node "{filepath}"',         'test': 'npm test', 'compile': 'tsc "{filepath}"' },
  \ 'go':         { 'run': 'go run "{filepath}"',          'test': 'go test ./...', 'build': 'go build -o "{basename}" "{filepath}"' },
  \ 'rust':       { 'run': './{basename}',                 'test': 'cargo test', 'build': 'cargo build --release', 'compile': 'rustc "{filepath}"' },
  \ 'c':          { 'run': './{basename}',                 'compile': 'gcc -Wall -O2 -g "{filepath}" -o "{basename}" -lm' },
  \ 'cpp':        { 'run': './{basename}',                 'compile': 'g++ -Wall -O2 -g -std=c++17 "{filepath}" -o "{basename}" -lm' },
  \ 'zig':        { 'run': 'zig run "{filepath}"',         'build': 'zig build', 'test': 'zig test "{filepath}"' },
  \ 'kotlin':     { 'compile': 'kotlinc "{filepath}" -include-runtime -d "{basename}.jar"', 'run': 'java -jar "{basename}.jar"' },
  \ 'sh':         { 'run': 'bash "{filepath}"' },
  \ 'lua':        { 'run': 'lua "{filepath}"' },
  \ 'ruby':       { 'run': 'ruby "{filepath}"',            'test': 'ruby -Itest "{filepath}"' },
  \ 'fortran':    { 'compile': 'gfortran -O2 "{filepath}" -o "{basename}"', 'run': './{basename}' },
  \ 'cobol':      { 'compile': 'cobc -x "{filepath}" -o "{basename}"',      'run': './{basename}' },
  \ 'julia':      { 'run': 'julia "{filepath}"' },
  \ 'r':          { 'run': 'Rscript "{filepath}"' },
  \ 'nim':        { 'run': 'nim c -r "{filepath}"',        'build': 'nim c -d:release "{filepath}"' },
  \ 'crystal':    { 'run': 'crystal run "{filepath}"',     'build': 'crystal build --release "{filepath}"' },
  \ 'd':          { 'compile': 'dmd "{filepath}" -of="{basename}"', 'run': './{basename}' },
  \ }

function! s:RunAction(action)
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
  let l:vars = {
    \ 'filepath': l:fp,         'dirname':  fnamemodify(l:fp, ':h'),
    \ 'filename': expand('%:t'), 'basename': expand('%:t:r'),
    \ }
  let l:cmd = substitute(l:cmd, '{\(\w\+\)}', '\=get(l:vars, submatch(1), submatch(0))', 'g')
  execute 'FloatermNew --autoclose=0 ' . l:cmd
endfunction

" -----------------------------------------------------------------------------
" 10. Commands and utilities
" -----------------------------------------------------------------------------
function! s:VimInfo()
  echo '========== Vim IDE Info =========='
  echo 'Version  : Vim ' . v:version/100 . '.' . v:version%100
  echo 'Config   : ' . $MYVIMRC
  echo 'Filetype : ' . &filetype
  echo 'Theme    : ' . (exists('g:colors_name') ? g:colors_name : 'none') . ' (' . &background . ')'
  echo '$VIM_THEME: ' . (!empty($VIM_THEME) ? $VIM_THEME : '(not set → catppuccin_mocha)')
  echo 'Python3  : ' . (executable('python3') ? trim(system('python3 --version')) : 'not found')
  echo 'Node     : ' . (executable('node')    ? trim(system('node --version'))    : 'not found')
  echo 'Git      : ' . (executable('git')     ? trim(system('git --version'))     : 'not found')
  echo 'rg       : ' . (executable('rg')      ? trim(system('rg --version | head -1')) : 'not found')
  echo 'bat      : ' . (executable('bat')     ? trim(system('bat --version'))     : 'not found (optional, for FZF preview)')
  echo 'gopls    : ' . (executable('gopls')   ? 'found' : 'not found')
  echo 'tmux     : ' . (executable('tmux')    ? trim(system('tmux -V'))           : 'not found')
  echo 'LSP log  : ' . expand('~/.vim/vim-lsp.log')
  echo '=================================='
endfunction

function! s:CleanBuffers()
  let l:cur = bufnr('%')
  let l:count = 0
  for i in range(1, bufnr('$'))
    if buflisted(i) && i != l:cur
      silent! execute 'bdelete ' . i
      let l:count += 1
    endif
  endfor
  echom 'Closed ' . l:count . ' buffer(s)'
endfunction

function! s:FindProjectRoot()
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

function! s:RenameFile(new)
  let l:old = expand('%:p')
  if rename(l:old, a:new) == 0
    execute 'edit ' . fnameescape(a:new)
    execute 'bdelete ' . fnameescape(l:old)
    echom 'Renamed to ' . a:new
  else
    echohl ErrorMsg | echom 'Rename failed' | echohl None
  endif
endfunction

command! VimInfo         call s:VimInfo()
command! CleanBuffers    call s:CleanBuffers()
command! FindProjectRoot call s:FindProjectRoot()
command! ReloadConfig    source $MYVIMRC | echom 'Config reloaded'
command! -nargs=1 -complete=file Rename call s:RenameFile(<q-args>)

" -----------------------------------------------------------------------------
" 11. tmux integration
" -----------------------------------------------------------------------------
" Automatically rename tmux window to current filename
if exists('$TMUX')
  augroup TmuxRename
    autocmd!
    autocmd BufEnter * call system(
      \ "tmux rename-window '" . expand('%:t') . "'")
    autocmd VimLeave * call system('tmux set-window-option automatic-rename on')
  augroup END
endif

" -----------------------------------------------------------------------------
" 12. Machine-local overrides
" -----------------------------------------------------------------------------
" Place per-machine settings in ~/.vimrc.local, e.g.:
"   export VIM_THEME=gruvbox          (in shell rc)
"   let g:lsp_settings_filetype_python = ['pyright']
"   let g:dbs['mydb'] = 'postgresql://...'
"   let g:vimspector_configurations = {...}
if filereadable(expand('~/.vimrc.local'))
  source ~/.vimrc.local
endif
