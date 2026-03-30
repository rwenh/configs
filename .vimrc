" =============================================================================
" VIM FULL-SCALE IDE — Vim 9+ / openSUSE — 2026 edition
" Philosophy: Fast. Self-contained. Professional. No AI. No cloud. No bloat.
" 40+ languages | coc.nvim LSP | DAP | Git | REST | SQL | Markdown | tmux
" Lazy-loaded per filetype — startup < 80ms regardless of stack size.
" 2026-upgrade: vim-matchup · vim-asterisk · vim-easy-align · gutentags+rg
" 2026-final:   vim-indent-guides (replaces indentLine) · vim-textobj-user/indent/comment
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
" Migration from vim-lsp: remove old plugin dirs once coc.nvim is installed:
"   rm -rf ~/.vim/plugged/vim-lsp ~/.vim/plugged/vim-lsp-settings
"   rm -rf ~/.vim/plugged/asyncomplete.vim ~/.vim/plugged/asyncomplete-lsp.vim
"   rm -rf ~/.vim/plugged/asyncomplete-ultisnips.vim ~/.vim/plugged/auto-pairs
"
" coc.nvim requires Node.js >= 18. Install extensions once with:
"   :CocInstall coc-pyright coc-rust-analyzer coc-go coc-tsserver coc-clangd
"   :CocInstall coc-json coc-yaml coc-html coc-css coc-sh coc-snippets
"   :CocInstall coc-vimlsp coc-kotlin coc-solargraph coc-docker coc-terraform
"
" 2026 upgrade — new plugins (auto-installed on first launch):
"   vim-matchup        : replaces matchit/matchparen — % g% [% ]% i% a%
"   vim-asterisk       : non-jumpy * / # / g* / g# with visual star
"   vim-easy-align     : ga interactive alignment (replaces manual :Tab use)
"   gutentags          : now uses rg as file lister + aggressive --exclude list
" 2026 final upgrade:
"   vim-indent-guides  : replaces indentLine — highlight-based, no conceal issues
"   vim-textobj-user   : base for custom text objects
"   vim-textobj-indent : ii/ai — operate on indent block (dii, vii, cai, etc.)
"   vim-textobj-comment: ic/ac — operate on comment block (dic, vac, etc.)
" -----------------------------------------------------------------------------

" -----------------------------------------------------------------------------
" 0. Must be first
" -----------------------------------------------------------------------------
set nocompatible
if has('vim_starting')
  set encoding=utf-8
endif
scriptencoding utf-8

set nomodeline modelines=0 secure

" -----------------------------------------------------------------------------
" 1. Performance
" -----------------------------------------------------------------------------
set regexpengine=0 synmaxcol=300 lazyredraw
set updatetime=150 redrawtime=1500
set ttimeoutlen=10 timeoutlen=500

if has('mouse_sgr') | set ttymouse=sgr | endif
set mouse=a

" -----------------------------------------------------------------------------
" 2. Core Settings
" -----------------------------------------------------------------------------
filetype plugin indent on
syntax enable

set fileencodings=utf-8,ucs-bom,latin1

" Editing
set backspace=indent,eol,start history=5000 undolevels=2000 undoreload=10000
set nrformats-=octal virtualedit=block nojoinspaces

" Display
set number relativenumber cursorline laststatus=2 scrolloff=8 sidescrolloff=5
set showcmd ruler showmatch matchtime=2 display+=lastline
set signcolumn=yes pumheight=14 cmdheight=1 noshowmode shortmess+=acFI

" Search
set incsearch hlsearch ignorecase smartcase wrapscan
if executable('rg')
  set grepprg=rg\ --vimgrep\ --smart-case\ --follow grepformat=%f:%l:%c:%m
endif

" Indentation defaults (vim-sleuth overrides per project)
set autoindent expandtab tabstop=4 softtabstop=4 shiftwidth=4 shiftround smarttab

" No auto-continuation of comment leaders
augroup FormatOptions
  autocmd!
  autocmd FileType * setlocal formatoptions-=cro
  autocmd FileType gitcommit setlocal formatoptions+=cro
augroup END

" Buffers / windows
set hidden switchbuf=useopen,usetab splitbelow splitright winminheight=0 winminwidth=0

" Command-line completion
set wildmenu wildmode=longest:full,full
set wildignore+=*.o,*~,*.pyc,*.class,*.jar
set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*/node_modules/*,*/bower_components/*
set wildignore+=*.DS_Store,*.log,*.tmp

" coc.nvim owns completion — keep native opts minimal
set completeopt=menuone,noselect,noinsert

" Directories
for s:d in ['swap', 'backup', 'undo', 'tags', 'sessions', 'fzf-history', 'db_ui']
  if !isdirectory(expand('~/.vim/' . s:d))
    call mkdir(expand('~/.vim/' . s:d), 'p', 0700)
  endif
endfor

set swapfile   directory=~/.vim/swap//
" backup=keep a tilde backup after write; nowritebackup=coc.nvim requirement
" (coc needs to read the file mid-write; writebackup would break that)
set backup     nowritebackup backupdir=~/.vim/backup//
set autoread

" Persistent undo
if has('persistent_undo')
  set undofile undodir=~/.vim/undo
endif

" Folding (coc-based folding off; using indent folding at 99 = open by default)
set foldmethod=indent foldlevel=99 foldlevelstart=99

" Visible whitespace
set list listchars=tab:▸\ ,trail:·,extends:→,precedes:←,nbsp:⦸
set fillchars=vert:│,fold:─

" Spell
set spelllang=en_us

" True colour
if has('termguicolors') && ($COLORTERM =~# 'truecolor\|24bit')
  set termguicolors
endif

" GUI
if has('gui_running')
  set guifont=JetBrainsMono\ Nerd\ Font\ Mono:h12 guioptions=ac columns=120 lines=35
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
" 3. Plugin Manager — vim-plug (robust bootstrap: curl → wget → abort)
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
    echohl ErrorMsg | echom 'vim-plug bootstrap failed: ' . v:exception | echohl None
    return 0
  endtry
endfunction

if !s:EnsureVimPlug() | finish | endif

call plug#begin(expand('~/.vim/plugged'))

" ---------------------------------------------------------------------------
" APPEARANCE
" Set $VIM_THEME in shell or ~/.vimrc.local
" Options: catppuccin_mocha | catppuccin_frappe | gruvbox | onedark
" ---------------------------------------------------------------------------
Plug 'catppuccin/vim', { 'as': 'catppuccin' }
Plug 'morhetz/gruvbox'
Plug 'joshdick/onedark.vim'

Plug 'itchyny/lightline.vim'
Plug 'mengelbrecht/lightline-bufferline'
Plug 'ryanoasis/vim-devicons'

" ---------------------------------------------------------------------------
" FILE EXPLORER — fern (async, Vim 9 native, replaces netrw silently)
" ---------------------------------------------------------------------------
Plug 'lambdalisue/fern.vim',                   { 'on': 'Fern' }
Plug 'lambdalisue/fern-git-status.vim',        { 'on': 'Fern' }
Plug 'lambdalisue/nerdfont.vim',               { 'on': 'Fern' }
Plug 'lambdalisue/fern-renderer-nerdfont.vim', { 'on': 'Fern' }
Plug 'LumaKernel/fern-mapping-fzf.vim',        { 'on': 'Fern' }
Plug 'lambdalisue/vim-fern-hijack'

" ---------------------------------------------------------------------------
" NAVIGATION
" ---------------------------------------------------------------------------
Plug 'junegunn/fzf',    { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'christoomey/vim-tmux-navigator'
Plug 'preservim/tagbar',  { 'on': 'TagbarToggle' }
Plug 'ludovicchabant/vim-gutentags'
Plug 'mhinz/vim-startify'
" vim-matchup: replaces matchit/matchparen — handles Rust lifetimes, JSX tags,
" TS generics, HTML nesting. %, g%, [%, ]% all become context-aware.
Plug 'andymass/vim-matchup'

" ---------------------------------------------------------------------------
" LSP / COMPLETION — coc.nvim
" Replaces: vim-lsp, vim-lsp-settings, asyncomplete, asyncomplete-lsp,
"           asyncomplete-ultisnips. One plugin, Node-powered, battle-tested.
" ---------------------------------------------------------------------------
Plug 'neoclide/coc.nvim', { 'branch': 'release' }

" ---------------------------------------------------------------------------
" DAP — vimspector
" Config lives in .vimspector.json at project root.
" Gadgets: debugpy | vscode-go | CodeLLDB | vscode-node | java/kotlin adapters
" ---------------------------------------------------------------------------
Plug 'puremourning/vimspector'

" ---------------------------------------------------------------------------
" EDITING — core power tools
" ---------------------------------------------------------------------------
Plug 'tpope/vim-commentary'       " gc to comment
Plug 'tpope/vim-surround'         " cs/ds/ys surround motions
Plug 'tpope/vim-repeat'           " . repeats plugin actions
Plug 'tpope/vim-unimpaired'       " [b ]b [q ]q and friends
Plug 'tpope/vim-sleuth'           " auto-detect indent per project
Plug 'cohama/lexima.vim'          " bracket/quote auto-close (replaces auto-pairs)
Plug 'wellle/targets.vim'         " extra text objects: cin, da,
Plug 'kana/vim-textobj-user'                " required base for custom text objects
Plug 'kana/vim-textobj-indent'             " ii/ai — indent-level text objects
Plug 'glts/vim-textobj-comment'            " ic/ac — comment text objects
Plug 'matze/vim-move'             " Alt+j/k move lines/blocks
Plug 'mbbill/undotree',           { 'on': 'UndotreeToggle' }
Plug 'jdhao/better-escape.vim'    " jk/kj → Esc in insert mode
" Note: vim-visual-multi can conflict with coc.nvim insert-mode mappings.
" If completion behaves oddly in multi-cursor mode, check :verbose imap <Tab>
Plug 'mg979/vim-visual-multi'     " multi-cursor Ctrl+N
Plug 'psliwka/vim-smoothie'       " smooth C-d/C-u scrolling
Plug 'nathanaelkane/vim-indent-guides'  " indent guides — no conceal tricks
Plug 'wellle/context.vim'         " sticky function context at top
" vim-asterisk: non-jumpy * / # / g* / g# with visual star support.
" Replaces manual *zzzv/#zzzv mappings with proper stay-in-place behaviour.
Plug 'haya14busa/vim-asterisk'
" vim-easy-align: ga interactive alignment — replaces tabular for daily use.
" Works on YAML, JSON, SQL, Markdown tables, struct fields, assignment blocks.
Plug 'junegunn/vim-easy-align'

" ---------------------------------------------------------------------------
" SNIPPETS — vim-vsnip (pure Vimscript) integrates with coc-snippets
" ---------------------------------------------------------------------------
Plug 'hrsh7th/vim-vsnip'          " pure Vimscript, no Python needed
Plug 'honza/vim-snippets'

" ---------------------------------------------------------------------------
" LINTING / FORMATTING — ALE kept for linters-only
" coc.nvim handles: diagnostics, signature help, inlay hints, formatting
" ALE handles: shellcheck (sh), vint (vim) — no LSP equivalent available
" ALE fixers: black/isort/prettier/shfmt/rustfmt/gofmt/clang-format etc.
" ---------------------------------------------------------------------------
Plug 'dense-analysis/ale'

" ---------------------------------------------------------------------------
" GIT
" ---------------------------------------------------------------------------
Plug 'tpope/vim-fugitive'         " :Git everything
Plug 'tpope/vim-rhubarb'          " GitHub :GBrowse
Plug 'airblade/vim-gitgutter'     " live hunk signs
Plug 'junegunn/gv.vim'            " beautiful git log :GV
Plug 'whiteinge/diffconflicts'    " 2-way merge conflict resolution

" ---------------------------------------------------------------------------
" LANGUAGE SUPPORT — lazy by filetype
" vim-polyglot covers ~70 languages; individual plugins fill gaps.
" ---------------------------------------------------------------------------
Plug 'sheerun/vim-polyglot'

" Systems / compiled
Plug 'rust-lang/rust.vim',        { 'for': 'rust' }
Plug 'fatih/vim-go',              { 'for': 'go', 'do': ':GoUpdateBinaries' }
Plug 'rhysd/vim-llvm',            { 'for': ['c', 'cpp'] }
Plug 'ziglang/zig.vim',           { 'for': 'zig' }

" Web
Plug 'pangloss/vim-javascript',       { 'for': ['javascript', 'javascriptreact'] }
Plug 'leafgarland/typescript-vim',    { 'for': ['typescript', 'typescriptreact'] }
Plug 'maxmellon/vim-jsx-pretty',      { 'for': ['javascriptreact', 'typescriptreact'] }
Plug 'jparise/vim-graphql',           { 'for': 'graphql' }
Plug 'mustache/vim-mustache-handlebars', { 'for': ['html.handlebars', 'mustache'] }

" Data / scripting
Plug 'Vimjas/vim-python-pep8-indent', { 'for': 'python' }
Plug 'vim-ruby/vim-ruby',             { 'for': 'ruby' }
Plug 'tpope/vim-rails',               { 'for': 'ruby' }
Plug 'udalov/kotlin-vim',             { 'for': 'kotlin' }

" Legacy / scientific
Plug 'vim-scripts/fortran.vim',   { 'for': 'fortran' }
Plug 'suoto/vim-hdl',             { 'for': ['vhdl', 'verilog'] }

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

" Markup / docs
Plug 'godlygeek/tabular'
Plug 'preservim/vim-markdown',    { 'for': 'markdown' }
Plug 'iamcco/markdown-preview.nvim', { 'for': 'markdown', 'do': { -> mkdp#util#install() } }
Plug 'lervag/vimtex',             { 'for': 'tex' }
Plug 'vim-scripts/xml.vim',       { 'for': ['xml', 'html'] }

" SQL
Plug 'tpope/vim-dadbod'
Plug 'kristijanhusak/vim-dadbod-ui'
Plug 'kristijanhusak/vim-dadbod-completion'

" ---------------------------------------------------------------------------
" TERMINAL
" ---------------------------------------------------------------------------
Plug 'voldikss/vim-floaterm'

" ---------------------------------------------------------------------------
" REST CLIENT
" ---------------------------------------------------------------------------
Plug 'diepm/vim-rest-console'
Plug 'baverman/vial-http', { 'for': 'http' }

" ---------------------------------------------------------------------------
" TEST RUNNER
" ---------------------------------------------------------------------------
Plug 'vim-test/vim-test'
Plug 'tpope/vim-dispatch'

" ---------------------------------------------------------------------------
" SESSION / UTILITIES
" ---------------------------------------------------------------------------
Plug 'tpope/vim-obsession'
Plug 'editorconfig/editorconfig-vim'
Plug 'liuchengxu/vim-which-key'
Plug 'szw/vim-maximizer',   { 'on': 'MaximizerToggle' }
Plug 'tpope/vim-eunuch'

call plug#end()

" Auto-install any missing plugins on startup (catches newly added entries)
augroup PlugAutoInstall
  autocmd!
  autocmd VimEnter *
    \ let s:missing = filter(values(g:plugs), '!isdirectory(v:val.dir)') |
    \ if len(s:missing) |
    \   echom len(s:missing) . ' plugin(s) missing — installing…' |
    \   PlugInstall --sync |
    \ endif
augroup END

" -----------------------------------------------------------------------------
" 4. Colorscheme
" -----------------------------------------------------------------------------
" Set $VIM_THEME in shell or ~/.vimrc.local
" Available: catppuccin_mocha | catppuccin_frappe | gruvbox | onedark
let s:theme = !empty($VIM_THEME) ? $VIM_THEME : 'catppuccin_mocha'

function! s:ApplyTheme()
  set background=dark
  try
    execute 'colorscheme ' . s:theme
  catch
    colorscheme desert
    echom 'Theme "' . s:theme . '" not found — fell back to desert'
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
nnoremap <leader>bd    :bdelete<CR>
nnoremap <leader>bD    :bdelete!<CR>
nnoremap <leader>bn    :bnext<CR>
nnoremap <leader>bp    :bprevious<CR>
nnoremap <leader>bC    :CleanBuffers<CR>

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
" vim-asterisk: stay-in-place * / # — cursor doesn't jump on first match.
" z* / z# = stay put, gz* / gz# = stay put + whole-word off (useful in visual)
map *  <Plug>(asterisk-z*)zv
map #  <Plug>(asterisk-z#)zv
map g* <Plug>(asterisk-gz*)zv
map g# <Plug>(asterisk-gz#)zv
" Keep visual * on selection too
xmap *  <Plug>(asterisk-z*)zv
xmap #  <Plug>(asterisk-z#)zv
nnoremap <C-d> <C-d>zz
nnoremap <C-u> <C-u>zz

" --- Visual ---
vnoremap < <gv
vnoremap > >gv
vnoremap . :norm .<CR>

" --- Yank / paste ---
nnoremap Y         y$
nnoremap <leader>p "+p
nnoremap <leader>P "+P
vnoremap <leader>y "+y
nnoremap <leader>yy "+yy
vnoremap <leader>vp "_dP

" --- Alignment (vim-easy-align) ---
" ga in normal/visual then motion or text object — interactive, live preview
" Examples: gaip=  (align paragraph on =)   ga2,  (align 2nd comma in line)
"           vipga= (visual select then align) gaip* (align all = in paragraph)
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

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
nnoremap <silent> <leader>e :Fern . -drawer -reveal=% -toggle<CR>
nnoremap <silent> <leader>E :Fern . -drawer -reveal=%<CR>

" --- Terminal ---
nnoremap <silent> <C-\>  :FloatermToggle<CR>
tnoremap <silent> <C-\>  <C-\><C-n>:FloatermToggle<CR>
nnoremap <leader>tn :FloatermNew<CR>
nnoremap <leader>tk :FloatermKill<CR>
nnoremap <leader>tl :FloatermNext<CR>
nnoremap <leader>th :FloatermPrev<CR>
vnoremap <leader>ts :FloatermSend<CR>

" --- coc.nvim LSP ---
" TAB: cycle completion popup forward; trigger refresh if cursor follows a word
inoremap <silent><expr> <TAB>
  \ coc#pum#visible() ? coc#pum#next(1) :
  \ col('.') > 1 && getline('.')[col('.')-2] !~# '\s' ? coc#refresh() : "\<Tab>"
inoremap <expr><S-TAB> coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"
" CR confirms selection without adding an extra undo break
inoremap <silent><expr> <CR>
  \ coc#pum#visible() ? coc#pum#confirm() : "\<C-g>u\<CR>\<C-r>=coc#on_enter()\<CR>"
" Force refresh (replaces asyncomplete_force_refresh)
inoremap <silent><expr> <C-Space> coc#refresh()

" Navigation
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gD <Plug>(coc-declaration)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
" Hover (K) — show docs or fallback to keywordprg
nnoremap <silent> K :call CocActionAsync('doHover')<CR>

" LSP actions
nmap <silent> <leader>rn <Plug>(coc-rename)
nmap <silent> <leader>ca <Plug>(coc-codeaction-cursor)
nmap <silent> <leader>cf <Plug>(coc-format)
nmap <silent> <leader>cs :CocList outline<CR>
nmap <silent> <leader>cS :CocList -I symbols<CR>
" Diagnostics (replaces ]e [e ]w [w from vim-lsp)
nmap <silent> ]g <Plug>(coc-diagnostic-next)
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]e <Plug>(coc-diagnostic-next-error)
nmap <silent> [e <Plug>(coc-diagnostic-prev-error)
" Diagnostic list
nnoremap <silent> <leader>cd :CocList diagnostics<CR>
" Signature help in insert mode
inoremap <silent> <C-k> <C-r>=CocActionAsync('showSignatureHelp')<CR>

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
nnoremap <leader>ui :IndentGuidesToggle<CR>
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

" which-key popup — keep last among leader mappings
nnoremap <silent> <leader> :<c-u>WhichKey '<Space>'<CR>

" Run / compile / build / test — F5–F9
nnoremap <silent> <F5> :call <SID>RunAction('run')<CR>
nnoremap <silent> <F6> :call <SID>RunAction('compile')<CR>
nnoremap <silent> <F7> :call <SID>RunAction('build')<CR>
nnoremap <silent> <F9> :call <SID>RunAction('test')<CR>

" -----------------------------------------------------------------------------
" 6. Plugin Configuration
" -----------------------------------------------------------------------------

" --- context.vim — disable on large files to avoid scroll lag ---
let g:context_max_height = 5
autocmd BufReadPre * if getfsize(expand('<afile>')) > 500000 | let b:context_enabled = 0 | endif

" --- better-escape ---
let g:better_escape_shortcut = ['jk', 'kj']
let g:better_escape_interval = 200

" --- Lightline ---
let s:ll_theme_map = {
  \ 'catppuccin_mocha':  'catppuccin_mocha',
  \ 'catppuccin_frappe': 'catppuccin_frappe',
  \ 'gruvbox':           'gruvbox',
  \ 'onedark':           'onedark',
  \ }

function! LightlineCocStatus() abort
  return get(g:, 'coc_status', '')
endfunction

let g:lightline = {
  \ 'colorscheme': get(s:ll_theme_map, s:theme, 'one'),
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

" Refresh lightline when coc status changes
augroup LightlineCoc
  autocmd!
  autocmd User CocStatusChange,CocDiagnosticChange
    \ silent call lightline#update()
augroup END

" --- coc.nvim ---
" Diagnostic signs (matches former vim-lsp sign set)
call coc#config('diagnostic.errorSign',       '󰅚 ')
call coc#config('diagnostic.warningSign',     '󰀪 ')
call coc#config('diagnostic.infoSign',        '󰋽 ')
call coc#config('diagnostic.hintSign',        '󰌶 ')
call coc#config('diagnostic.virtualText',     v:false)
call coc#config('diagnostic.displayByAle',    v:false)
call coc#config('signature.enable',           v:true)
call coc#config('hover.autoHide',             v:true)
" vsnip integration
call coc#config('snippets.enable', v:true)
" Format on save (coc handles the languages ALE used to fix)
call coc#config('coc.preferences.formatOnSaveFiletypes', [
  \ 'python', 'javascript', 'typescript', 'javascriptreact', 'typescriptreact',
  \ 'go', 'rust', 'sh', 'c', 'cpp', 'kotlin', 'ruby',
  \ 'yaml', 'json', 'html', 'css', 'scss', 'zig',
  \ ])

" --- Fern ---
let g:fern#renderer       = 'nerdfont'
let g:fern#default_hidden = 1
let g:fern#default_exclude =
  \ '^\%(\.git\|__pycache__\|node_modules\|\.DS_Store\|\.cache\|dist\|build\)$'

let g:fern_git_status#disable_ignored   = 1
let g:fern_git_status#disable_untracked = 0
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

" --- FZF ---
let g:fzf_layout      = { 'window': { 'width': 0.92, 'height': 0.88, 'rounded': v:true } }
let g:fzf_history_dir = expand('~/.vim/fzf-history')

" Catppuccin Mocha palette
let $FZF_DEFAULT_OPTS =
  \ '--layout=reverse --border=rounded --info=inline ' .
  \ '--color=bg+:#313244,bg:#1e1e2e,spinner:#f5e0dc,hl:#f38ba8 ' .
  \ '--color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc ' .
  \ '--color=marker:#f5e0dc,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8'

if executable('bat')
  let $FZF_DEFAULT_OPTS .= ' --preview "bat --style=numbers --color=always {}"'
endif

" --- ALE (linters-only — coc.nvim owns diagnostics for LSP languages) ---
let g:ale_disable_lsp          = 1   " critical: no LSP conflict with coc
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
" ALE fixers: run on save for all languages coc format-on-save also covers.
" They are complementary — coc formats via LSP, ALE fixes via CLI tools.
" If you see double-formatting, disable the fixers for that filetype here.
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

" --- Vimspector (DAP) ---
let g:vimspector_enable_mappings = 'NONE'
let g:vimspector_sign_priority = {
  \ 'vimspectorBP':         20,
  \ 'vimspectorBPCond':     19,
  \ 'vimspectorBPDisabled': 18,
  \ 'vimspectorPC':         999,
  \ }

" --- UltiSnips ---
let g:UltiSnipsExpandTrigger       = '<C-e>'
let g:UltiSnipsJumpForwardTrigger  = '<C-l>'
let g:UltiSnipsJumpBackwardTrigger = '<C-b>'
let g:UltiSnipsEditSplit           = 'vertical'

" --- vim-go ---
let g:go_fmt_command            = 'goimports'
let g:go_highlight_types        = 1
let g:go_highlight_fields       = 1
let g:go_highlight_functions    = 1
let g:go_highlight_operators    = 1
let g:go_def_mapping_enabled    = 0   " coc handles gd
let g:go_doc_keywordprg_enabled = 0   " coc handles K

" --- rust.vim ---
let g:rustfmt_autosave = 0   " ALE/coc handles formatting

" --- lexima.vim ---
" Works out of the box. The one rule to add: don't double-close inside coc popup.
" lexima is rule-based and does not fight CR/TAB the way auto-pairs did.
let g:lexima_enable_basic_rules   = 1
let g:lexima_enable_newline_rules = 1
let g:lexima_enable_endwise_rules = 1

" --- Floaterm ---
let g:floaterm_width       = 0.88
let g:floaterm_height      = 0.88
let g:floaterm_autoclose   = 1
let g:floaterm_position    = 'center'
let g:floaterm_borderchars = '─│─│╭╮╯╰'
let g:floaterm_title       = '  terminal ($1/$2) '
let g:floaterm_wintype     = 'float'

" --- vim-tmux-navigator ---
let g:tmux_navigator_disable_when_zoomed = 1
let g:tmux_navigator_save_on_switch      = 2

" --- vim-smoothie ---
let g:smoothie_speed_constant_factor = 30
let g:smoothie_speed_linear_factor   = 30

" --- vim-indent-guides ---
" Uses actual highlight groups — no conceal tricks, no slowdown on large files.
" Even columns get a subtle alternate background; odd columns stay as-is.
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

" --- context.vim (sticky scroll) ---
let g:context_enabled          = 1
let g:context_max_height       = 5
let g:context_filetype_exclude = ['fern', 'startify', 'help', 'dbui']

" --- Gitgutter ---
let g:gitgutter_enabled       = 1
let g:gitgutter_map_keys      = 0
let g:gitgutter_sign_added    = '▎'
let g:gitgutter_sign_modified = '▎'
let g:gitgutter_sign_removed  = '▎'

" --- Gutentags ---
let g:gutentags_cache_dir                 = expand('~/.vim/tags')
let g:gutentags_generate_on_new           = 1
let g:gutentags_generate_on_missing       = 1
let g:gutentags_generate_on_write         = 1
let g:gutentags_ctags_extra_args          = [
  \ '--tag-relative=yes', '--fields=+ailmnS',
  \ '--exclude=.git', '--exclude=.hg', '--exclude=.svn',
  \ '--exclude=node_modules', '--exclude=bower_components',
  \ '--exclude=dist', '--exclude=build', '--exclude=out',
  \ '--exclude=target', '--exclude=__pycache__', '--exclude=.cache',
  \ '--exclude=*.min.js', '--exclude=*.min.css',
  \ ]
" Use ripgrep as file lister if available — dramatically faster on monorepos
if executable('rg')
  let g:gutentags_file_list_command = 'rg --files --follow'
endif
let g:gutentags_add_default_project_roots = 0
let g:gutentags_project_root              =
  \ ['.git', '.svn', '.hg', 'package.json', 'Cargo.toml', 'go.mod', 'pyproject.toml']

" --- vim-move ---
let g:move_key_modifier = 'A'

" --- vim-matchup ---
" Disables the default matchit and matchparen — matchup supersedes both.
" offscreen_position: show match in statusline when it's off-screen.
let g:matchup_matchparen_offscreen   = { 'method': 'status_manual' }
let g:matchup_matchparen_deferred    = 1   " defer highlights for speed
let g:matchup_matchparen_hi_surround_always = 1
let g:matchup_motion_enabled         = 1   " % g% [% ]% work everywhere
let g:matchup_text_obj_enabled       = 1   " i% a% text objects
" Don't fight coc.nvim's popup
let g:matchup_matchparen_pumvisible  = 0

" --- vim-visual-multi ---
let g:VM_maps                       = {}
let g:VM_maps['Find Under']         = '<C-n>'
let g:VM_maps['Find Subword Under'] = '<C-n>'
let g:VM_maps['Select All']         = '<leader>ma'
let g:VM_maps['Skip Region']        = '<C-x>'

" --- vim-asterisk ---
" is_animated: flash the match highlight briefly on land (visual feedback)
let g:asterisk#keeppos = 1   " cursor column preserved after z* jump

" --- vim-textobj-user / vim-textobj-indent / vim-textobj-comment ---
" Provided text objects (all work with d/c/y/v operators):
"   ii / ai  — inner / around indent block (same level / including blank lines)
"   ic / ac  — inner / around comment (line or block comment)
" No config required — vim-textobj-user wires them automatically on load.

" --- vim-easy-align ---
" Custom delimiters beyond the built-in set (=, :, |, #, &, ,, .)
" Accessible interactively via: ga <motion> <key>
let g:easy_align_delimiters = {
  \ '>':  { 'pattern': '>>\|=>\|>' },
  \ '/':  { 'pattern': '//\+\|/\*\|\*/', 'delimiter_align': 'l', 'ignore_groups': [] },
  \ '#':  { 'pattern': '#',  'delimiter_align': 'l', 'ignore_groups': ['String'] },
  \ ']':  { 'pattern': '[[\]]', 'left_margin': 0, 'right_margin': 0, 'stick_to_left': 0 },
  \ ')':  { 'pattern': '[()]', 'left_margin': 0, 'right_margin': 0, 'stick_to_left': 0 },
  \ 'd':  { 'pattern': '\s\+\ze\S', 'left_margin': 0, 'right_margin': 0 },
  \ }

" --- vim-test ---
let g:test#strategy          = 'floaterm'
let g:test#python#runner     = 'pytest'
let g:test#javascript#runner = 'jest'
let g:test#go#runner         = 'gotest'
let g:test#rust#runner       = 'cargotest'

" --- REST console ---
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
let g:mkdp_auto_close = 1
let g:mkdp_theme      = 'dark'
let g:mkdp_browser    = ''

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
  \ { 'type': 'sessions',  'header': ['   Sessions']     },
  \ { 'type': 'files',     'header': ['   Recent files'] },
  \ { 'type': 'dir',       'header': ['   ' . getcwd()]  },
  \ { 'type': 'bookmarks', 'header': ['   Bookmarks']    },
  \ ]
let g:startify_bookmarks = [
  \ { 'v': '~/.vimrc'  },
  \ { 'z': '~/.zshrc'  },
  \ { 'b': '~/.bashrc' },
  \ ]

" --- which-key ---
call which_key#register('<Space>', "g:which_key_map")
let g:which_key_map             = {}
let g:which_key_map['<CR>']     = 'reload config'
let g:which_key_map['<Tab>']    = 'last buffer'
let g:which_key_map.e           = 'file explorer'
let g:which_key_map.E           = 'reveal in tree'
let g:which_key_map.U           = 'undo tree'
let g:which_key_map.w           = { 'name': '+window'    }
let g:which_key_map.b           = { 'name': '+buffer'    }
let g:which_key_map.f           = { 'name': '+find/fzf'  }
let g:which_key_map.g           = { 'name': '+git'       }
let g:which_key_map.h           = { 'name': '+hunk'      }
let g:which_key_map.t           = { 'name': '+terminal'  }
let g:which_key_map.u           = { 'name': '+toggle'    }
let g:which_key_map.s           = { 'name': '+search'    }
let g:which_key_map.y           = { 'name': '+yank'      }
let g:which_key_map.c           = { 'name': '+lsp/coc'   }
let g:which_key_map.l           = { 'name': '+loclist'   }
let g:which_key_map.r           = { 'name': '+rename'    }
let g:which_key_map.d           = { 'name': '+debug/dap' }
let g:which_key_map.D           = { 'name': '+database'  }
let g:which_key_map.S           = { 'name': '+session'   }
let g:which_key_map.T           = { 'name': '+test'      }
let g:which_key_map.R           = { 'name': '+rest'      }
let g:which_key_map.m           = { 'name': '+multicursor' }
let g:which_key_map.p           = { 'name': '+markdown'  }
let g:which_key_map.a           = 'align (easy-align ga+motion)'

" -----------------------------------------------------------------------------
" 7. Autocommands
" -----------------------------------------------------------------------------
function! s:HandleLargeFile()
  if getfsize(expand('%')) > 10485760
    setlocal eventignore+=FileType bufhidden=unload undolevels=-1
    setlocal noundofile noswapfile syntax=off nowrap nocursorline norelativenumber
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
  autocmd BufReadPre          * call s:HandleLargeFile()
  autocmd BufWritePre         * call s:StripTrailing()
  autocmd BufWritePre         * call s:MkdirOnSave()
  autocmd BufReadPost         * call s:RestoreCursor()
  autocmd TextYankPost        * silent! call s:FlashYank()
  autocmd FocusGained,BufEnter * silent! checktime
  autocmd VimResized          * wincmd =
  autocmd TerminalOpen        * setlocal nonumber norelativenumber signcolumn=no
  autocmd FileType sql        setlocal omnifunc=vim_dadbod_completion#omni
augroup END

" -----------------------------------------------------------------------------
" 8. Filetype indent — 40+ languages
" -----------------------------------------------------------------------------
augroup FileTypeIndent
  autocmd!
  " Python
  autocmd FileType python                           setlocal ts=4 sw=4 expandtab
  " Web
  autocmd FileType javascript,typescript            setlocal ts=2 sw=2 expandtab
  autocmd FileType javascriptreact,typescriptreact  setlocal ts=2 sw=2 expandtab
  autocmd FileType html,css,scss,sass,less          setlocal ts=2 sw=2 expandtab
  autocmd FileType json,jsonc                       setlocal ts=2 sw=2 expandtab
  autocmd FileType graphql                          setlocal ts=2 sw=2 expandtab
  " Systems
  autocmd FileType go,make                          setlocal ts=4 sw=4 noexpandtab
  autocmd FileType c,cpp                            setlocal ts=4 sw=4 expandtab
  autocmd FileType rust                             setlocal ts=4 sw=4 expandtab
  autocmd FileType zig                              setlocal ts=4 sw=4 expandtab
  autocmd FileType java,kotlin                      setlocal ts=4 sw=4 expandtab
  " Scripting
  autocmd FileType lua,vim,ruby                     setlocal ts=2 sw=2 expandtab
  autocmd FileType sh,zsh,bash,fish                 setlocal ts=2 sw=2 expandtab
  autocmd FileType perl,php                         setlocal ts=4 sw=4 expandtab
  autocmd FileType elixir,erlang                    setlocal ts=2 sw=2 expandtab
  autocmd FileType haskell,ocaml,elm                setlocal ts=2 sw=2 expandtab
  autocmd FileType scala,clojure,lisp               setlocal ts=2 sw=2 expandtab
  autocmd FileType fsharp,csharp                    setlocal ts=4 sw=4 expandtab
  " Data / config
  autocmd FileType yaml,toml                        setlocal ts=2 sw=2 expandtab
  autocmd FileType xml,xhtml                        setlocal ts=2 sw=2 expandtab
  autocmd FileType sql                              setlocal ts=2 sw=2 expandtab
  autocmd FileType csv                              setlocal ts=4 sw=4 noexpandtab
  autocmd FileType terraform                        setlocal ts=2 sw=2 expandtab
  autocmd FileType dockerfile                       setlocal ts=2 sw=2 expandtab
  " Legacy / scientific
  autocmd FileType fortran                          setlocal ts=3 sw=3 expandtab
  autocmd FileType cobol                            setlocal ts=4 sw=4 noexpandtab
  autocmd FileType vhdl,verilog                     setlocal ts=2 sw=2 expandtab
  " Docs
  autocmd FileType markdown,text
    \ setlocal ts=4 sw=4 expandtab spell textwidth=80 wrap linebreak
  autocmd FileType gitcommit                        setlocal spell textwidth=72
  autocmd FileType tex                              setlocal ts=2 sw=2 expandtab spell
  autocmd FileType rst                              setlocal ts=3 sw=3 expandtab spell
  " Niche but commonly needed
  autocmd FileType swift                            setlocal ts=4 sw=4 expandtab
  autocmd FileType dart                             setlocal ts=2 sw=2 expandtab
  autocmd FileType r,rmd                            setlocal ts=2 sw=2 expandtab
  autocmd FileType julia                            setlocal ts=4 sw=4 expandtab
  autocmd FileType nim                              setlocal ts=2 sw=2 expandtab
  autocmd FileType crystal                          setlocal ts=2 sw=2 expandtab
  autocmd FileType d                                setlocal ts=4 sw=4 expandtab
augroup END

" -----------------------------------------------------------------------------
" 9. Terminal execution — F5/F6/F7/F9 run/compile/build/test
" -----------------------------------------------------------------------------
let s:runners = {
  \ 'python':     { 'run': 'python3 "{filepath}"',       'test': 'python3 -m pytest "{dirname}"' },
  \ 'javascript': { 'run': 'node "{filepath}"',           'test': 'npm test' },
  \ 'typescript': { 'run': 'ts-node "{filepath}"',        'test': 'npm test', 'compile': 'tsc "{filepath}"' },
  \ 'go':         { 'run': 'go run "{filepath}"',         'test': 'go test ./...', 'build': 'go build -o "{basename}" "{filepath}"' },
  \ 'rust':       { 'run': './{basename}',                'test': 'cargo test', 'build': 'cargo build --release', 'compile': 'rustc "{filepath}"' },
  \ 'c':          { 'run': './{basename}',                'compile': 'gcc -Wall -O2 -g "{filepath}" -o "{basename}" -lm' },
  \ 'cpp':        { 'run': './{basename}',                'compile': 'g++ -Wall -O2 -g -std=c++17 "{filepath}" -o "{basename}" -lm' },
  \ 'zig':        { 'run': 'zig run "{filepath}"',        'build': 'zig build', 'test': 'zig test "{filepath}"' },
  \ 'kotlin':     { 'compile': 'kotlinc "{filepath}" -include-runtime -d "{basename}.jar"', 'run': 'java -jar "{basename}.jar"' },
  \ 'sh':         { 'run': 'bash "{filepath}"' },
  \ 'lua':        { 'run': 'lua "{filepath}"' },
  \ 'ruby':       { 'run': 'ruby "{filepath}"',           'test': 'ruby -Itest "{filepath}"' },
  \ 'fortran':    { 'compile': 'gfortran -O2 "{filepath}" -o "{basename}"', 'run': './{basename}' },
  \ 'cobol':      { 'compile': 'cobc -x "{filepath}" -o "{basename}"',     'run': './{basename}' },
  \ 'julia':      { 'run': 'julia "{filepath}"' },
  \ 'r':          { 'run': 'Rscript "{filepath}"' },
  \ 'nim':        { 'run': 'nim c -r "{filepath}"',       'build': 'nim c -d:release "{filepath}"' },
  \ 'crystal':    { 'run': 'crystal run "{filepath}"',    'build': 'crystal build --release "{filepath}"' },
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
    \ 'filepath': l:fp,        'dirname':  fnamemodify(l:fp, ':h'),
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
  echo 'coc.nvim : ' . (exists('g:did_coc_loaded') ? 'loaded' : 'NOT loaded')
  echo 'Node     : ' . (executable('node')    ? trim(system('node --version'))    : 'not found')
  echo 'Python3  : ' . (executable('python3') ? trim(system('python3 --version')) : 'not found')
  echo 'Git      : ' . (executable('git')     ? trim(system('git --version'))     : 'not found')
  echo 'rg       : ' . (executable('rg')      ? trim(system('rg --version | head -1')) : 'not found')
  echo 'bat      : ' . (executable('bat')     ? trim(system('bat --version'))     : 'not found (optional, FZF preview)')
  echo 'gopls    : ' . (executable('gopls')   ? 'found' : 'not found')
  echo 'tmux     : ' . (executable('tmux')    ? trim(system('tmux -V'))           : 'not found')
  echo '=================================='
endfunction

function! s:CleanBuffers()
  let l:cur   = bufnr('%')
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
if exists('$TMUX')
  augroup TmuxRename
    autocmd!
    autocmd BufEnter * call system("tmux rename-window '" . expand('%:t') . "'")
    autocmd VimLeave * call system('tmux set-window-option automatic-rename on')
  augroup END
endif

" -----------------------------------------------------------------------------
" 12. Machine-local overrides
" -----------------------------------------------------------------------------
" Place per-machine settings in ~/.vimrc.local, e.g.:
"   export VIM_THEME=gruvbox              (in shell rc)
"   let g:dbs['mydb'] = 'postgresql://...'
"   let g:vimspector_configurations = {...}
"   " Override a coc extension for a filetype:
"   call coc#config('python.pythonPath', '/usr/local/bin/python3')
if filereadable(expand('~/.vimrc.local'))
  source ~/.vimrc.local
endif
