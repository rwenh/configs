" =============================================================================
" Vim Configuration — Vim 8.2+ / openSUSE
" Fully audited. No Neovim. No LSP. No cruft.
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
" Also remove old LSP plugin dirs if they exist:
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
set regexpengine=0        " auto-select NFA/old engine per pattern
set synmaxcol=300         " stop syntax highlight at col 300
set lazyredraw            " don't redraw during macros/registers
set updatetime=150        " faster CursorHold — gitgutter responds quickly
set redrawtime=1500
set ttimeoutlen=10        " near-instant escape key
set timeoutlen=500        " leader chord window

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
set pumheight=12
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
set completeopt=menuone,noselect,noinsert

" File handling
set nobackup
set nowritebackup
set noswapfile
set autoread
" autowrite deliberately OFF

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

" --- Appearance ---
Plug 'lifepillar/vim-solarized8'          " dark/light solarized
Plug 'morhetz/gruvbox'                    " warm retro
Plug 'joshdick/onedark.vim'               " atom one dark
Plug 'vim-airline/vim-airline'            " statusline
Plug 'vim-airline/vim-airline-themes'
Plug 'ryanoasis/vim-devicons'             " filetype icons

" --- Navigation ---
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'preservim/nerdtree', { 'on': ['NERDTreeToggle', 'NERDTreeFind'] }
Plug 'tiagofumo/vim-nerdtree-syntax-highlight', { 'on': 'NERDTreeToggle' }
Plug 'xuyuanp/nerdtree-git-plugin', { 'on': 'NERDTreeToggle' }
Plug 'christoomey/vim-tmux-navigator'
Plug 'preservim/tagbar', { 'on': 'TagbarToggle' }
Plug 'ludovicchabant/vim-gutentags'

" --- Editing ---
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-sleuth'
Plug 'jiangmiao/auto-pairs'
Plug 'wellle/targets.vim'
Plug 'matze/vim-move'
Plug 'mbbill/undotree', { 'on': 'UndotreeToggle' }
Plug 'jdhao/better-escape.vim'

" --- Linting / Formatting — ALE only, no LSP ---
Plug 'dense-analysis/ale'

" --- Snippets ---
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

" --- Git ---
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
Plug 'airblade/vim-gitgutter'

" --- Language support ---
Plug 'sheerun/vim-polyglot'

" --- Terminal ---
Plug 'voldikss/vim-floaterm'

" --- Database ---
Plug 'tpope/vim-dadbod'
Plug 'kristijanhusak/vim-dadbod-ui'
Plug 'kristijanhusak/vim-dadbod-completion'

" --- Session / Start screen ---
Plug 'mhinz/vim-startify'
Plug 'tpope/vim-obsession'

" --- Utilities ---
Plug 'editorconfig/editorconfig-vim'
Plug 'liuchengxu/vim-which-key'

call plug#end()

" Auto-install missing plugins — checks actual dirs so it self-corrects
" without a stamp file. Once installed the dirs exist and this is a no-op.
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
" Change s:theme to: solarized8 | gruvbox | onedark
let s:theme = 'solarized8'

function! s:ApplyTheme()
  try
    set background=dark
    execute 'colorscheme ' . s:theme
  catch
    colorscheme desert
    echom 'Theme not found, falling back to desert'
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

" --- Window navigation ---
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

" --- Window resize ---
nnoremap <M-Up>    :resize +2<CR>
nnoremap <M-Down>  :resize -2<CR>
nnoremap <M-Left>  :vertical resize -2<CR>
nnoremap <M-Right> :vertical resize +2<CR>

" --- Buffers ---
nnoremap <Tab>      :bnext<CR>
nnoremap <S-Tab>    :bprevious<CR>
nnoremap <leader>bd :bdelete<CR>
nnoremap <leader>bD :bdelete!<CR>
nnoremap <leader>bn :bnext<CR>
nnoremap <leader>bp :bprevious<CR>
nnoremap <leader>bC :CleanBuffers<CR>

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
nnoremap <leader>yf :let @+=expand('%:t')<CR>:echom 'Copied filename'<CR>
nnoremap <leader>yF :let @+=expand('%:p')<CR>:echom 'Copied path'<CR>

" --- Move lines ---
nnoremap <silent> <A-j> :m .+1<CR>==
nnoremap <silent> <A-k> :m .-2<CR>==
vnoremap <silent> <A-j> :m '>+1<CR>gv=gv
vnoremap <silent> <A-k> :m '<-2<CR>gv=gv

" --- Duplicate ---
nnoremap <leader>dl :t.<CR>
vnoremap <leader>dl :t'><CR>

" --- Macro ---
nnoremap Q @q
vnoremap Q :norm @q<CR>

" --- File explorer ---
nnoremap <leader>e :NERDTreeToggle<CR>
nnoremap <leader>E :NERDTreeFind<CR>

" --- FZF ---
nnoremap <leader>ff :Files<CR>
nnoremap <leader>fg :Rg<CR>
nnoremap <leader>fb :Buffers<CR>
nnoremap <leader>fh :History<CR>
nnoremap <leader>fc :Commands<CR>
nnoremap <leader>fm :Maps<CR>
nnoremap <leader>ft :BTags<CR>
nnoremap <leader>fT :Tags<CR>
nnoremap <leader>fl :BLines<CR>
nnoremap <leader>fL :Lines<CR>

" --- Git ---
nnoremap <leader>gs :Git<CR>
nnoremap <leader>gc :Git commit<CR>
nnoremap <leader>gp :Git push<CR>
nnoremap <leader>gl :Git pull<CR>
nnoremap <leader>gb :Git blame<CR>
nnoremap <leader>gd :Gdiffsplit<CR>
nnoremap <leader>gD :Gdiffsplit!<CR>
nnoremap <leader>gL :Git log --oneline<CR>

" --- Gitgutter ---
nnoremap ]h         :GitGutterNextHunk<CR>
nnoremap [h         :GitGutterPrevHunk<CR>
nnoremap <leader>hs :GitGutterStageHunk<CR>
nnoremap <leader>hu :GitGutterUndoHunk<CR>
nnoremap <leader>hp :GitGutterPreviewHunk<CR>

" --- Floaterm ---
nnoremap <leader>tt :FloatermToggle<CR>
nnoremap <leader>tn :FloatermNew<CR>
nnoremap <leader>tk :FloatermKill<CR>
nnoremap <leader>t[ :FloatermPrev<CR>
nnoremap <leader>t] :FloatermNext<CR>
tnoremap <Esc><Esc> <C-\><C-n>:FloatermToggle<CR>

" --- ALE ---
nnoremap <leader>af :ALEFix<CR>
nnoremap <leader>ad :ALEDetail<CR>
nnoremap <leader>at :ALEToggle<CR>
nnoremap ]a         :ALENextWrap<CR>
nnoremap [a         :ALEPreviousWrap<CR>

" --- Toggles ---
nnoremap <leader>un :set number!<CR>
nnoremap <leader>ur :set relativenumber!<CR>
nnoremap <leader>uw :set wrap!<CR>
nnoremap <leader>us :set spell!<CR>
nnoremap <leader>uh :set hlsearch!<CR>
nnoremap <leader>ub :call ToggleBackground()<CR>
nnoremap <leader>uc :set cursorline!<CR>
nnoremap <leader>ul :set list!<CR>

" --- Tools ---
nnoremap <F8>      :TagbarToggle<CR>
nnoremap <leader>U :UndotreeToggle<CR>

" --- Database ---
nnoremap <leader>Du :DBUIToggle<CR>
nnoremap <leader>Df :DBUIFindBuffer<CR>

" --- Session ---
nnoremap <leader>SS :SSave<CR>
nnoremap <leader>SL :SLoad<CR>
nnoremap <leader>Sd :SDelete<CR>
nnoremap <leader>Sc :SClose<CR>
nnoremap <leader>st :Startify<CR>

" --- Misc ---
nnoremap <leader>vi :VimInfo<CR>
nnoremap <leader>PR :FindProjectRoot<CR>

" which-key popup — must be last leader mapping
nnoremap <silent> <leader> :<c-u>WhichKey '<Space>'<CR>

" -----------------------------------------------------------------------------
" 6. Plugin Configuration
" -----------------------------------------------------------------------------

" --- better-escape ---
let g:better_escape_shortcut = ['jk', 'kj']
let g:better_escape_interval = 200

" --- Airline ---
let g:airline_theme                         = 'solarized'
let g:airline_powerline_fonts               = 1
let g:airline#extensions#tabline#enabled   = 1
let g:airline#extensions#tabline#formatter = 'unique_tail'
let g:airline#extensions#ale#enabled       = 1
let g:airline#extensions#gutentags#enabled = 1

" --- NERDTree ---
let g:NERDTreeShowHidden       = 1
let g:NERDTreeMinimalUI        = 1
let g:NERDTreeWinSize          = 35
let g:NERDTreeAutoDeleteBuffer = 1
let g:NERDTreeIgnore = ['\~$', '\.pyc$', '__pycache__', '\.git$', 'node_modules']
augroup NERDTreeQuit
  autocmd!
  autocmd BufEnter *
    \ if winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif
augroup END

" --- FZF ---
let g:fzf_layout      = { 'down': '~40%' }
let g:fzf_history_dir = expand('~/.vim/fzf-history')
let g:fzf_colors = {
  \ 'fg':      ['fg', 'Normal'],      'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],     'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine'],  'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],     'border':  ['fg', 'Ignore'],
  \ 'prompt':  ['fg', 'Conditional'], 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ }

" --- ALE — sole lint/fix engine, no LSP ---
let g:ale_linters_explicit     = 1
let g:ale_linters = {
  \ 'python':     ['ruff', 'mypy'],
  \ 'javascript': ['eslint'],
  \ 'typescript': ['eslint'],
  \ 'go':         ['gopls'],
  \ 'rust':       ['analyzer'],
  \ 'sh':         ['shellcheck'],
  \ 'vim':        ['vint'],
  \ }
let g:ale_fixers = {
  \ '*':          ['remove_trailing_lines', 'trim_whitespace'],
  \ 'python':     ['black', 'isort'],
  \ 'javascript': ['prettier'],
  \ 'typescript': ['prettier'],
  \ 'go':         ['gofmt'],
  \ 'rust':       ['rustfmt'],
  \ 'sh':         ['shfmt'],
  \ }
let g:ale_fix_on_save          = 1
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_insert_leave = 0
let g:ale_lint_on_enter        = 0
let g:ale_sign_error           = ' '
let g:ale_sign_warning         = ' '
let g:ale_echo_msg_format      = '[%linter%] %s [%severity%]'
let g:ale_virtualtext_cursor   = 'disabled'

" --- UltiSnips ---
let g:UltiSnipsExpandTrigger       = '<C-e>'
let g:UltiSnipsJumpForwardTrigger  = '<C-l>'
let g:UltiSnipsJumpBackwardTrigger = '<C-b>'
let g:UltiSnipsEditSplit           = 'vertical'

" --- Floaterm ---
let g:floaterm_width     = 0.85
let g:floaterm_height    = 0.85
let g:floaterm_autoclose = 1
let g:floaterm_position  = 'center'
let g:floaterm_title     = ' terminal ($1/$2) '

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
let g:gutentags_ctags_extra_args          = ['--tag-relative=yes', '--fields=+ailmnS']
let g:gutentags_add_default_project_roots = 0
let g:gutentags_project_root              = ['.git', '.svn', '.hg', 'package.json', 'Cargo.toml', 'go.mod']

" --- vim-move ---
let g:move_key_modifier = 'A'

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

" --- Markdown ---
let g:markdown_fenced_languages = [
  \ 'python', 'javascript', 'typescript', 'go', 'rust',
  \ 'bash=sh', 'sh', 'vim', 'json', 'yaml', 'html', 'css'
  \ ]
let g:markdown_syntax_conceal = 0

" --- which-key ---
call which_key#register('<Space>', "g:which_key_map")
let g:which_key_map             = {}
let g:which_key_map['<CR>']     = 'reload config'
let g:which_key_map.e           = 'file explorer'
let g:which_key_map.E           = 'find in tree'
let g:which_key_map.U           = 'undo tree'
let g:which_key_map.w           = { 'name': '+window'    }
let g:which_key_map.b           = { 'name': '+buffer'    }
let g:which_key_map.f           = { 'name': '+find'      }
let g:which_key_map.g           = { 'name': '+git'       }
let g:which_key_map.h           = { 'name': '+hunk'      }
let g:which_key_map.t           = { 'name': '+terminal'  }
let g:which_key_map.a           = { 'name': '+ale'       }
let g:which_key_map.u           = { 'name': '+toggle'    }
let g:which_key_map.s           = { 'name': '+search'    }
let g:which_key_map.y           = { 'name': '+yank'      }
let g:which_key_map.d           = { 'name': '+duplicate' }
let g:which_key_map.c           = { 'name': '+quickfix'  }
let g:which_key_map.l           = { 'name': '+loclist'   }
let g:which_key_map.D           = { 'name': '+database'  }
let g:which_key_map.S           = { 'name': '+session'   }

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
  if &modifiable && &filetype !~# '\v^(markdown|diff)$'
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

" Yank flash — passes match id directly into lambda to avoid E802
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
" 8. Filetype indent
" -----------------------------------------------------------------------------
augroup FileTypeIndent
  autocmd!
  autocmd FileType python                 setlocal ts=4 sw=4 expandtab
  autocmd FileType javascript,typescript  setlocal ts=2 sw=2 expandtab
  autocmd FileType html,css,scss,json,yaml setlocal ts=2 sw=2 expandtab
  autocmd FileType lua,vim,ruby           setlocal ts=2 sw=2 expandtab
  autocmd FileType go,make                setlocal ts=4 sw=4 noexpandtab
  autocmd FileType c,cpp,java,kotlin,rust setlocal ts=4 sw=4 expandtab
  autocmd FileType sh,zsh,bash            setlocal ts=2 sw=2 expandtab
  autocmd FileType sql,xml,xhtml          setlocal ts=2 sw=2 expandtab
  autocmd FileType markdown,text          setlocal ts=4 sw=4 expandtab spell textwidth=80 wrap linebreak
  autocmd FileType gitcommit              setlocal spell textwidth=72
augroup END

" -----------------------------------------------------------------------------
" 9. Terminal execution — F5/F6/F7/F9 run/compile/build/test
" -----------------------------------------------------------------------------
let s:runners = {
  \ 'python':     { 'run': 'python3 "{filepath}"', 'test': 'python3 -m pytest "{dirname}"' },
  \ 'javascript': { 'run': 'node "{filepath}"', 'test': 'npm test' },
  \ 'typescript': { 'run': 'ts-node "{filepath}"', 'test': 'npm test', 'compile': 'tsc "{filepath}"' },
  \ 'go':         { 'run': 'go run "{filepath}"', 'test': 'go test ./...', 'build': 'go build -o "{basename}" "{filepath}"' },
  \ 'rust':       { 'run': './{basename}', 'test': 'cargo test', 'build': 'cargo build', 'compile': 'rustc "{filepath}"' },
  \ 'c':          { 'run': './{basename}', 'compile': 'gcc -Wall -O2 -g "{filepath}" -o "{basename}" -lm' },
  \ 'cpp':        { 'run': './{basename}', 'compile': 'g++ -Wall -O2 -g -std=c++17 "{filepath}" -o "{basename}" -lm' },
  \ 'sh':         { 'run': 'bash "{filepath}"' },
  \ 'lua':        { 'run': 'lua "{filepath}"' },
  \ 'ruby':       { 'run': 'ruby "{filepath}"', 'test': 'ruby -Itest "{filepath}"' },
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
    \ 'filepath': l:fp,       'dirname':  fnamemodify(l:fp, ':h'),
    \ 'filename': expand('%:t'), 'basename': expand('%:t:r'),
    \ }
  let l:cmd = substitute(l:cmd, '{\(\w\+\)}', '\=get(l:vars, submatch(1), submatch(0))', 'g')
  execute 'FloatermNew --autoclose=0 ' . l:cmd
endfunction

nnoremap <silent> <F5> :call <SID>RunAction('run')<CR>
nnoremap <silent> <F6> :call <SID>RunAction('compile')<CR>
nnoremap <silent> <F7> :call <SID>RunAction('build')<CR>
nnoremap <silent> <F9> :call <SID>RunAction('test')<CR>

" -----------------------------------------------------------------------------
" 10. Commands and utilities
" -----------------------------------------------------------------------------
function! s:VimInfo()
  echo '========== Vim Info =========='
  echo 'Version : Vim ' . v:version/100 . '.' . v:version%100
  echo 'Config  : ' . $MYVIMRC
  echo 'Filetype: ' . &filetype
  echo 'Theme   : ' . (exists('g:colors_name') ? g:colors_name : 'none') . ' (' . &background . ')'
  echo 'Python3 : ' . (executable('python3') ? trim(system('python3 --version')) : 'not found')
  echo 'Node    : ' . (executable('node')    ? trim(system('node --version'))    : 'not found')
  echo 'Git     : ' . (executable('git')     ? trim(system('git --version'))     : 'not found')
  echo 'rg      : ' . (executable('rg')      ? 'yes' : 'not found')
  echo '=============================='
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
  let l:markers = ['.git', '.svn', '.hg', 'package.json', 'Cargo.toml', 'go.mod', 'Makefile', 'pyproject.toml']
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
" 11. Machine-local overrides
" -----------------------------------------------------------------------------
if filereadable(expand('~/.vimrc.local'))
  source ~/.vimrc.local
endif
