# KEYMAP REFERENCE — v2.4.0

**Leader:** `Space`

All keymaps are registered lazily — they load only when the relevant plugin
is active. Plugin-backed keymaps display a warning notification if the plugin
is not loaded. Buffer-local keymaps (LSP, language-specific) activate on the
first relevant `LspAttach` or `FileType` event.

---

## Contents

- [Editing](#editing)
- [Windows & Splits](#windows--splits)
- [Buffers](#buffers)
- [Navigation](#navigation)
- [Find — Telescope](#find--telescope)
- [File Explorer — Neo-tree](#file-explorer--neo-tree)
- [LSP](#lsp)
- [Git](#git)
- [Debug — DAP](#debug--dap)
- [Run & Test](#run--test)
- [Terminal](#terminal)
- [UI & Focus](#ui--focus)
- [Search & Replace](#search--replace)
- [Harpoon](#harpoon)
- [Editing Enhancements — mini.*](#editing-enhancements--mini)
- [Treesitter Text Objects](#treesitter-text-objects)
- [Folding](#folding)
- [Utilities](#utilities)
- [Sessions](#sessions)
- [Task Runner — Overseer](#task-runner--overseer)
- [Language: Python](#language-python)
- [Language: Rust](#language-rust)
- [Language: Go](#language-go)
- [Language: TypeScript](#language-typescript)
- [Language: JavaScript Packages](#language-javascript-packages)
- [Language: Java](#language-java)
- [Language: Kotlin](#language-kotlin)
- [Language: Ruby](#language-ruby)
- [Language: Elixir](#language-elixir)
- [Language: C](#language-c)
- [Language: C++ / CMake](#language-c--cmake)
- [Language: Fortran](#language-fortran)
- [Language: Zig](#language-zig)
- [Language: VHDL](#language-vhdl)
- [Language: COBOL](#language-cobol)
- [Language: Database](#language-database)
- [Language: REST](#language-rest)
- [Language: Markdown](#language-markdown)
- [Language: HTML / CSS / Emmet](#language-html--css--emmet)

---

## Editing

| Key | Mode | Action |
|-----|------|--------|
| `jk` / `kj` | Insert | Exit insert mode |
| `<Esc>` | Normal | Clear search highlight |
| `Alt+j` | Normal / Visual | Move line / selection down |
| `Alt+k` | Normal / Visual | Move line / selection up |
| `<` | Visual | Indent left, keep selection |
| `>` | Visual | Indent right, keep selection |

---

## Windows & Splits

| Key | Action |
|-----|--------|
| `<leader>sv` | Vertical split |
| `<leader>sh` | Horizontal split |
| `<leader>se` | Equal splits |
| `<leader>sx` | Close current split |
| `<leader>sm` | Maximize / restore split (toggle) |
| `Ctrl+h` | Move to left split |
| `Ctrl+j` | Move to split below |
| `Ctrl+k` | Move to split above |
| `Ctrl+l` | Move to right split |
| `Ctrl+↑` | Increase split height |
| `Ctrl+↓` | Decrease split height |
| `Ctrl+←` | Decrease split width |
| `Ctrl+→` | Increase split width |

> `Ctrl+j` and `Ctrl+k` are normal-mode only.
> In insert mode, blink.cmp uses them for completion navigation.

---

## Buffers

| Key | Action |
|-----|--------|
| `<leader>ww` | Save current buffer |
| `<leader>wq` | Save and quit |
| `<leader>qq` | Quit |
| `<leader>qa` | Quit all |
| `<leader>bn` / `]b` | Next buffer |
| `<leader>bp` / `[b` | Previous buffer |
| `<leader>bd` | Delete buffer |
| `<leader>bo` | Delete all other buffers |

---

## Navigation

Global movement keys work in all file types.

| Key | Action |
|-----|--------|
| `s` | Flash jump — type 2 chars to jump anywhere visible |
| `]d` / `[d` | Next / prev diagnostic |
| `]h` / `[h` | Next / prev git hunk |
| `]b` / `[b` | Next / prev buffer |
| `]t` / `[t` | Next / prev TODO comment |
| `]f` / `[f` | Next / prev function start (treesitter) |
| `]c` / `[c` | Next / prev class start (treesitter) |
| `]a` / `[a` | Next / prev parameter start (treesitter) |
| `]F` / `[F` | Next / prev function end (treesitter) |
| `]C` / `[C` | Next / prev class end (treesitter) |
| `<leader>uc` | Go to treesitter context start (jump to enclosing scope) |
| `-` | Open Oil (parent directory) |

---

## Find — Telescope

| Key | Action |
|-----|--------|
| `<leader>ff` | Find files |
| `<leader>fg` | Find git-tracked files |
| `<leader>fw` | Live grep (project-wide search) |
| `<leader>fb` | Find open buffers |
| `<leader>fh` | Search help tags |
| `<leader>fm` | Browse marks |
| `<leader>fk` | Browse keymaps |
| `<leader>fc` | Browse user commands |
| `<leader>fo` | Recent files |
| `<leader>fr` | Resume last Telescope search |
| `Ctrl+s` | Live grep (shortcut) |

---

## File Explorer — Neo-tree

| Key | Action |
|-----|--------|
| `<leader>ee` | Toggle / reveal current file in explorer |
| `<leader>ef` | Focus explorer |
| `<leader>ec` | Close explorer |
| `<leader>er` | Refresh explorer |
| `<leader>eo` | Open Oil (edit filesystem as buffer) |
| `-` | Open Oil at parent directory (works anywhere) |

### Inside Neo-tree

| Key | Action |
|-----|--------|
| `<CR>` / `l` | Open file or enter directory |
| `<Space>` | Toggle node expand/collapse |
| `a` | Add file |
| `A` | Add directory |
| `d` | Delete |
| `r` | Rename |
| `c` | Copy |
| `x` | Cut |
| `p` | Paste |
| `y` | Copy path to clipboard |
| `H` | Toggle hidden files |
| `/` | Fuzzy find in explorer |
| `P` | Toggle preview |
| `S` | Open in horizontal split |
| `s` | Open in vertical split |
| `t` | Open in new tab |
| `i` | Show file details |
| `?` | Show help |

---

## LSP

> Buffer-local. Set on `LspAttach` — active only when an LSP server is
> attached to the buffer.

| Key | Action |
|-----|--------|
| `gd` | Go to definition |
| `gD` | Go to declaration |
| `gi` | Go to implementation |
| `gr` | Show references |
| `K` | Hover documentation |
| `<leader>k` | Signature help |
| `<leader>,a` | Code action (via actions-preview) |
| `<leader>,r` | Rename symbol (via inc-rename if available) |
| `<leader>,f` | Format buffer or range |
| `<leader>,o` | Code outline (Trouble symbols) |
| `<leader>,i` | Toggle inlay hints |
| `<leader>,d` | Diagnostic float for current line |
| `<leader>,l` | Diagnostic list (location list) |
| `<leader>,t` | Toggle diagnostics for buffer |
| `<leader>ty` | Go to type definition |
| `]d` | Next diagnostic |
| `[d` | Previous diagnostic |

> `<leader>,f` also works in Visual mode to format a selection.
> `<leader>,a` also works in Visual mode for range code actions.

---

## Git

| Key | Action |
|-----|--------|
| `<leader>.g` | LazyGit — full-screen interactive Git TUI |
| `<leader>.b` | Browse branches (Telescope) |
| `<leader>.c` | Browse commits (Telescope) |
| `<leader>.s` | Git status (Telescope) |
| `<leader>.d` | Open Diffview |
| `<leader>.h` | File history (Diffview) |
| `<leader>.N` | Neogit status window |
| `<leader>.C` | Neogit commit |
| `<leader>.v` | Commit graph browser (GV) |
| `<leader>.B` | Toggle per-line blame (blame.nvim) |

### Hunk operations (buffer-local, git files only)

| Key | Action |
|-----|--------|
| `]h` | Next hunk |
| `[h` | Previous hunk |
| `<leader>.p` | Preview hunk |
| `<leader>.r` | Reset hunk |
| `<leader>.S` | Stage hunk |
| `<leader>.r` (visual) | Reset selected lines |
| `<leader>.S` (visual) | Stage selected lines |

### GitHub — Octo

| Key | Action |
|-----|--------|
| `<leader>.oi` | List GitHub issues |
| `<leader>.op` | List GitHub PRs |
| `<leader>.or` | Start PR review |
| `<leader>.oc` | Checkout PR |

### Conflict resolution (git-conflict)

| Key | Action |
|-----|--------|
| `<leader>gco` | Choose ours |
| `<leader>gct` | Choose theirs |
| `<leader>gcb` | Choose both |
| `<leader>gc0` | Choose neither |

---

## Debug — DAP

### Function keys (global)

| Key | Action |
|-----|--------|
| `F5` | Continue / start debugging |
| `F6` | Toggle breakpoint |
| `F7` | Step into |
| `F8` | Step over |
| `F9` | Step out |
| `F10` | Run to cursor |
| `F11` | Terminate debug session |

### Leader bindings

| Key | Action |
|-----|--------|
| `<leader>;b` | Toggle breakpoint |
| `<leader>;B` | Set conditional breakpoint (prompts for condition) |
| `<leader>;l` | Set log point (prompts for message) |
| `<leader>;c` | Continue |
| `<leader>;i` | Step into |
| `<leader>;o` | Step over |
| `<leader>;O` | Step out |
| `<leader>;L` | Run last debug configuration |
| `<leader>;r` | Toggle REPL |
| `<leader>;t` | Toggle DAP UI |
| `<leader>;x` | Terminate |
| `<leader>;h` | Hover variable value |
| `<leader>;p` | Preview variable value |

### Supported adapters

| Language | Adapter | Install |
|----------|---------|---------|
| Python | debugpy | `:MasonInstall debugpy` |
| Rust | codelldb | `:MasonInstall codelldb` |
| C / C++ | codelldb | `:MasonInstall codelldb` |
| Go | delve | `:MasonInstall delve` |
| JavaScript / TypeScript | pwa-node | `:MasonInstall js-debug-adapter` |
| Java / Kotlin | java-debug | `:MasonInstall java-debug-adapter` |
| Ruby | rdbg | `gem install debug` |
| Elixir | elixir-ls | `:MasonInstall elixir-ls` |
| Zig | codelldb | `:MasonInstall codelldb` |

---

## Run & Test

### File and selection runner

| Key | Mode | Action |
|-----|------|--------|
| `<leader>'r` | Normal | Run current file (auto-saves first) |
| `<leader>'s` | Visual | Run selected lines in interpreter |
| `<leader>'t` | Normal | Run project test suite |

Supported filetypes for `<leader>'r`: Python, Rust, Go, JavaScript,
TypeScript, Lua, C, C++, Java, Bash/sh, Julia, Zig, Nim, Ruby,
Elixir, Kotlin, COBOL, VHDL, Fortran.

### Neotest

| Key | Action |
|-----|--------|
| `<leader>'n` | Run nearest test |
| `<leader>'f` | Run all tests in file |
| `<leader>'a` | Run entire test suite |
| `<leader>'P` | Run suite in parallel (concurrency=4) |
| `<leader>'d` | Debug nearest test (DAP strategy) |
| `<leader>'w` | Watch file for changes, re-run tests |
| `<leader>'W` | Watch nearest test |
| `<leader>'o` | Open test output |
| `<leader>'p` | Toggle test output panel |
| `<leader>'u` | Toggle test summary tree |

### Coverage

| Key | Action |
|-----|--------|
| `<leader>tcv` | Load coverage data |
| `<leader>tcs` | Show coverage summary |
| `<leader>tct` | Toggle coverage highlights |

---

## Terminal

| Key | Mode | Action |
|-----|------|--------|
| `<C-\>` | Normal / Terminal | Toggle terminal |
| `<leader>\t` | Normal | Open terminal (toggleterm) |
| `<leader>\f` | Normal | Open floating terminal |
| `<leader>\h` | Normal | Open horizontal terminal |
| `<leader>\v` | Normal | Open vertical terminal |
| `<Esc>` | Terminal | Exit terminal mode (back to Normal) |

---

## UI & Focus

| Key | Action |
|-----|--------|
| `<leader>ut` | Toggle dark / light theme |
| `<leader>uw` | Toggle line wrap |
| `<leader>us` | Toggle spell check |
| `<leader>ul` | Toggle line numbers |
| `<leader>uz` | Zen mode (centred buffer, strips chrome) |
| `<leader>uF` | Deep focus mode (zen + twilight + strip all chrome) |
| `<leader>uT` | Twilight (dims inactive code regions) |
| `<leader>uc` | Jump to current treesitter context start |
| `<leader>un` | Dismiss all notifications |
| `<leader>uN` | Browse notification history |

> **Deep focus vs Zen mode:**
> `<leader>uF` activates ZenMode + Twilight and also strips the statusline,
> bufferline, signcolumn, ruler, and line numbers. Pressing it again
> restores all options exactly as they were before activation.
> `<leader>uz` activates ZenMode only — HUD elements remain visible.

---

## Search & Replace

| Key | Action |
|-----|--------|
| `<leader>/s` | Open Spectre (project-wide search & replace) |
| `<leader>/w` | Replace word under cursor across project |
| `<leader>/f` | Replace within current file only |

---

## Harpoon

| Key | Action |
|-----|--------|
| `<leader>ha` | Add current file to Harpoon list |
| `<leader>hm` | Open Harpoon quick menu |
| `<leader>h1` / `Alt+1` | Jump to Harpoon file 1 |
| `<leader>h2` / `Alt+2` | Jump to Harpoon file 2 |
| `<leader>h3` / `Alt+3` | Jump to Harpoon file 3 |
| `<leader>h4` / `Alt+4` | Jump to Harpoon file 4 |

> Harpoon list is keyed to the project root. Opening a different project
> gives a different list automatically.

---

## Editing Enhancements — mini.*

### Comments (mini.comment)

| Key | Mode | Action |
|-----|------|--------|
| `gc` | Normal / Visual | Toggle comment |
| `gcc` | Normal | Toggle line comment |

### Surround (mini.surround)

| Key | Mode | Action |
|-----|------|--------|
| `gsa` | Visual | Add surrounding |
| `gsd` | Normal | Delete surrounding |
| `gsr` | Normal | Replace surrounding |
| `gsf` | Normal | Find surrounding (forward) |
| `gsF` | Normal | Find surrounding (backward) |
| `gsh` | Normal | Highlight surrounding |

### Align (mini.align)

| Key | Mode | Action |
|-----|------|--------|
| `ga` | Normal / Visual | Align (interactive) |
| `gA` | Normal / Visual | Align with preview |

### Split / Join (mini.splitjoin)

| Key | Action |
|-----|--------|
| `gS` | Toggle split / join (arguments, arrays, objects) |

### Move (mini.move)

| Key | Mode | Action |
|-----|------|--------|
| `Alt+h` | Normal / Visual | Move selection left |
| `Alt+j` | Normal / Visual | Move selection down |
| `Alt+k` | Normal / Visual | Move selection up |
| `Alt+l` | Normal / Visual | Move selection right |

> `Alt+j` / `Alt+k` in Normal mode are also wired to `keymaps.lua` for
> single-line movement. The mini.move binding takes precedence in Visual mode.

---

## Treesitter Text Objects

### Selection

| Key | Selects |
|-----|---------|
| `af` / `if` | Function outer / inner |
| `ac` / `ic` | Class outer / inner |
| `al` / `il` | Loop outer / inner |
| `aa` / `ia` | Parameter outer / inner |
| `ai` / `ii` | Conditional outer / inner |

### Motion

| Key | Action |
|-----|--------|
| `]f` / `[f` | Next / prev function start |
| `]F` / `[F` | Next / prev function end |
| `]c` / `[c` | Next / prev class start |
| `]C` / `[C` | Next / prev class end |
| `]a` / `[a` | Next / prev parameter start |
| `]A` / `[A` | Next / prev parameter end |

### Swap

| Key | Action |
|-----|--------|
| `<leader>sa` | Swap parameter with next |
| `<leader>sA` | Swap parameter with previous |

### Incremental selection

| Key | Action |
|-----|--------|
| `Ctrl+Space` | Initialise / expand selection |
| `<Backspace>` | Shrink selection |

---

## Folding

Powered by nvim-ufo (treesitter + indent providers).

| Key | Action |
|-----|--------|
| `zR` | Open all folds |
| `zM` | Close all folds |
| `zr` | Open folds except imports/comments |
| `zm` | Close folds by level |
| `zo` / `zO` | Open fold / open recursively |
| `zc` / `zC` | Close fold / close recursively |
| `za` | Toggle fold |
| `zp` | Preview fold content (nvim-ufo) |

---

## Utilities

| Key | Action |
|-----|--------|
| `<leader>xc` | Copy absolute file path to clipboard |
| `<leader>xr` | Copy relative file path to clipboard |
| `<leader>xd` | Change directory to current file's directory |
| `<leader>xe` | Make current file executable (`chmod +x`) |
| `<leader>xm` | Run Lua garbage collection (free memory) |
| `<leader>xh` | `:Health` summary (version, LSP, memory) |
| `<leader>xp` | LCD to project root (window-local) |
| `<leader>xl` | Open Lazy plugin manager |
| `<leader>xn` | Open Mason |
| `<leader>xx` | Trouble: all diagnostics |
| `<leader>xX` | Trouble: current buffer diagnostics |
| `<leader>xL` | Trouble: location list |
| `<leader>xQ` | Trouble: quickfix list |
| `<leader>xu` | Undo tree |
| `<leader>xg` | Generate docstring at cursor (Neogen) |
| `<leader>xT` | Find TODOs in project (Telescope) |

---

## Sessions

| Key | Action |
|-----|--------|
| `<leader>qs` | Restore session for current directory |
| `<leader>ql` | Restore most recent session |
| `<leader>qd` | Disable session saving for this session |

> Sessions are stored in `~/.local/state/nvim/sessions/` and are keyed
> to the working directory. The session includes open buffers, fold state,
> window sizes, and tab pages.

---

## Task Runner — Overseer

| Key | Action |
|-----|--------|
| `<leader>ot` | Toggle task list |
| `<leader>or` | Run a task (picker) |
| `<leader>ob` | Run "build" task template |
| `<leader>oa` | Task action menu |
| `<leader>oc` | Clear task cache |
| `<leader>os` | Run shell command as task |

---

## Language: Python

> Buffer-local. Active on Python files. `<leader>py*`

| Key | Action |
|-----|--------|
| `<leader>pyv` | Select virtual environment (venv-selector) |
| `<leader>pyg` | Generate docstring (Neogen, Google style) |

### DAP (buffer-local, Python files)

| Key | Action |
|-----|--------|
| `<leader>pydm` | Debug test method under cursor |
| `<leader>pydc` | Debug test class under cursor |
| `<leader>pyds` | Debug selected code (visual) |

### REPL — iron.nvim

| Key | Mode | Action |
|-----|------|--------|
| `<leader>pyrs` | Normal | Start REPL (ipython if available, else python3) |
| `<leader>pyrr` | Normal | Restart REPL |
| `<leader>pyrc` | Normal | Send motion to REPL (operator, e.g. `<leader>pyrcip`) |
| `<leader>pyrv` | Visual | Send selection to REPL |
| `<leader>pyrl` | Normal | Send current line to REPL |
| `<leader>pyru` | Normal | Send from top to cursor to REPL |
| `<leader>pyri` | Normal | Interrupt REPL |
| `<leader>pyrq` | Normal | Quit REPL |
| `<leader>pyrx` | Normal | Clear REPL screen |

---

## Language: Rust

> Buffer-local. Active on Rust files. Powered by rustaceanvim.

| Key | Action |
|-----|--------|
| `<leader>rh` | Hover actions (expand macros, view type layouts) |
| `<leader>ra` | Code action (Rust-specific) |
| `<leader>rd` | Debuggables (pick binary to debug) |
| `<leader>rt` | Testables (pick test to run) |

---

## Language: Go

> Buffer-local. Active on Go files. Powered by go.nvim.

| Key | Action |
|-----|--------|
| `<leader>got` | Run all tests (`go test ./...`) |
| `<leader>gof` | Run test function under cursor |
| `<leader>goc` | Show test coverage |
| `<leader>gor` | Run package (`go run`) |
| `<leader>gob` | Build (`go build`) |
| `<leader>goi` | Generate interface implementation stub |
| `<leader>goa` | Add struct tags |
| `<leader>gom` | Go mod operations |

---

## Language: TypeScript

> Buffer-local. Active on TypeScript and TSX files.

| Key | Action |
|-----|--------|
| `<leader>tso` | Organize imports |
| `<leader>tsi` | Add missing imports |
| `<leader>tsr` | Remove unused imports |
| `<leader>tsf` | Fix all auto-fixable issues |
| `<leader>tsd` | Go to source definition (not `.d.ts`) |

---

## Language: JavaScript Packages

> Active when `package.json` is open. Requires network access for version data.

| Key | Action |
|-----|--------|
| `<leader>jps` | Show current package versions (network) |
| `<leader>jpu` | Update package under cursor |
| `<leader>jpd` | Delete package under cursor |
| `<leader>jpi` | Install new package |
| `<leader>jpc` | Change version of package under cursor |

---

## Language: Java

> Buffer-local. Active on Java files. Powered by nvim-jdtls.

| Key | Action |
|-----|--------|
| `<leader>jvo` | Organize imports |
| `<leader>jvv` | Extract variable |
| `<leader>jvc` | Extract constant |
| `<leader>jvm` | Extract method (visual selection) |
| `<leader>jvt` | Run test class |
| `<leader>jvn` | Run nearest test method |
| `<leader>jvg` | Generate Javadoc at cursor |

---

## Language: Kotlin

> Buffer-local. Active on Kotlin files.

| Key | Action |
|-----|--------|
| `<leader>ktb` | Build (Gradle or Maven, falls back to kotlinc) |
| `<leader>ktt` | Test (Gradle or Maven) |
| `<leader>ktr` | Run (Gradle or Maven) |

---

## Language: Ruby

> Buffer-local. Active on Ruby files.

| Key | Action |
|-----|--------|
| `<leader>rbn` | Run nearest test (vim-test) |
| `<leader>rbf` | Run tests in current file (vim-test) |
| `<leader>rbs` | Run entire test suite (vim-test) |
| `<leader>rbl` | Re-run last test (vim-test) |
| `<leader>rbv` | Open last-run test file (vim-test) |

---

## Language: Elixir

> Buffer-local. Active on Elixir files.

| Key | Action |
|-----|--------|
| `<leader>ext` | Run `mix test` (project root) |
| `<leader>exf` | Run `mix format` (project root) |
| `<leader>exp` | Start Phoenix server (`mix phx.server`) |
| `<leader>exi` | Open IEx REPL (`iex -S mix`) |

---

## Language: C

> Buffer-local. Active on C files.

| Key | Action |
|-----|--------|
| `<leader>cb` | Build and run with gcc (`-Wall -Wextra -g`) |
| `<leader>cm` | Run `make` at project root |
| `<leader>csy` | Syntax-only check (`gcc -fsyntax-only`) |

> Override build flags: `vim.g.c_build_flags = "-Wall -O2 -std=c11"`

---

## Language: C++ / CMake

> Buffer-local. Active on C++ and CMake files.

| Key | Action |
|-----|--------|
| `<leader>ccg` | CMake generate (`cmake -S. -Bbuild`) |
| `<leader>ccb` | CMake build |
| `<leader>ccr` | CMake run |
| `<leader>cct` | CMake test (`ctest`) |
| `<leader>ccc` | CMake clean |
| `<leader>ccs` | Select CMake target |
| `<leader>ccd` | Generate docstring at cursor (Neogen / Doxygen) |

---

## Language: Fortran

> Buffer-local. Active on Fortran files.

| Key | Action |
|-----|--------|
| `<leader>ftb` | Build and run with gfortran |
| `<leader>ftc` | Syntax check (`gfortran -fsyntax-only`) |
| `<leader>ftm` | Run `make` at project root |

---

## Language: Zig

> Buffer-local. Active on Zig files.

| Key | Action |
|-----|--------|
| `<leader>zb` | `zig build run` (requires `build.zig`) |
| `<leader>zt` | `zig build test` (requires `build.zig`) |
| `<leader>zc` | `zig run <current file>` |

---

## Language: VHDL

> Buffer-local. Active on VHDL files. Requires `ghdl` on `PATH`.

| Key | Action |
|-----|--------|
| `<leader>vha` | Analyze file (`ghdl -a`) |
| `<leader>vhe` | Elaborate entity (prompts for entity name) |
| `<leader>vhr` | Run simulation and open GTKWave |
| `<leader>vhc` | Syntax check (`ghdl -s`) |

---

## Language: COBOL

> Buffer-local. Active on COBOL files. Requires `cobc` (GnuCOBOL).

| Key | Action |
|-----|--------|
| `<leader>cob` | Compile and run (temp executable, cleaned up after) |
| `<leader>coc` | Syntax check (`cobc -fsyntax-only`) |

---

## Language: Database

> Commands available via `:DBUI` or the keymaps below.

| Key | Action |
|-----|--------|
| `<leader>dbu` | Toggle DB UI panel |
| `<leader>dba` | Add database connection |
| `<leader>dbf` | Find query buffer |

---

## Language: REST

> Buffer-local. Active on `.http` and `.rest` files.
> Requires `curl` on `PATH`. `jq` recommended for JSON formatting.

| Key | Action |
|-----|--------|
| `<leader>rer` | Run request under cursor |
| `<leader>rep` | Preview request (show curl command) |
| `<leader>rel` | Re-run last request |
| `<leader>ree` | Select environment file (`.env`) |

---

## Language: Markdown

> Buffer-local. Active on Markdown files.

| Key | Action |
|-----|--------|
| `<leader>mp` | Toggle browser preview (markdown-preview.nvim) |
| `<leader>tm` | Toggle table mode (vim-table-mode) |

> Markdown files automatically enable `wrap = true` and `spell = true`
> via `autocmds.lua`.

---

## Language: HTML / CSS / Emmet

> Active on HTML, CSS, JavaScript, TypeScript, and related files.

| Key | Mode | Action |
|-----|------|--------|
| `Ctrl+e` | Insert | Expand Emmet abbreviation |

> Auto-close and auto-rename HTML/JSX tags is handled by nvim-ts-autotag
> and requires no keybinding — it activates on `>` and tag rename in Insert mode.

---

*Last updated: v2.4.0*
