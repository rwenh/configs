# KEYMAP REFERENCE — v2.4.1

**Leader:** `Space`

Buffer-local keymaps (LSP, language-specific) activate on the first `LspAttach` or `FileType` event for that buffer. Plugin-backed keymaps notify if the plugin is not loaded.

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
- [Language: TypeScript / JavaScript](#language-typescript--javascript)
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

> `jk` / `kj` insert-mode escape is handled by **better-escape.nvim** (timeout 200 ms).
> It is not a hard keymap and does not appear in `:map`.

| Key | Mode | Action |
|-----|------|--------|
| `jk` / `kj` | Insert | Exit insert mode (better-escape.nvim) |
| `<Esc>` | Normal | Clear search highlight |
| `Alt+j` | Normal / Visual | Move line / selection down |
| `Alt+k` | Normal / Visual | Move line / selection up |
| `<` / `>` | Visual | Indent left / right, keep selection |

---

## Windows & Splits

| Key | Action |
|-----|--------|
| `<leader>sv` | Vertical split |
| `<leader>sh` | Horizontal split |
| `<leader>se` | Equal splits |
| `<leader>sx` | Close current split |
| `<leader>sm` | Maximize / restore split |
| `Ctrl+h` | Move to left split |
| `Ctrl+j` | Move to lower split |
| `Ctrl+k` | Move to upper split |
| `Ctrl+l` | Move to right split |
| `Ctrl+↑` | Increase split height |
| `Ctrl+↓` | Decrease split height |
| `Ctrl+←` | Decrease split width |
| `Ctrl+→` | Increase split width |

> `Ctrl+j` / `Ctrl+k` are Normal-mode only. blink.cmp owns them in Insert mode — there is no conflict.

---

## Buffers

| Key | Action |
|-----|--------|
| `<leader>ww` | Save |
| `<leader>wq` | Save and quit |
| `<leader>qq` | Quit |
| `<leader>qa` | Quit all |
| `<leader>bn` / `]b` | Next buffer |
| `<leader>bp` / `[b` | Previous buffer |
| `<leader>bd` | Delete buffer |
| `<leader>bo` | Delete all other buffers |

---

## Navigation

| Key | Action |
|-----|--------|
| `s` | Flash jump |
| `]d` / `[d` | Next / prev diagnostic |
| `]h` / `[h` | Next / prev git hunk |
| `]b` / `[b` | Next / prev buffer |
| `]t` / `[t` | Next / prev TODO |
| `]f` / `[f` | Next / prev function start |
| `]c` / `[c` | Next / prev class start |
| `]a` / `[a` | Next / prev parameter start |
| `<leader>uc` | Jump to enclosing treesitter context |
| `-` | Open Oil (parent directory) |

---

## Find — Telescope

| Key | Action |
|-----|--------|
| `<leader>ff` | Find files |
| `<leader>fg` | Find git-tracked files |
| `<leader>fw` | Live grep |
| `<leader>fb` | Find open buffers |
| `<leader>fh` | Search help tags |
| `<leader>fm` | Browse marks |
| `<leader>fk` | Browse keymaps |
| `<leader>fc` | Browse user commands |
| `<leader>fo` | Recent files |
| `<leader>fr` | Resume last search |
| `Ctrl+s` | Live grep (shortcut) |

---

## File Explorer — Neo-tree

| Key | Action |
|-----|--------|
| `<leader>ee` | Toggle / reveal current file |
| `<leader>ef` | Focus explorer |
| `<leader>ec` | Close explorer |
| `<leader>er` | Refresh explorer |
| `<leader>eo` | Open Oil |
| `-` | Open Oil at parent directory |

### Inside Neo-tree

| Key | Action |
|-----|--------|
| `<CR>` / `l` | Open file or enter directory |
| `<Space>` | Toggle node |
| `a` / `A` | Add file / directory |
| `d` / `r` / `c` / `x` / `p` | Delete / rename / copy / cut / paste |
| `y` | Copy path to clipboard |
| `H` | Toggle hidden files |
| `/` | Fuzzy find in explorer |
| `P` | Toggle preview |
| `S` / `s` / `t` | Open in split / vsplit / tab |
| `?` | Show help |

---

## LSP

> Buffer-local — active when an LSP server is attached.

| Key | Action |
|-----|--------|
| `gd` | Go to definition |
| `gD` | Go to declaration |
| `gi` | Go to implementation |
| `gr` | Show references |
| `K` | Hover documentation |
| `<leader>k` | Signature help |
| `<leader>,a` | Code action |
| `<leader>,r` | Rename symbol |
| `<leader>,f` | Format buffer or range |
| `<leader>,o` | Code outline (Trouble) |
| `<leader>,i` | Toggle inlay hints |
| `<leader>,d` | Diagnostic float |
| `<leader>,l` | Diagnostic location list |
| `<leader>,t` | Toggle diagnostics |
| `<leader>ty` | Go to type definition |
| `]d` / `[d` | Next / prev diagnostic |

---

## Git

| Key | Action |
|-----|--------|
| `<leader>.g` | LazyGit |
| `<leader>.b` | Browse branches |
| `<leader>.c` | Browse commits |
| `<leader>.s` | Git status |
| `<leader>.d` | Diffview |
| `<leader>.h` | File history |
| `<leader>.N` | Neogit status |
| `<leader>.C` | Neogit commit |
| `<leader>.v` | Commit graph (GV) |
| `<leader>.B` | Toggle per-line blame |

### Hunk operations (buffer-local)

| Key | Action |
|-----|--------|
| `]h` / `[h` | Next / prev hunk |
| `<leader>.p` | Preview hunk |
| `<leader>.r` | Reset hunk |
| `<leader>.S` | Stage hunk |
| `<leader>.r` / `<leader>.S` | Reset / stage (visual) |

### Stash

| Key | Action |
|-----|--------|
| `<leader>.zz` | Stash push (prompts for optional message) |
| `<leader>.zp` | Stash pop |
| `<leader>.zl` | List stashes (Telescope) |
| `<leader>.zh` | File history with rename tracking (`--follow`) |

### Worktree

| Key | Action |
|-----|--------|
| `<leader>.wl` | List / switch worktrees (Telescope) |
| `<leader>.wc` | Create new worktree |

### GitHub — Octo

| Key | Action |
|-----|--------|
| `<leader>.oi` | List issues |
| `<leader>.op` | List PRs |
| `<leader>.or` | Start PR review |
| `<leader>.oc` | Checkout PR |

### Conflict resolution

| Key | Action |
|-----|--------|
| `<leader>gco` | Choose ours |
| `<leader>gct` | Choose theirs |
| `<leader>gcb` | Choose both |
| `<leader>gc0` | Choose neither |

---

## Debug — DAP

### Function keys

| Key | Action |
|-----|--------|
| `F5` | Continue / start |
| `F6` | Toggle breakpoint |
| `F7` | Step into |
| `F8` | Step over |
| `F9` | Step out |
| `F10` | Run to cursor |
| `F11` | Terminate |

### Leader bindings

| Key | Action |
|-----|--------|
| `<leader>;b` | Toggle breakpoint |
| `<leader>;B` | Conditional breakpoint |
| `<leader>;l` | Log point |
| `<leader>;c` | Continue |
| `<leader>;i/o/O` | Step into / over / out |
| `<leader>;L` | Run last config |
| `<leader>;r` | Toggle REPL |
| `<leader>;t` | Toggle DAP UI |
| `<leader>;x` | Terminate |
| `<leader>;h` | Hover variable |
| `<leader>;p` | Preview variable |

---

## Run & Test

### Runner (`<leader>'`)

| Key | Mode | Action |
|-----|------|--------|
| `<leader>'r` | Normal | Run current file |
| `<leader>'s` | Visual | Run selected lines |
| `<leader>'t` | Normal | Run project test suite |

### Neotest

| Key | Action |
|-----|--------|
| `<leader>'n` | Run nearest test |
| `<leader>'f` | Run tests in file |
| `<leader>'a` | Run entire suite |
| `<leader>'P` | Run suite in parallel (concurrency=4) |
| `<leader>'d` | Debug nearest test |
| `<leader>'w` / `<leader>'W` | Watch file / watch nearest |
| `<leader>'o` | Open test output |
| `<leader>'p` | Toggle output panel |
| `<leader>'u` | Toggle summary tree |

### Coverage

| Key | Action |
|-----|--------|
| `<leader>tcv` | Load coverage |
| `<leader>tcs` | Coverage summary |
| `<leader>tct` | Toggle coverage highlights |

---

## Terminal

| Key | Mode | Action |
|-----|------|--------|
| `<C-\>` | Normal / Terminal | Toggle terminal |
| `<leader>\t` | Normal | Open terminal |
| `<leader>\f` | Normal | Float terminal |
| `<leader>\h` | Normal | Horizontal terminal |
| `<leader>\v` | Normal | Vertical terminal |
| `<Esc>` | Terminal | Exit terminal mode |

> **`<Esc>` in terminal mode** exits back to Normal before the running program
> receives the key. This is intentional for most use cases but breaks nested
> TUI programs (ranger, vim, fzf). To use double-escape instead, add to `init.lua`:
> ```lua
> vim.keymap.del("t", "<Esc>")
> vim.keymap.set("t", "<Esc><Esc>", "<C-\\><C-n>", { noremap = true })
> ```

---

## UI & Focus

| Key | Action |
|-----|--------|
| `<leader>ut` | Toggle dark / light theme |
| `<leader>uw` | Toggle line wrap |
| `<leader>us` | Toggle spell check |
| `<leader>ul` | Toggle line numbers |
| `<leader>uz` | ZenMode |
| `<leader>uF` | Deep focus (ZenMode + Twilight + strip chrome) |
| `<leader>uT` | Twilight |
| `<leader>un` | Dismiss notifications |
| `<leader>uN` | Notification history |

> `<leader>uF` snapshots all UI options and restores them exactly on exit. `<leader>uz` is ZenMode only — chrome stays visible.

---

## Search & Replace

| Key | Action |
|-----|--------|
| `<leader>/s` | Open Spectre |
| `<leader>/w` | Replace word under cursor |
| `<leader>/f` | Replace in current file |

---

## Harpoon

| Key | Action |
|-----|--------|
| `<leader>ha` | Add current file |
| `<leader>hm` | Quick menu |
| `<leader>h1`–`h4` | Jump to file 1–4 |
| `Alt+1`–`4` | Jump to file 1–4 (shortcut) |

---

## Editing Enhancements — mini.*

### Comments

| Key | Mode | Action |
|-----|------|--------|
| `gc` | Normal / Visual | Toggle comment |
| `gcc` | Normal | Toggle line comment |

### Surround

| Key | Action |
|-----|--------|
| `gsa` | Add surrounding (visual) |
| `gsd` | Delete surrounding |
| `gsr` | Replace surrounding |
| `gsf` / `gsF` | Find surrounding forward / backward |
| `gsh` | Highlight surrounding |

### Align / Split-Join / Move

| Key | Mode | Action |
|-----|------|--------|
| `ga` / `gA` | Normal / Visual | Align / align with preview |
| `gS` | Normal | Toggle split / join |
| `Alt+h/j/k/l` | Normal / Visual | Move selection |

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
| `]a` / `[a` | Next / prev parameter |

### Swap & Selection

| Key | Action |
|-----|--------|
| `<leader>sa` / `<leader>sA` | Swap parameter next / prev |
| `Ctrl+Space` | Expand incremental selection |
| `<Backspace>` | Shrink selection |

---

## Folding

| Key | Action |
|-----|--------|
| `zR` / `zM` | Open / close all folds |
| `zr` / `zm` | Open except imports / close by level |
| `zo` / `zc` / `za` | Open / close / toggle fold |
| `zp` | Preview fold (nvim-ufo) |

---

## Utilities

| Key | Action |
|-----|--------|
| `<leader>xc` | Copy absolute path |
| `<leader>xr` | Copy relative path |
| `<leader>xd` | CD to file directory |
| `<leader>xe` | Make file executable |
| `<leader>xm` | Lua garbage collection |
| `<leader>xh` | `:Health` summary |
| `<leader>xp` | LCD to project root |
| `<leader>xl` | Lazy |
| `<leader>xn` | Mason |
| `<leader>xx` / `<leader>xX` | Trouble: all / buffer diagnostics |
| `<leader>xL` / `<leader>xQ` | Trouble: location list / quickfix |
| `<leader>xu` | Undo tree |
| `<leader>xg` | Generate docstring (Neogen) |
| `<leader>xT` | Find TODOs (Telescope) |

---

## Sessions

| Key | Action |
|-----|--------|
| `<leader>qs` | Restore session (cwd) |
| `<leader>ql` | Restore last session |
| `<leader>qd` | Disable session saving |

---

## Task Runner — Overseer

| Key | Action |
|-----|--------|
| `<leader>ot` | Toggle task list |
| `<leader>or` | Run task |
| `<leader>ob` | Run build template |
| `<leader>oa` | Task action menu |
| `<leader>oc` | Clear cache |
| `<leader>os` | Run shell command as task |

---

## Language: Python

| Key | Action |
|-----|--------|
| `<leader>pyv` | Select virtual environment |
| `<leader>pyg` | Generate docstring |
| `<leader>pydm` | Debug test method |
| `<leader>pydc` | Debug test class |
| `<leader>pyds` | Debug selection (visual) |
| `<leader>pyrs` | Start REPL |
| `<leader>pyrr` | Restart REPL |
| `<leader>pyrc` | Send motion to REPL |
| `<leader>pyrv` | Send selection to REPL (visual) |
| `<leader>pyrl` | Send line to REPL |
| `<leader>pyru` | Send top-to-cursor to REPL |
| `<leader>pyri` | Interrupt REPL |
| `<leader>pyrq` | Quit REPL |
| `<leader>pyrx` | Clear REPL |

> **`<leader>pyrc` (send motion):** enters operator-pending mode — press a motion
> (`ip`, `aw`, `3j`, etc.) to send that range to the REPL. The operatorfunc is
> restored after the motion completes via a `ModeChanged` autocmd rather than a
> fixed-delay timer, so the motion selection is always captured correctly.

---

## Language: Rust

| Key | Action |
|-----|--------|
| `<leader>rh` | Hover actions |
| `<leader>ra` | Code action |
| `<leader>rd` | Debuggables |
| `<leader>rt` | Testables |

---

## Language: Go

| Key | Action |
|-----|--------|
| `<leader>got` | Run all tests |
| `<leader>gof` | Run function under cursor |
| `<leader>goc` | Coverage |
| `<leader>gor` | Run package |
| `<leader>gob` | Build |
| `<leader>goi` | Implement interface |
| `<leader>goa` | Add struct tags |
| `<leader>gom` | Go mod operations |

---

## Language: TypeScript / JavaScript

> These keymaps are active in **all four filetypes**: `typescript`, `typescriptreact`,
> `javascript`, and `javascriptreact`. All TypeScript Tools commands work for
> JavaScript files when TypeScript's JS support is active.

| Key | Action |
|-----|--------|
| `<leader>tso` | Organize imports |
| `<leader>tsi` | Add missing imports |
| `<leader>tsr` | Remove unused imports |
| `<leader>tsf` | Fix all auto-fixable |
| `<leader>tsd` | Go to source definition (not `.d.ts`) |

---

## Language: JavaScript Packages

> Active when `package.json` is open.

| Key | Action |
|-----|--------|
| `<leader>jps` | Show package versions |
| `<leader>jpu` | Update package under cursor |
| `<leader>jpd` | Delete package |
| `<leader>jpi` | Install package |
| `<leader>jpc` | Change version |

---

## Language: Java

| Key | Action |
|-----|--------|
| `<leader>jvo` | Organize imports |
| `<leader>jvv` | Extract variable |
| `<leader>jvc` | Extract constant |
| `<leader>jvm` | Extract method (visual) |
| `<leader>jvt` | Test class |
| `<leader>jvn` | Test nearest method |
| `<leader>jvg` | Generate Javadoc |

---

## Language: Kotlin

| Key | Action |
|-----|--------|
| `<leader>ktb` | Build (Gradle / Maven / kotlinc) |
| `<leader>ktt` | Test |
| `<leader>ktr` | Run |

---

## Language: Ruby

| Key | Action |
|-----|--------|
| `<leader>rgm` | Rails generate model |
| `<leader>rgc` | Rails generate controller |
| `<leader>rgs` | Rails generate scaffold |
| `<leader>rgj` | Rails generate job |
| `<leader>rgM` | Rails generate migration |
| `<leader>rgS` | Rails server (`bundle exec rails server`) |
| `<leader>rgd` | Rails db:migrate |
| `<leader>rgr` | Rails console |

### vim-test

| Key | Action |
|-----|--------|
| `<leader>rbn` | Test nearest |
| `<leader>rbf` | Test file |
| `<leader>rbs` | Test suite |
| `<leader>rbl` | Re-run last test |
| `<leader>rbv` | Open last test file |

---

## Language: Elixir

| Key | Action |
|-----|--------|
| `<leader>ext` | `mix test` |
| `<leader>exf` | `mix format` |
| `<leader>exp` | Phoenix server |
| `<leader>exi` | IEx REPL |

---

## Language: C

| Key | Action |
|-----|--------|
| `<leader>cb` | Build & run (gcc) |
| `<leader>cm` | `make` |
| `<leader>csy` | Syntax check |

> Override flags: `vim.g.c_build_flags = "-Wall -O2 -std=c11"`

---

## Language: C++ / CMake

| Key | Action |
|-----|--------|
| `<leader>ccg` | CMake generate |
| `<leader>ccb` | CMake build |
| `<leader>ccr` | CMake run |
| `<leader>cct` | CMake test |
| `<leader>ccc` | CMake clean |
| `<leader>ccs` | Select target |
| `<leader>ccd` | Generate docstring |

---

## Language: Fortran

| Key | Action |
|-----|--------|
| `<leader>ftb` | Build & run |
| `<leader>ftc` | Syntax check |
| `<leader>ftm` | `make` |

---

## Language: Zig

| Key | Action |
|-----|--------|
| `<leader>zb` | `zig build run` |
| `<leader>zt` | `zig build test` |
| `<leader>zc` | `zig run <file>` |

---

## Language: VHDL

> Requires `ghdl` on PATH. Formatter requires `vsg` (`pip install vsg`).
> Format-on-save uses `--stdin` so unsaved buffer changes are formatted correctly.

| Key | Action |
|-----|--------|
| `<leader>vha` | Analyze (`ghdl -a`) |
| `<leader>vhe` | Elaborate (prompts entity) |
| `<leader>vhr` | Run simulation + GTKWave |
| `<leader>vhc` | Syntax check |

---

## Language: COBOL

> Requires `cobc` (GnuCOBOL).

| Key | Action |
|-----|--------|
| `<leader>cob` | Compile & run |
| `<leader>coc` | Syntax check |

---

## Language: Database

| Key | Action |
|-----|--------|
| `<leader>dbu` | Toggle DB UI |
| `<leader>dba` | Add connection |
| `<leader>dbf` | Find query buffer |

---

## Language: REST

> Active on `.http` / `.rest` files. Requires `curl`.

| Key | Action |
|-----|--------|
| `<leader>rer` | Run request |
| `<leader>rep` | Preview (show curl command) |
| `<leader>rel` | Re-run last request |
| `<leader>ree` | Select environment file |

---

## Language: Markdown

| Key | Action |
|-----|--------|
| `<leader>mp` | Toggle browser preview |
| `<leader>tm` | Toggle table mode |

---

## Language: HTML / CSS / Emmet

| Key | Mode | Action |
|-----|------|--------|
| `Ctrl+e` | Insert | Expand Emmet abbreviation |

> Auto-close and auto-rename HTML/JSX tags via nvim-ts-autotag requires no keymap.
