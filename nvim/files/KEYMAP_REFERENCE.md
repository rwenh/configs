# KEYMAP REFERENCE — v2.4.1

**Leader:** `Space`  
Buffer-local keymaps activate on `LspAttach` or `FileType` for that buffer.

---

## Contents

- [Editing](#editing)
- [Windows & Splits](#windows--splits)
- [Buffers](#buffers)
- [Navigation](#navigation)
- [Find — Telescope](#find--telescope)
- [File Explorer](#file-explorer)
- [LSP](#lsp)
- [Git](#git)
- [Debug — DAP](#debug--dap)
- [Run & Test](#run--test)
- [Terminal](#terminal)
- [UI & Focus](#ui--focus)
- [Search & Replace](#search--replace)
- [Harpoon](#harpoon)
- [Editing Enhancements](#editing-enhancements--mini)
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
- [Language: HTML / CSS / Web](#language-html--css--web)

---

## Editing

| Key | Mode | Action |
|-----|------|--------|
| `jk` / `kj` | Insert | Exit insert mode (better-escape.nvim, 200 ms timeout) |
| `<Esc>` | Normal | Clear search highlight |
| `Alt+j` / `Alt+k` | Normal, Visual | Move line / selection down / up |
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
| `Ctrl+h/j/k/l` | Move to left / lower / upper / right split |
| `Ctrl+↑/↓` | Increase / decrease split height |
| `Ctrl+←/→` | Decrease / increase split width |

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
| `-` | Oil: open parent directory |

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
| `<leader>fp` | Find files (project root) |
| `<leader>fP` | Live grep (project root) |
| `<leader>fv` | Frequent / recent files (mini.visits frecency) |
| `Ctrl+s` | Live grep shortcut |

---

## File Explorer

| Key | Action |
|-----|--------|
| `<leader>ee` | Toggle / reveal current file in Neo-tree |
| `<leader>ef` | Focus Neo-tree |
| `<leader>ec` | Close Neo-tree |
| `<leader>er` | Refresh Neo-tree |
| `<leader>eo` | Open Oil |
| `<leader>em` | mini.files at current file's directory |
| `<leader>eM` | mini.files at project root |
| `-` | Oil: parent directory |

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

### Hunks

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
| `<leader>.wl` | List / switch worktrees |
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
| `<leader>;i` / `;o` / `;O` | Step into / over / out |
| `<leader>;L` | Run last config |
| `<leader>;r` | Toggle REPL |
| `<leader>;t` | Toggle DAP UI |
| `<leader>;x` | Terminate |
| `<leader>;h` | Hover variable |
| `<leader>;p` | Preview variable |
| `<leader>;E` | Configure exception breakpoints (active session only) |

---

## Run & Test

### Runner

| Key | Mode | Action |
|-----|------|--------|
| `<leader>'r` | Normal | Run current file |
| `<leader>'s` | Visual | Run selected lines |
| `<leader>'t` | Normal | Run project test suite |
| `<leader>'F` | Normal | Run nearest function (Treesitter detection) |

### Neotest

| Key | Action |
|-----|--------|
| `<leader>'n` | Run nearest test |
| `<leader>'f` | Run tests in file |
| `<leader>'a` | Run entire suite |
| `<leader>'P` | Run suite in parallel (concurrency=4) |
| `<leader>'d` | Debug nearest test |
| `<leader>'w` / `'W` | Watch file / watch suite |
| `<leader>'o` | Open test output |
| `<leader>'p` | Toggle output panel |
| `<leader>'u` | Toggle summary tree |

### Coverage

| Key | Action |
|-----|--------|
| `<leader>'cv` | Load coverage and show |
| `<leader>'cs` | Coverage summary |
| `<leader>'ct` | Toggle coverage highlights |
| `<leader>tcv` | Load coverage (alias) |
| `<leader>tcs` | Coverage summary (alias) |
| `<leader>tct` | Toggle highlights (alias) |

---

## Terminal

| Key | Mode | Action |
|-----|------|--------|
| `<C-\>` | Normal, Terminal | Toggle terminal |
| `<leader>\t` | Normal | Open terminal |
| `<leader>\f` | Normal | Float terminal |
| `<leader>\h` | Normal | Horizontal terminal |
| `<leader>\v` | Normal | Vertical terminal |
| `<Esc>` | Terminal | Exit terminal mode |

> To use double-escape instead (for nested TUIs): `vim.keymap.del("t","<Esc>")` then map `<Esc><Esc>` to `<C-\><C-n>`.

---

## UI & Focus

| Key | Action |
|-----|--------|
| `<leader>ut` | Toggle dark / light theme |
| `<leader>uw` | Toggle line wrap |
| `<leader>us` | Toggle spell check |
| `<leader>ul` | Toggle line numbers |
| `<leader>uz` | ZenMode only |
| `<leader>uF` | Deep focus (ZenMode + Twilight + strip chrome) |
| `<leader>uT` | Twilight only |
| `<leader>un` | Dismiss notifications |
| `<leader>uN` | Notification history |

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
| `gc` | Normal, Visual | Toggle comment |
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
| `ga` / `gA` | Normal, Visual | Align / align with preview |
| `gS` | Normal | Toggle split / join |
| `Alt+h/j/k/l` | Normal, Visual | Move selection |

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
| `<leader>xK` | Show global keymap conflicts |
| `<leader>xx` / `xX` | Trouble: all / buffer diagnostics |
| `<leader>xL` / `xQ` | Trouble: location list / quickfix |
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
| `<leader>pyrc` | Send motion to REPL (operator-pending) |
| `<leader>pyrv` | Send selection to REPL (visual) |
| `<leader>pyrl` | Send line to REPL |
| `<leader>pyru` | Send top-to-cursor to REPL |
| `<leader>pyri` | Interrupt REPL |
| `<leader>pyrq` | Quit REPL |
| `<leader>pyrx` | Clear REPL |

---

## Language: Rust

| Key | Action |
|-----|--------|
| `<leader>rh` | Hover actions |
| `<leader>ra` | Code action |
| `<leader>rd` | Debuggables |
| `<leader>rt` | Testables |
| `<leader>rf` | Toggle one Cargo feature flag (repeat to toggle more) |
| `<leader>rx` | cargo expand (macro viewer) |

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
| `<leader>goA` | Remove struct tags |
| `<leader>gom` | Go mod operations |
| `<leader>gog` | go generate ./... |
| `<leader>goe` | Show go env |
| `<leader>gov` | go vet ./... |

---

## Language: TypeScript / JavaScript

> Active in `typescript`, `typescriptreact`, `javascript`, `javascriptreact`.

| Key | Action |
|-----|--------|
| `<leader>tso` | Organize imports |
| `<leader>tsi` | Add missing imports |
| `<leader>tsr` | Remove unused imports |
| `<leader>tsf` | Fix all auto-fixable |
| `<leader>tsd` | Go to source definition (not `.d.ts`) |
| `<leader>tsc` | Jump to tsconfig.json |
| `<leader>tsb` | Generate barrel index.ts (prompts before overwrite) |
| `<leader>tsp` | Show tsconfig paths aliases |
| `<leader>jsm` | Show module type (ESM / CJS) |

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
| `<leader>jvR` | Hot-reload (update project config) |
| `<leader>jvs` | Spring Boot run — Spring projects only |
| `<leader>jvS` | Spring Boot package (bootJar) — Spring projects only |

---

## Language: Kotlin

| Key | Action |
|-----|--------|
| `<leader>ktb` | Build (Gradle / Maven / kotlinc) |
| `<leader>ktt` | Test |
| `<leader>ktr` | Run |
| `<leader>ktd` | Generate KDoc (function) |
| `<leader>ktD` | Generate KDoc (class) |
| `<leader>kts` | Spring Boot run — Spring projects only |
| `<leader>ktS` | Spring Boot package (bootJar) — Spring projects only |

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
| `<leader>exC` | `mix test --cover` |
| `<leader>exw` | `mix test --stale` (watch mode) |
| `<leader>exf` | `mix format` |
| `<leader>exp` | Phoenix server |
| `<leader>exi` | IEx session |
| `<leader>exd` | `mix deps.get` |
| `<leader>exc` | `mix compile` |

---

## Language: C

> Override build flags: `vim.g.c_build_flags = "-Wall -O2 -std=c11"`

| Key | Action |
|-----|--------|
| `<leader>cb` | Build & run (gcc) |
| `<leader>cm` | `make` |
| `<leader>csy` | Syntax check |
| `<leader>ctt` | CTest run (requires build/) |
| `<leader>ctb` | CMake configure + build + CTest |

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
| `<leader>ch` | Switch header ↔ source (clangd) |

---

## Language: Fortran

| Key | Action |
|-----|--------|
| `<leader>ftb` | Build & run |
| `<leader>ftc` | Syntax check |
| `<leader>ftm` | `make` |
| `<leader>ftf` | Format (fprettify) |

---

## Language: Zig

| Key | Action |
|-----|--------|
| `<leader>zb` | `zig build run` |
| `<leader>zt` | `zig build test` |
| `<leader>zc` | `zig run <file>` |
| `<leader>zf` | `zig fetch --save <url>` (prompts for URL) |
| `<leader>ze` | Show zig env (floating window) |

---

## Language: VHDL

> Requires `ghdl`. Formatter requires `vsg` (`pip install vsg`).

| Key | Action |
|-----|--------|
| `<leader>vha` | Analyze (`ghdl -a`) |
| `<leader>vhe` | Elaborate (prompts entity name) |
| `<leader>vhr` | Run simulation + GTKWave |
| `<leader>vhc` | Syntax check |
| `<leader>vht` | Generate testbench skeleton |

---

## Language: COBOL

> Requires `cobc` (GnuCOBOL).

| Key | Action |
|-----|--------|
| `<leader>cob` | Compile & run |
| `<leader>coc` | Syntax check |
| `<leader>cod` | Show detected dialect |

---

## Language: Database

| Key | Action |
|-----|--------|
| `<leader>dbu` | Toggle DB UI |
| `<leader>dba` | Add connection |
| `<leader>dbf` | Find query buffer |
| `<leader>dbs` | Schema introspection (scratch buffer) |
| `<leader>dbc` | Clear auto-loaded connection |

---

## Language: REST

> Active on `.http` / `.rest` files. Requires `curl`.

| Key | Action |
|-----|--------|
| `<leader>rer` | Run request |
| `<leader>rep` | Preview (show curl command) |
| `<leader>rel` | Re-run last request |
| `<leader>ree` | Select environment file |
| `<leader>ren` | Jump to next request |
| `<leader>reN` | Jump to prev request |
| `<leader>rec` | Copy request as curl |
| `<leader>red` | Diff two recent responses |
| `<leader>reA` | Add / store auth token (session) |
| `<leader>reat` | Insert stored auth token at cursor |

---

## Language: Markdown

| Key | Action |
|-----|--------|
| `<leader>mp` | Toggle browser preview (peek.nvim) |
| `<leader>mv` | Toggle inline render (markview) |
| `<leader>ml` | Check links (markdown-link-check) |
| `<leader>mw` | Show word count |
| `<leader>tm` | Toggle table mode |

---

## Language: HTML / CSS / Web

| Key | Mode | Action |
|-----|------|--------|
| `Ctrl+e` | Insert | Expand Emmet abbreviation |
| `<leader>cv` | Normal | Jump to CSS variable definition |
| `<leader>wv` | Normal | Start Vite dev server |
| `<leader>wb` | Normal | Run build (npm / pnpm / yarn / bun) |
