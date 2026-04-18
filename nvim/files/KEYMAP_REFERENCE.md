# NEOVIM KEYMAP REFERENCE — v2.3.10

## LEADER KEY
Space (` `)

---

## CORE EDITING
- `jk` / `kj` — Exit insert mode *(better-escape.nvim, zero lag)*
- `<Esc>` — Clear search highlight
- `Alt+j` / `Alt+k` — Move lines up/down (normal + visual)
- `<` / `>` (visual) — Indent left/right (keeps selection)

---

## WINDOW MANAGEMENT
- `<leader>ww` — Save file
- `<leader>wq` — Save and quit
- `<leader>qq` — Quit
- `<leader>qa` — Quit all
- `<leader>sv` — Vertical split
- `<leader>sh` — Horizontal split
- `<leader>se` — Equal splits
- `<leader>sx` — Close split
- `<leader>sm` — Maximize/restore split *(native Lua toggle, no plugin)*
- `Ctrl+h/j/k/l` — Navigate splits
- `Ctrl+arrows` — Resize splits

---

## BUFFERS
- `<leader>bn` / `]b` — Next buffer
- `<leader>bp` / `[b` — Previous buffer
- `<leader>bd` — Delete buffer
- `<leader>bo` — Delete other buffers

---

## FILE EXPLORER
- `<leader>ee` — Toggle explorer
- `<leader>ef` — Focus explorer
- `<leader>ec` — Close explorer
- `<leader>er` — Refresh explorer

---

## TELESCOPE (FIND)
- `<leader>ff` — Find files
- `<leader>fg` — Find git files
- `<leader>fw` — Find word (live grep)
- `<leader>fb` — Find buffers
- `<leader>fh` — Find help
- `<leader>fm` — Find marks
- `<leader>fk` — Find keymaps
- `<leader>fc` — Find commands
- `<leader>fr` — Resume last search
- `<leader>fo` — Recent files
- `Ctrl+s` — Live grep

---

## LSP & CODE
*(keymaps set on LspAttach — lsp.lua)*
- `gd` — Go to definition
- `gD` — Go to declaration
- `gi` — Go to implementation
- `gr` — References
- `K` — Hover documentation
- `<leader>k` — Signature help
- `<leader>,a` — Code action (normal + visual)
- `<leader>,r` — Rename symbol
- `<leader>,f` — Format (normal + visual)
- `<leader>,o` — Code outline (Trouble symbols)
- `<leader>,i` — Toggle inlay hints
- `<leader>,d` — Diagnostic float
- `<leader>,l` — Diagnostics list
- `<leader>,t` — Toggle diagnostics
- `<leader>ty` — Type definition
- `]d` / `[d` — Next / prev diagnostic

---

## GIT
- `<leader>.g` — LazyGit
- `<leader>.b` — Git branches
- `<leader>.c` — Git commits
- `<leader>.s` — Git status
- `<leader>.d` — Git diff (Diffview)
- `<leader>.h` — File history (Diffview)
- `<leader>.N` — Neogit UI
- `<leader>.C` — Git commit (Neogit)
- `<leader>.v` — Git history (GV)
- `<leader>.B` — Git blame HUD
- `<leader>.oi` — GitHub issues (Octo)
- `<leader>.op` — GitHub PRs (Octo)
- `<leader>.or` — Start PR review (Octo)
- `<leader>.oc` — Checkout PR (Octo)
- `]h` / `[h` — Next / prev hunk
- `<leader>.p` — Preview hunk *(buffer-local, git files only)*
- `<leader>.r` — Reset hunk *(buffer-local, git files only)*
- `<leader>.S` — Stage hunk *(buffer-local, git files only)*
- `<leader>gco` — Conflict: choose ours
- `<leader>gct` — Conflict: choose theirs
- `<leader>gcb` — Conflict: choose both
- `<leader>gc0` — Conflict: choose neither

---

## DEBUG (DAP)
- `F5` — Continue/Start
- `F6` — Toggle breakpoint
- `F7` — Step into
- `F8` — Step over
- `F9` — Step out
- `F10` — Run to cursor
- `F11` — Terminate
- `<leader>;b` — Toggle breakpoint
- `<leader>;B` — Conditional breakpoint
- `<leader>;l` — Log point
- `<leader>;c` — Continue
- `<leader>;i` — Step into
- `<leader>;o` — Step over
- `<leader>;O` — Step out
- `<leader>;r` — Toggle REPL
- `<leader>;L` — Run last
- `<leader>;t` — Toggle debug UI
- `<leader>;x` — Terminate
- `<leader>;h` — Debug hover
- `<leader>;p` — Debug preview

---

## RUN & TEST
- `<leader>'r` — Run file
- `<leader>'s` (visual) — Run selection
- `<leader>'t` — Run tests
- `<leader>'n` — Test nearest (Neotest)
- `<leader>'f` — Test file (Neotest)
- `<leader>'a` — Test all (Neotest)
- `<leader>'o` — Test output
- `<leader>'p` — Test panel
- `<leader>'u` — Test summary
- `<leader>'d` — Test debug nearest (Neotest + DAP)
- `<leader>'P` — Test all parallel (Neotest, concurrency=4)
- `<leader>'w` — Neotest watch file
- `<leader>'W` — Neotest watch nearest
- `<leader>tcv` — Coverage load
- `<leader>tcs` — Coverage summary
- `<leader>tct` — Coverage toggle

---

## TERMINAL
- `<leader>\\t` — Open terminal
- `<leader>\\f` — Float terminal
- `<leader>\\h` — Horizontal terminal
- `<leader>\\v` — Vertical terminal
- `Ctrl+\` — Toggle terminal
- `Esc` (in terminal) — Exit terminal mode

---

## UI TOGGLES
- `<leader>ut` — Toggle theme (dark/light)
- `<leader>uw` — Toggle wrap
- `<leader>us` — Toggle spell
- `<leader>ul` — Toggle line numbers
- `<leader>uz` — Zen mode
- `<leader>uF` — Deep focus mode
- `<leader>uT` — Twilight
- `<leader>un` — Dismiss notifications
- `<leader>uN` — Notification history

---

## SEARCH & REPLACE
- `<leader>/s` — Search & replace (Spectre)
- `<leader>/w` — Replace word under cursor
- `<leader>/f` — Replace in file

---

## HARPOON
- `<leader>ha` — Add file
- `<leader>hm` — Toggle menu
- `<leader>h1`–`h4` — Jump to file 1–4
- `Alt+1`–`4` — Jump to file 1–4

---

## MISC UTILITIES
- `s` — Flash jump
- `]t` / `[t` — Next / prev todo
- `<leader>xc` — Copy file path
- `<leader>xr` — Copy relative path
- `<leader>xd` — Change to file directory
- `<leader>xe` — Make executable
- `<leader>xm` — Clean memory (GC)
- `<leader>xh` — Health check
- `<leader>xp` — Go to project root
- `<leader>xl` — Lazy
- `<leader>xn` — Mason
- `<leader>xx` — Trouble diagnostics (all workspaces)
- `<leader>xX` — Trouble buffer diagnostics
- `<leader>xL` — Trouble location list
- `<leader>xQ` — Trouble quickfix list
- `<leader>xu` — Undo tree
- `<leader>xg` — Generate docstring (Neogen)
- `<leader>xT` — Find TODOs (TodoTelescope)

---

## LANGUAGE-SPECIFIC

### Python (`<leader>py*`)
- `<leader>pyv` — Select virtualenv
- `<leader>pyg` — Generate docstring
- `<leader>pydm` — Debug method
- `<leader>pydc` — Debug class
- `<leader>pyds` — Debug selection
- `<leader>pyrs` — REPL start
- `<leader>pyrr` — REPL restart
- `<leader>pyrc` — REPL send motion
- `<leader>pyrv` — REPL send visual
- `<leader>pyrl` — REPL send line
- `<leader>pyru` — REPL send until cursor
- `<leader>pyri` — REPL interrupt
- `<leader>pyrq` — REPL quit
- `<leader>pyrx` — REPL clear

### Ruby (`<leader>rb*`)
- `<leader>rbn` — Test nearest
- `<leader>rbf` — Test file
- `<leader>rbs` — Test suite
- `<leader>rbl` — Test last
- `<leader>rbv` — Test visit

### Rust (`<leader>r*`)
- `<leader>rh` — Hover actions
- `<leader>ra` — Code action
- `<leader>rd` — Debuggables
- `<leader>rt` — Testables

### Go (`<leader>go*`)
- `<leader>got` — Test
- `<leader>gof` — Test function
- `<leader>goc` — Coverage
- `<leader>gor` — Run
- `<leader>gob` — Build
- `<leader>goi` — Impl
- `<leader>goa` — Add tag
- `<leader>gom` — Mod

### Java (`<leader>jv*`)
- `<leader>jvo` — Organize imports
- `<leader>jvv` — Extract variable (normal + visual)
- `<leader>jvc` — Extract constant (normal + visual)
- `<leader>jvm` — Extract method *(visual only)*
- `<leader>jvt` — Test class
- `<leader>jvn` — Test nearest method

### Kotlin (`<leader>kt*`)
- `<leader>ktb` — Build
- `<leader>ktt` — Test
- `<leader>ktr` — Run

### TypeScript (`<leader>ts*`)
- `<leader>tso` — Organize imports
- `<leader>tsi` — Add missing imports
- `<leader>tsr` — Remove unused imports
- `<leader>tsf` — Fix all
- `<leader>tsd` — Go to source definition

### JavaScript / packages (`<leader>jp*`)
- `<leader>jps` — Show package versions
- `<leader>jpu` — Update package
- `<leader>jpd` — Delete package
- `<leader>jpi` — Install package
- `<leader>jpc` — Change version

### Elixir (`<leader>ex*`)
- `<leader>ext` — mix test
- `<leader>exf` — mix format
- `<leader>exp` — Phoenix server
- `<leader>exi` — IEx session

### C++ / CMake (`<leader>cc*`)
- `<leader>ccg` — CMake generate
- `<leader>ccb` — CMake build
- `<leader>ccr` — CMake run
- `<leader>cct` — CMake test
- `<leader>ccc` — CMake clean
- `<leader>ccs` — CMake select target
- `<leader>ccd` — Generate docstring

### Fortran (`<leader>ft*`)
- `<leader>ftb` — Build & run
- `<leader>ftc` — Check syntax
- `<leader>ftm` — Make

### Zig (`<leader>z*`)
- `<leader>zb` — Build run
- `<leader>zt` — Build test
- `<leader>zc` — Run file

### VHDL (`<leader>vh*`)
- `<leader>vha` — GHDL Analyze
- `<leader>vhe` — GHDL Elaborate
- `<leader>vhr` — GHDL Run & View
- `<leader>vhc` — GHDL Syntax Check

### COBOL (`<leader>co*`)
- `<leader>cob` — Compile & run
- `<leader>coc` — Syntax check

### Database (`<leader>db*`)
- `<leader>dbu` — Toggle DB UI
- `<leader>dba` — Add connection
- `<leader>dbf` — Find buffer

### REST (`<leader>re*`)
- `<leader>rer` — Run request
- `<leader>rep` — Preview request
- `<leader>rel` — Run last request
- `<leader>ree` — Select env file

### Markdown
- `<leader>mp` — Markdown preview toggle
- `<leader>tm` — Table mode toggle

---

## EMMET (HTML/CSS/JS/TS)
- `Ctrl+e` — Expand emmet abbreviation

---

## TASK RUNNER (Overseer) (`<leader>o*`)
- `<leader>ot` — Toggle task list
- `<leader>or` — Run task (picker)
- `<leader>ob` — Build
- `<leader>oa` — Task action
- `<leader>oc` — Clear cache
- `<leader>os` — Shell command

---

## HUD / FOCUS
- `<leader>uF` — Deep focus mode (strips all chrome + Twilight + Zen)
- `<leader>uT` — Twilight (dim non-active block)
- `<leader>.B` — Git blame virtual text toggle
- `-` — Oil inline file editor (open parent dir)
- `<leader>eo` — Oil inline file editor
- `<leader>uc` — Go to treesitter context start

---

## TREESITTER TEXT OBJECTS
- `af` / `if` — Function outer/inner
- `ac` / `ic` — Class outer/inner
- `al` / `il` — Loop outer/inner
- `aa` / `ia` — Parameter outer/inner
- `ai` / `ii` — Conditional outer/inner
- `]f` / `[f` — Next/prev function start
- `]c` / `[c` — Next/prev class start
- `]a` / `[a` — Next/prev parameter start
- `<leader>sa` / `<leader>sA` — Swap parameter next/prev

---

## SESSIONS (persistence.nvim)
- `<leader>qs` — Restore session (cwd)
- `<leader>ql` — Restore last session
- `<leader>qd` — Don't save session on exit
