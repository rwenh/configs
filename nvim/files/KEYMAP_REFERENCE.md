# NEOVIM KEYMAP REFERENCE

## LEADER KEY
Space (` `)

---

## CORE EDITING
- `jk` / `kj` — Exit insert mode *(handled by better-escape.nvim, zero lag)*
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
- `<leader>sm` — Maximize split
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
- `<leader>ef` — Find file in explorer
- `<leader>ec` — Collapse explorer
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
*(keymaps set on LspAttach in lsp.lua)*
- `gd` — Go to definition
- `gD` — Go to declaration
- `gi` — Go to implementation
- `gr` — Go to references
- `K` — Hover documentation
- `<leader>k` — Signature help
- `<leader>,a` — Code action (normal + visual)
- `<leader>,r` — Rename symbol
- `<leader>,f` — Format code (normal + visual)
- `<leader>,o` — Code outline (Aerial)
- `]d` / `[d` — Next/previous diagnostic
- `<leader>,d` — Show diagnostic float
- `<leader>,l` — Diagnostics list
- `<leader>,t` — Toggle diagnostics

---

## GIT
- `<leader>.g` — LazyGit
- `<leader>.b` — Git branches
- `<leader>.c` — Git commits
- `<leader>.s` — Git status
- `<leader>.d` — Git diff (Diffview)
- `<leader>.h` — File history (Diffview)
- `]h` / `[h` — Next/previous hunk
- `<leader>.p` — Preview hunk
- `<leader>.r` — Reset hunk

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
- `<leader>un` — Toggle line numbers
- `<leader>uz` — Zen mode
- `<leader>uu` — Undo tree

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
- `Ctrl+1`–`4` — Jump to file 1–4 (alternative)

---

## MISC UTILITIES
- `s` — Flash jump
- `]t` / `[t` — Next/previous todo
- `<leader>xc` — Copy file path
- `<leader>xr` — Copy relative path
- `<leader>xd` — Change to file directory
- `<leader>xe` — Make executable
- `<leader>xm` — Clean memory
- `<leader>xh` — Health check
- `<leader>xp` — Go to project root
- `<leader>xl` — Lazy (plugin manager)
- `<leader>xn` — Mason (LSP installer)
- `<leader>xx` — Trouble diagnostics

---

## LANGUAGE-SPECIFIC

### Python (`<leader>py*`)
- `<leader>pyv` — Select virtualenv
- `<leader>pyd` — Generate docstring
- `<leader>pydm` — Debug method
- `<leader>pydc` — Debug class
- `<leader>pyds` — Debug selection
- `<leader>pyrs` — REPL start
- `<leader>pyrr` — REPL restart
- `<leader>pyrc` — REPL send motion/visual
- `<leader>pyrl` — REPL send line

### Ruby (`<leader>rb*`)
- `<leader>rbn` — Test nearest
- `<leader>rbf` — Test file
- `<leader>rbs` — Test suite
- `<leader>rbl` — Test last
- `<leader>rbv` — Test visit

### Rust (`<leader>rs*`)
- `<leader>rsa` — Code action
- `<leader>rsd` — Debuggables
- `<leader>rsr` — Runnables
- `<leader>rse` — Expand macro
- `<leader>rsh` — Hover actions
- `<leader>rsc` — Toggle crates
- `<leader>rsu` — Update crate
- `<leader>rsU` — Upgrade crate

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
- `<leader>jvv` — Extract variable
- `<leader>jvc` — Extract constant
- `<leader>jvm` — Extract method
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
- `<leader>exn` — Test nearest
- `<leader>exf` — Test file
- `<leader>exa` — Test all
- `<leader>exs` — Test summary

### C++ / CMake (`<leader>cc*`)
- `<leader>ccg` — CMake generate
- `<leader>ccb` — CMake build
- `<leader>ccr` — CMake run
- `<leader>cct` — CMake test
- `<leader>ccc` — CMake clean
- `<leader>ccs` — CMake select target
- `<leader>ccd` — Generate docstring

### Fortran (`<leader>fo*`)
- `<leader>fob` — Build & run
- `<leader>foc` — Check syntax
- `<leader>fom` — Make

### Zig (`<leader>z*`)
- `<leader>zb` — Build run
- `<leader>zt` — Build test
- `<leader>zc` — Run file

### VHDL (`<leader>vh*`)  ← was `<leader>v*`
- `<leader>vha` — GHDL Analyze
- `<leader>vhe` — GHDL Elaborate
- `<leader>vhr` — GHDL Run & View
- `<leader>vhc` — GHDL Syntax Check

### COBOL (`<leader>co*`)  ← was `<leader>cb`
- `<leader>cob` — Compile & run
- `<leader>coc` — Syntax check

### Database (`<leader>db*`)
- `<leader>dbu` — Toggle DB UI
- `<leader>dba` — Add connection
- `<leader>dbf` — Find buffer

### REST (`<leader>h*`)
- `<leader>hr` — Run request
- `<leader>hp` — Preview request
- `<leader>hl` — Run last request
- `<leader>he` — Select env file

### Markdown (`<leader>md*`)
- `<leader>mdt` — Table mode toggle
- `<leader>mdf` — Table realign
- `<leader>mdp` — Paste image

---

## EMMET (HTML/CSS/JS/TS)
- `Ctrl+e` — Expand emmet abbreviation
