# KEYMAP REFERENCE — v2.3.16

Leader: `Space`

---

## Editing

| Key | Action |
|-----|--------|
| `jk` / `kj` | Exit insert mode |
| `<Esc>` | Clear search highlight |
| `Alt+j` / `Alt+k` | Move line down / up |
| `<` / `>` (visual) | Indent left / right, keep selection |

---

## Windows & Buffers

| Key | Action |
|-----|--------|
| `<leader>ww` | Save |
| `<leader>wq` | Save and quit |
| `<leader>qq` | Quit |
| `<leader>qa` | Quit all |
| `<leader>sv` | Vertical split |
| `<leader>sh` | Horizontal split |
| `<leader>se` | Equal splits |
| `<leader>sx` | Close split |
| `<leader>sm` | Maximize / restore split |
| `Ctrl+h/j/k/l` | Navigate splits |
| `Ctrl+arrows` | Resize splits |
| `<leader>bn` / `]b` | Next buffer |
| `<leader>bp` / `[b` | Previous buffer |
| `<leader>bd` | Delete buffer |
| `<leader>bo` | Delete other buffers |

---

## Find (Telescope)

| Key | Action |
|-----|--------|
| `<leader>ff` | Find files |
| `<leader>fg` | Git files |
| `<leader>fw` | Live grep |
| `<leader>fb` | Buffers |
| `<leader>fh` | Help tags |
| `<leader>fm` | Marks |
| `<leader>fk` | Keymaps |
| `<leader>fc` | Commands |
| `<leader>fo` | Recent files |
| `<leader>fr` | Resume last search |
| `Ctrl+s` | Live grep |

---

## Explorer

| Key | Action |
|-----|--------|
| `<leader>ee` | Toggle |
| `<leader>ef` | Focus |
| `<leader>ec` | Close |
| `<leader>er` | Refresh |
| `<leader>eo` | Oil file editor |
| `-` | Oil (parent dir) |

---

## LSP

> Set on `LspAttach` per buffer.

| Key | Action |
|-----|--------|
| `gd` | Definition |
| `gD` | Declaration |
| `gi` | Implementation |
| `gr` | References |
| `K` | Hover docs |
| `<leader>k` | Signature help |
| `<leader>,a` | Code action |
| `<leader>,r` | Rename symbol |
| `<leader>,f` | Format |
| `<leader>,o` | Outline (Trouble symbols) |
| `<leader>,i` | Toggle inlay hints |
| `<leader>,d` | Diagnostic float |
| `<leader>,l` | Diagnostic list |
| `<leader>,t` | Toggle diagnostics |
| `<leader>ty` | Type definition |
| `]d` / `[d` | Next / prev diagnostic |

---

## Git

| Key | Action |
|-----|--------|
| `<leader>.g` | LazyGit |
| `<leader>.b` | Branches |
| `<leader>.c` | Commits |
| `<leader>.s` | Status |
| `<leader>.d` | Diff (Diffview) |
| `<leader>.h` | File history (Diffview) |
| `<leader>.N` | Neogit |
| `<leader>.C` | Commit (Neogit) |
| `<leader>.v` | Commit graph (GV) |
| `<leader>.B` | Blame virtual text |
| `<leader>.oi` | GitHub issues (Octo) |
| `<leader>.op` | GitHub PRs (Octo) |
| `<leader>.or` | Start PR review (Octo) |
| `<leader>.oc` | Checkout PR (Octo) |
| `]h` / `[h` | Next / prev hunk |
| `<leader>.p` | Preview hunk ¹ |
| `<leader>.r` | Reset hunk ¹ |
| `<leader>.S` | Stage hunk ¹ |
| `<leader>gco` | Conflict: choose ours |
| `<leader>gct` | Conflict: choose theirs |
| `<leader>gcb` | Conflict: choose both |
| `<leader>gc0` | Conflict: choose neither |

> ¹ Buffer-local, git files only.

---

## Debug (DAP)

| Key | Action |
|-----|--------|
| `F5` | Continue / start |
| `F6` | Toggle breakpoint |
| `F7` | Step into |
| `F8` | Step over |
| `F9` | Step out |
| `F10` | Run to cursor |
| `F11` | Terminate |
| `<leader>;b` | Toggle breakpoint |
| `<leader>;B` | Conditional breakpoint |
| `<leader>;l` | Log point |
| `<leader>;c` | Continue |
| `<leader>;i` | Step into |
| `<leader>;o` | Step over |
| `<leader>;O` | Step out |
| `<leader>;r` | Toggle REPL |
| `<leader>;L` | Run last |
| `<leader>;t` | Toggle DAP UI |
| `<leader>;x` | Terminate |
| `<leader>;h` | Hover |
| `<leader>;p` | Preview |

---

## Run & Test

| Key | Action |
|-----|--------|
| `<leader>'r` | Run file |
| `<leader>'s` (visual) | Run selection |
| `<leader>'t` | Run tests |
| `<leader>'n` | Neotest nearest |
| `<leader>'f` | Neotest file |
| `<leader>'a` | Neotest all |
| `<leader>'o` | Test output |
| `<leader>'p` | Test panel |
| `<leader>'u` | Test summary |
| `<leader>'d` | Test debug nearest |
| `<leader>'P` | Test all (parallel, concurrency=4) |
| `<leader>'w` | Watch file |
| `<leader>'W` | Watch nearest |
| `<leader>tcv` | Coverage load |
| `<leader>tcs` | Coverage summary |
| `<leader>tct` | Coverage toggle |

---

## Terminal

| Key | Action |
|-----|--------|
| `<leader>\\t` | Open terminal |
| `<leader>\\f` | Float terminal |
| `<leader>\\h` | Horizontal terminal |
| `<leader>\\v` | Vertical terminal |
| `Ctrl+\` | Toggle terminal |
| `Esc` (terminal mode) | Exit terminal mode |

---

## UI

| Key | Action |
|-----|--------|
| `<leader>ut` | Toggle dark / light theme |
| `<leader>uw` | Toggle wrap |
| `<leader>us` | Toggle spell |
| `<leader>ul` | Toggle line numbers |
| `<leader>uz` | Zen mode |
| `<leader>uF` | Deep focus mode |
| `<leader>uT` | Twilight |
| `<leader>un` | Dismiss notifications |
| `<leader>uN` | Notification history |
| `<leader>uc` | Go to treesitter context start |

---

## Search & Replace

| Key | Action |
|-----|--------|
| `<leader>/s` | Search & replace (Spectre) |
| `<leader>/w` | Replace word under cursor |
| `<leader>/f` | Replace in file |

---

## Harpoon

| Key | Action |
|-----|--------|
| `<leader>ha` | Add file |
| `<leader>hm` | Toggle menu |
| `<leader>h1`–`h4` | Jump to file 1–4 |
| `Alt+1`–`4` | Jump to file 1–4 |

---

## Utilities

| Key | Action |
|-----|--------|
| `s` | Flash jump |
| `]t` / `[t` | Next / prev TODO |
| `<leader>xc` | Copy absolute path |
| `<leader>xr` | Copy relative path |
| `<leader>xd` | CD to file directory |
| `<leader>xe` | Make executable |
| `<leader>xm` | GC memory |
| `<leader>xh` | Health check |
| `<leader>xp` | LCD to project root |
| `<leader>xl` | Lazy |
| `<leader>xn` | Mason |
| `<leader>xx` | Trouble diagnostics |
| `<leader>xX` | Trouble buffer diagnostics |
| `<leader>xL` | Trouble location list |
| `<leader>xQ` | Trouble quickfix |
| `<leader>xu` | Undo tree |
| `<leader>xg` | Generate docstring (Neogen) |
| `<leader>xT` | Find TODOs (Telescope) |

---

## Sessions

| Key | Action |
|-----|--------|
| `<leader>qs` | Restore session (cwd) |
| `<leader>ql` | Restore last session |
| `<leader>qd` | Don't save on exit |

---

## Task Runner (Overseer)

| Key | Action |
|-----|--------|
| `<leader>ot` | Toggle task list |
| `<leader>or` | Run task |
| `<leader>ob` | Build |
| `<leader>oa` | Task action |
| `<leader>oc` | Clear cache |
| `<leader>os` | Shell command |

---

## Treesitter Text Objects

| Key | Action |
|-----|--------|
| `af` / `if` | Function outer / inner |
| `ac` / `ic` | Class outer / inner |
| `al` / `il` | Loop outer / inner |
| `aa` / `ia` | Parameter outer / inner |
| `ai` / `ii` | Conditional outer / inner |
| `]f` / `[f` | Next / prev function start |
| `]c` / `[c` | Next / prev class start |
| `]a` / `[a` | Next / prev parameter start |
| `<leader>sa` / `<leader>sA` | Swap parameter next / prev |

---

## Language-Specific

### Python `<leader>py*`

| Key | Action |
|-----|--------|
| `<leader>pyv` | Select virtualenv |
| `<leader>pyg` | Generate docstring |
| `<leader>pydm` | Debug method |
| `<leader>pydc` | Debug class |
| `<leader>pyds` | Debug selection |
| `<leader>pyrs` | REPL start |
| `<leader>pyrr` | REPL restart |
| `<leader>pyrc` | REPL send motion |
| `<leader>pyrv` | REPL send visual |
| `<leader>pyrl` | REPL send line |
| `<leader>pyru` | REPL send until cursor |
| `<leader>pyri` | REPL interrupt |
| `<leader>pyrq` | REPL quit |
| `<leader>pyrx` | REPL clear |

### Rust `<leader>r*`

| Key | Action |
|-----|--------|
| `<leader>rh` | Hover actions |
| `<leader>ra` | Code action |
| `<leader>rd` | Debuggables |
| `<leader>rt` | Testables |

### Go `<leader>go*`

| Key | Action |
|-----|--------|
| `<leader>got` | Test |
| `<leader>gof` | Test function |
| `<leader>goc` | Coverage |
| `<leader>gor` | Run |
| `<leader>gob` | Build |
| `<leader>goi` | Impl |
| `<leader>goa` | Add tag |
| `<leader>gom` | Mod |

### TypeScript `<leader>ts*`

| Key | Action |
|-----|--------|
| `<leader>tso` | Organize imports |
| `<leader>tsi` | Add missing imports |
| `<leader>tsr` | Remove unused imports |
| `<leader>tsf` | Fix all |
| `<leader>tsd` | Go to source definition |

### JavaScript packages `<leader>jp*`

| Key | Action |
|-----|--------|
| `<leader>jps` | Show versions |
| `<leader>jpu` | Update package |
| `<leader>jpd` | Delete package |
| `<leader>jpi` | Install package |
| `<leader>jpc` | Change version |

### Java `<leader>jv*`

| Key | Action |
|-----|--------|
| `<leader>jvo` | Organize imports |
| `<leader>jvv` | Extract variable |
| `<leader>jvc` | Extract constant |
| `<leader>jvm` | Extract method (visual) |
| `<leader>jvt` | Test class |
| `<leader>jvn` | Test nearest method |
| `<leader>jvg` | Generate Javadoc |

### Kotlin `<leader>kt*`

| Key | Action |
|-----|--------|
| `<leader>ktb` | Build |
| `<leader>ktt` | Test |
| `<leader>ktr` | Run |

### Ruby `<leader>rb*`

| Key | Action |
|-----|--------|
| `<leader>rbn` | Test nearest |
| `<leader>rbf` | Test file |
| `<leader>rbs` | Test suite |
| `<leader>rbl` | Test last |
| `<leader>rbv` | Test visit |

### Elixir `<leader>ex*`

| Key | Action |
|-----|--------|
| `<leader>ext` | mix test |
| `<leader>exf` | mix format |
| `<leader>exp` | Phoenix server |
| `<leader>exi` | IEx session |

### C `<leader>c*`

| Key | Action |
|-----|--------|
| `<leader>cb` | Build & run (gcc) |
| `<leader>cm` | Make |
| `<leader>csy` | Syntax check |

### C++ / CMake `<leader>cc*`

| Key | Action |
|-----|--------|
| `<leader>ccg` | CMake generate |
| `<leader>ccb` | CMake build |
| `<leader>ccr` | CMake run |
| `<leader>cct` | CMake test |
| `<leader>ccc` | CMake clean |
| `<leader>ccs` | CMake select target |
| `<leader>ccd` | Generate docstring |

### Fortran `<leader>ft*`

| Key | Action |
|-----|--------|
| `<leader>ftb` | Build & run |
| `<leader>ftc` | Syntax check |
| `<leader>ftm` | Make |

### Zig `<leader>z*`

| Key | Action |
|-----|--------|
| `<leader>zb` | Build run |
| `<leader>zt` | Build test |
| `<leader>zc` | Run file |

### VHDL `<leader>vh*`

| Key | Action |
|-----|--------|
| `<leader>vha` | GHDL analyze |
| `<leader>vhe` | GHDL elaborate |
| `<leader>vhr` | GHDL run & view |
| `<leader>vhc` | GHDL syntax check |

### COBOL `<leader>co*`

| Key | Action |
|-----|--------|
| `<leader>cob` | Compile & run |
| `<leader>coc` | Syntax check |

### Database `<leader>db*`

| Key | Action |
|-----|--------|
| `<leader>dbu` | Toggle DB UI |
| `<leader>dba` | Add connection |
| `<leader>dbf` | Find buffer |

### REST `<leader>re*`

| Key | Action |
|-----|--------|
| `<leader>rer` | Run request |
| `<leader>rep` | Preview request |
| `<leader>rel` | Run last request |
| `<leader>ree` | Select env file |

### Markdown

| Key | Action |
|-----|--------|
| `<leader>mp` | Preview toggle |
| `<leader>tm` | Table mode toggle |

### Emmet (HTML / CSS / JS / TS)

| Key | Action |
|-----|--------|
| `Ctrl+e` | Expand abbreviation |
