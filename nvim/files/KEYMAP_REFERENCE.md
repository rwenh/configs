# NEOVIM KEYMAP REFERENCE

## LEADER KEY
Space (` `)

## CORE EDITING
- `jk` / `kj` - Exit insert mode
- `<Esc>` - Clear search highlight
- `Alt+j` / `Alt+k` - Move lines up/down
- `<` / `>` (visual) - Indent left/right (keeps selection)

## WINDOW MANAGEMENT
- `<leader>ww` - Save file
- `<leader>wq` - Save and quit
- `<leader>qq` - Quit
- `<leader>qa` - Quit all
- `<leader>sv` - Vertical split
- `<leader>sh` - Horizontal split
- `<leader>se` - Equal splits
- `<leader>sx` - Close split
- `<leader>sm` - Maximize split
- `Ctrl+h/j/k/l` - Navigate splits
- `Ctrl+arrows` - Resize splits

## BUFFERS
- `<leader>bn` / `]b` - Next buffer
- `<leader>bp` / `[b` - Previous buffer
- `<leader>bd` - Delete buffer
- `<leader>bo` - Delete other buffers

## FILE EXPLORER
- `<leader>ee` - Toggle explorer
- `<leader>ef` - Find file in explorer
- `<leader>ec` - Collapse explorer
- `<leader>er` - Refresh explorer

## TELESCOPE (FIND)
- `<leader>ff` - Find files
- `<leader>fg` - Find git files
- `<leader>fw` - Find word (grep)
- `<leader>fb` - Find buffers
- `<leader>fh` - Find help
- `<leader>fm` - Find marks
- `<leader>fk` - Find keymaps
- `<leader>fc` - Find commands
- `<leader>fr` - Resume last search
- `<leader>fo` - Recent files
- `Ctrl+s` - Live grep

## LSP & CODE
- `gd` - Go to definition
- `gD` - Go to declaration
- `gi` - Go to implementation
- `gr` - Go to references
- `K` - Hover documentation
- `<leader>ca` - Code action
- `<leader>rn` - Rename symbol
- `<leader>cf` - Format code
- `<leader>o` - Code outline
- `]d` / `[d` - Next/previous diagnostic
- `<leader>cd` - Show diagnostic
- `<leader>cl` - Diagnostics list
- `<leader>ct` - Toggle diagnostics

## GIT
- `<leader>gg` - LazyGit
- `<leader>gb` - Git branches
- `<leader>gc` - Git commits
- `<leader>gs` - Git status
- `<leader>gd` - Git diff
- `<leader>gh` - File history
- `]h` / `[h` - Next/previous hunk
- `<leader>gp` - Preview hunk
- `<leader>gr` - Reset hunk

## DEBUG (DAP)
- `F5` - Continue/Start
- `F6` - Toggle breakpoint
- `F7` - Step into
- `F8` - Step over
- `F9` - Step out
- `F10` - Run to cursor
- `F11` - Terminate
- `<leader>db` - Toggle breakpoint
- `<leader>dB` - Conditional breakpoint
- `<leader>dc` - Continue
- `<leader>di` - Step into
- `<leader>do` - Step over
- `<leader>dO` - Step out
- `<leader>dr` - Toggle REPL
- `<leader>dl` - Run last
- `<leader>dt` - Toggle debug UI
- `<leader>dx` - Terminate
- `<leader>dh` - Debug hover
- `<leader>dp` - Debug preview

## RUN & TEST
- `<leader>rr` - Run file
- `<leader>rs` (visual) - Run selection
- `<leader>rt` - Run tests
- `<leader>tn` - Test nearest
- `<leader>tf` - Test file
- `<leader>ta` - Test all
- `<leader>to` - Test output
- `<leader>tp` - Test panel
- `<leader>ts` - Test summary

## TERMINAL
- `<leader>tt` - Open terminal
- `<leader>tf` - Float terminal
- `<leader>th` - Horizontal terminal
- `<leader>tv` - Vertical terminal
- `Ctrl+\` - Toggle terminal
- `Esc` (in terminal) - Exit terminal mode

## UI TOGGLES
- `<leader>ut` - Toggle theme
- `<leader>uw` - Toggle wrap
- `<leader>us` - Toggle spell
- `<leader>un` - Toggle line numbers
- `<leader>uz` - Zen mode
- `<leader>u` - Undo tree

## SEARCH & REPLACE
- `<leader>sr` - Search & replace
- `<leader>sw` - Replace word
- `<leader>sf` - Replace in file

## HARPOON
- `<leader>ha` - Add file
- `<leader>hm` - Toggle menu
- `<leader>h1-4` - Jump to file 1-4

## MISC
- `s` - Flash jump
- `]t` / `[t` - Next/previous todo
- `<leader>xc` - Copy file path
- `<leader>xr` - Copy relative path
- `<leader>xd` - Change to file directory
- `<leader>xe` - Make executable
- `<leader>xm` - Clean memory
- `<leader>xh` - Health check
- `<leader>xp` - Go to project root
- `<leader>xl` - Lazy (plugin manager)
- `<leader>xn` - Mason (LSP installer)
- `<leader>xx` - Trouble diagnostics

## LANGUAGE-SPECIFIC
- Python: `<leader>vs` - Select virtualenv
- Ruby: `<leader>rb*` - Ruby test commands
- Rust: `<leader>rs*` - Rust tools
- Go: `<leader>go*` - Go tools
- Java: `<leader>jv*` - Java tools
- Elixir: `<leader>ex*` - Elixir test commands
- Zig: `<leader>zb` - Zig build, `<leader>zt` - Zig test
- VHDL: `<leader>va` - Analyze, `<leader>ve` - Elaborate, `<leader>vr` - Run
- COBOL: `<leader>cb` - Compile & run

## EMMET (HTML/CSS)
- `Ctrl+y,` - Expand emmet (note: changed from Ctrl+e to avoid conflicts)
