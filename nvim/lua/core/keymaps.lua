-- lua/core/keymaps.lua - Safe Keybindings (No motion conflicts)
-- Strategy: Use punctuation keys after leader to avoid conflicts with vim motions

local map = vim.keymap.set
local opts = { noremap = true, silent = true }

-- ============================================================================
-- BASIC EDITING
-- ============================================================================

-- NOTE: jk/kj insert escape handled by better-escape.nvim (advanced.lua)
-- Native mappings removed to prevent double-handling and redundant timeoutlen lag.

-- Better indenting (keep selection)
map("v", "<", "<gv", opts)
map("v", ">", ">gv", opts)

-- Move lines (Alt+j/k)
map("n", "<A-j>", "<cmd>move .+1<cr>==", opts)
map("n", "<A-k>", "<cmd>move .-2<cr>==", opts)
-- Use "x" (visual only) not "v" (visual + select)
map("x", "<A-j>", ":move '>+1<cr>gv=gv", opts)
map("x", "<A-k>", ":move '<-2<cr>gv=gv", opts)

-- Clear search highlight
map("n", "<Esc>", "<cmd>nohlsearch<cr>", opts)

-- ============================================================================
-- WINDOW MANAGEMENT
-- ============================================================================

map("n", "<leader>wq", "<cmd>wq<cr>", { desc = "Save & Quit" })
map("n", "<leader>ww", "<cmd>w<cr>",  { desc = "Save" })
map("n", "<leader>qq", "<cmd>q<cr>",  { desc = "Quit" })
map("n", "<leader>qa", "<cmd>qa<cr>", { desc = "Quit all" })

map("n", "<leader>sv", "<cmd>vsplit<cr>",  { desc = "Vertical split" })
map("n", "<leader>sh", "<cmd>split<cr>",   { desc = "Horizontal split" })
map("n", "<leader>se", "<C-w>=",           { desc = "Equal splits" })
map("n", "<leader>sx", "<cmd>close<cr>",   { desc = "Close split" })
map("n", "<leader>sm", "<cmd>MaximizerToggle<cr>", { desc = "Maximize split" })

map("n", "<C-h>", "<C-w>h", opts)
map("n", "<C-j>", "<C-w>j", opts)
map("n", "<C-k>", "<C-w>k", opts)
map("n", "<C-l>", "<C-w>l", opts)

map("n", "<C-Up>",    "<cmd>resize +2<cr>",          opts)
map("n", "<C-Down>",  "<cmd>resize -2<cr>",          opts)
map("n", "<C-Left>",  "<cmd>vertical resize -2<cr>", opts)
map("n", "<C-Right>", "<cmd>vertical resize +2<cr>", opts)

-- ============================================================================
-- BUFFER MANAGEMENT
-- ============================================================================

map("n", "<leader>bn", "<cmd>bnext<cr>",   { desc = "Next buffer" })
map("n", "<leader>bp", "<cmd>bprev<cr>",   { desc = "Prev buffer" })
map("n", "<leader>bd", "<cmd>bdelete<cr>", { desc = "Delete buffer" })
map("n", "<leader>bo", "<cmd>BufOnly<cr>", { desc = "Delete other buffers" })
map("n", "]b", "<cmd>bnext<cr>", { desc = "Next buffer" })
map("n", "[b", "<cmd>bprev<cr>", { desc = "Prev buffer" })

-- ============================================================================
-- FILE EXPLORER
-- ============================================================================

map("n", "<leader>ee", "<cmd>NvimTreeToggle<cr>",   { desc = "Toggle explorer" })
map("n", "<leader>ef", "<cmd>NvimTreeFindFile<cr>", { desc = "Find file in explorer" })
map("n", "<leader>ec", "<cmd>NvimTreeCollapse<cr>", { desc = "Collapse explorer" })
map("n", "<leader>er", "<cmd>NvimTreeRefresh<cr>",  { desc = "Refresh explorer" })

-- ============================================================================
-- TELESCOPE FIND
-- ============================================================================

map("n", "<leader>ff", "<cmd>Telescope find_files<cr>",  { desc = "Find files" })
map("n", "<leader>fg", "<cmd>Telescope git_files<cr>",   { desc = "Find git files" })
map("n", "<leader>fw", "<cmd>Telescope live_grep<cr>",   { desc = "Find word (grep)" })
map("n", "<leader>fb", "<cmd>Telescope buffers<cr>",     { desc = "Find buffers" })
map("n", "<leader>fh", "<cmd>Telescope help_tags<cr>",   { desc = "Find help" })
map("n", "<leader>fm", "<cmd>Telescope marks<cr>",       { desc = "Find marks" })
map("n", "<leader>fk", "<cmd>Telescope keymaps<cr>",     { desc = "Find keymaps" })
map("n", "<leader>fc", "<cmd>Telescope commands<cr>",    { desc = "Find commands" })
map("n", "<leader>fr", "<cmd>TelescopeResume<cr>",       { desc = "Resume last search" })
map("n", "<leader>fo", "<cmd>Telescope oldfiles<cr>",    { desc = "Recent files" })

-- NOTE: <C-s> is XOFF in some terminals over SSH — may freeze in remote sessions.
map("n", "<C-s>", "<cmd>Telescope live_grep<cr>", { desc = "Live grep" })

-- ============================================================================
-- LSP & CODE ACTIONS
-- NOTE: All LSP buffer-local keymaps (gd, gD, gi, gr, K, <leader>k,
-- <leader>,*, ]d, [d) are defined in lsp.lua's LspAttach autocmd — sole owner.
-- ============================================================================

-- ============================================================================
-- GIT OPERATIONS
-- NOTE: ]h/[h hunk navigation is defined in git.lua's gitsigns on_attach —
-- sole owner. Defining them here caused double registration on every git buffer.
-- ============================================================================

map("n", "<leader>.g", "<cmd>LazyGit<cr>",                { desc = "LazyGit" })
map("n", "<leader>.b", "<cmd>Telescope git_branches<cr>", { desc = "Git branches" })
map("n", "<leader>.c", "<cmd>Telescope git_commits<cr>",  { desc = "Git commits" })
map("n", "<leader>.s", "<cmd>Telescope git_status<cr>",   { desc = "Git status" })
map("n", "<leader>.d", "<cmd>DiffviewOpen<cr>",           { desc = "Git diff" })
map("n", "<leader>.h", "<cmd>DiffviewFileHistory<cr>",    { desc = "File history" })
map("n", "<leader>.p", "<cmd>Gitsigns preview_hunk<cr>",  { desc = "Preview hunk" })
map("n", "<leader>.r", "<cmd>Gitsigns reset_hunk<cr>",    { desc = "Reset hunk" })

-- ============================================================================
-- DEBUG (DAP)
-- ============================================================================

map("n", "<leader>;b", "<cmd>lua require('dap').toggle_breakpoint()<cr>", { desc = "Toggle breakpoint" })
map("n", "<leader>;B", function()
  require("dap").set_breakpoint(vim.fn.input("Breakpoint condition: "))
end, { desc = "Conditional breakpoint" })
map("n", "<leader>;l", function()
  require("dap").set_breakpoint(nil, nil, vim.fn.input("Log point message: "))
end, { desc = "Log point" })

map("n", "<leader>;c", "<cmd>lua require('dap').continue()<cr>",       { desc = "Continue/Start" })
map("n", "<leader>;i", "<cmd>lua require('dap').step_into()<cr>",      { desc = "Step into" })
map("n", "<leader>;o", "<cmd>lua require('dap').step_over()<cr>",      { desc = "Step over" })
map("n", "<leader>;O", "<cmd>lua require('dap').step_out()<cr>",       { desc = "Step out" })
map("n", "<leader>;r", "<cmd>lua require('dap').repl.toggle()<cr>",    { desc = "Toggle REPL" })
map("n", "<leader>;L", "<cmd>lua require('dap').run_last()<cr>",       { desc = "Run last" })
map("n", "<leader>;t", "<cmd>lua require('dapui').toggle()<cr>",       { desc = "Toggle debug UI" })
map("n", "<leader>;x", "<cmd>lua require('dap').terminate()<cr>",      { desc = "Terminate debug" })
map("n", "<leader>;h", "<cmd>lua require('dap.ui.widgets').hover()<cr>",   { desc = "Debug hover" })
map("n", "<leader>;p", "<cmd>lua require('dap.ui.widgets').preview()<cr>", { desc = "Debug preview" })

map("n", "<F5>",  "<cmd>lua require('dap').continue()<cr>",        { desc = "Continue" })
map("n", "<F6>",  "<cmd>lua require('dap').toggle_breakpoint()<cr>",{ desc = "Toggle breakpoint" })
map("n", "<F7>",  "<cmd>lua require('dap').step_into()<cr>",        { desc = "Step into" })
map("n", "<F8>",  "<cmd>lua require('dap').step_over()<cr>",        { desc = "Step over" })
map("n", "<F9>",  "<cmd>lua require('dap').step_out()<cr>",         { desc = "Step out" })
map("n", "<F10>", "<cmd>lua require('dap').run_to_cursor()<cr>",    { desc = "Run to cursor" })
map("n", "<F11>", "<cmd>lua require('dap').terminate()<cr>",        { desc = "Terminate" })

-- ============================================================================
-- RUN & TEST
-- ============================================================================

map("n", "<leader>'r", "<cmd>lua require('core.util.runner').run_file()<cr>",     { desc = "Run file" })
map("x", "<leader>'s", "<cmd>lua require('core.util.runner').run_selection()<cr>",{ desc = "Run selection" })
map("n", "<leader>'t", "<cmd>lua require('core.util.runner').run_tests()<cr>",    { desc = "Run tests" })

map("n", "<leader>'n", function() require("neotest").run.run() end,                  { desc = "Test nearest" })
map("n", "<leader>'f", function() require("neotest").run.run(vim.fn.expand("%")) end, { desc = "Test file" })
map("n", "<leader>'a", function() require("neotest").run.run(vim.uv.cwd()) end,      { desc = "Test all" })
map("n", "<leader>'o", function() require("neotest").output.open() end,              { desc = "Test output" })
map("n", "<leader>'p", function() require("neotest").output_panel.toggle() end,      { desc = "Test panel" })
map("n", "<leader>'u", function() require("neotest").summary.toggle() end,           { desc = "Test summary" })

-- ============================================================================
-- TERMINAL
-- ============================================================================

map("n", "<leader>\\t", "<cmd>ToggleTerm<cr>",                      { desc = "Terminal" })
map("n", "<leader>\\f", "<cmd>ToggleTerm direction=float<cr>",      { desc = "Float terminal" })
map("n", "<leader>\\h", "<cmd>ToggleTerm direction=horizontal<cr>", { desc = "Horizontal terminal" })
map("n", "<leader>\\v", "<cmd>ToggleTerm direction=vertical<cr>",   { desc = "Vertical terminal" })
map("t", "<Esc>", "<C-\\><C-n>", opts)
map("n", "<C-\\>", "<cmd>ToggleTerm<cr>", opts)
map("t", "<C-\\>", "<cmd>ToggleTerm<cr>", opts)

-- ============================================================================
-- UI TOGGLES
-- ============================================================================

map("n", "<leader>ut", "<cmd>lua require('core.theme').toggle()<cr>", { desc = "Toggle theme" })
map("n", "<leader>uw", "<cmd>ToggleWrap<cr>",                         { desc = "Toggle wrap" })
map("n", "<leader>us", "<cmd>ToggleSpell<cr>",                        { desc = "Toggle spell" })
map("n", "<leader>un", "<cmd>set number! relativenumber!<cr>",        { desc = "Toggle line numbers" })
map("n", "<leader>uz", "<cmd>ZenMode<cr>",                            { desc = "Zen mode" })
-- NOTE: <leader>uu removed — undotree canonical binding is <leader>xu (advanced.lua)

-- ============================================================================
-- SEARCH & REPLACE
-- ============================================================================

map("n", "<leader>/s", function() require("spectre").open() end,                             { desc = "Search & replace" })
map("n", "<leader>/w", function() require("spectre").open_visual({ select_word = true }) end,{ desc = "Replace word" })
map("n", "<leader>/f", function() require("spectre").open_file_search() end,                 { desc = "Replace in file" })

-- ============================================================================
-- HARPOON
-- ============================================================================

map("n", "<leader>ha", function() require("harpoon"):list():add() end,                                    { desc = "Harpoon add" })
map("n", "<leader>hm", function() require("harpoon").ui:toggle_quick_menu(require("harpoon"):list()) end, { desc = "Harpoon menu" })
map("n", "<leader>h1", function() require("harpoon"):list():select(1) end, { desc = "Harpoon 1" })
map("n", "<leader>h2", function() require("harpoon"):list():select(2) end, { desc = "Harpoon 2" })
map("n", "<leader>h3", function() require("harpoon"):list():select(3) end, { desc = "Harpoon 3" })
map("n", "<leader>h4", function() require("harpoon"):list():select(4) end, { desc = "Harpoon 4" })

-- <M-number> (Alt+number) — widely supported vs <C-number> which most terminals don't transmit
map("n", "<M-1>", function() require("harpoon"):list():select(1) end, opts)
map("n", "<M-2>", function() require("harpoon"):list():select(2) end, opts)
map("n", "<M-3>", function() require("harpoon"):list():select(3) end, opts)
map("n", "<M-4>", function() require("harpoon"):list():select(4) end, opts)

-- ============================================================================
-- FLASH
-- ============================================================================

map({ "n", "x", "o" }, "s", function()
  local ok, flash = pcall(require, "flash")
  if ok and flash.jump then
    flash.jump()
  else
    vim.notify("Flash.nvim not available", vim.log.levels.WARN)
  end
end, { desc = "Flash jump" })

-- ============================================================================
-- MISC UTILITIES
-- ============================================================================

map("n", "<leader>xc", "<cmd>CopyPath<cr>",     { desc = "Copy file path" })
map("n", "<leader>xr", "<cmd>CopyRelPath<cr>",  { desc = "Copy relative path" })
map("n", "<leader>xd", "<cmd>cd %:p:h<cr>",     { desc = "Change to file dir" })
map("n", "<leader>xe", "<cmd>!chmod +x %<cr>",  { desc = "Make executable" })
map("n", "<leader>xm", "<cmd>CleanUp<cr>",       { desc = "Clean memory" })
map("n", "<leader>xh", "<cmd>Health<cr>",        { desc = "Health check" })
map("n", "<leader>xp", "<cmd>ProjectRoot<cr>",   { desc = "Go to project root" })
map("n", "<leader>xl", "<cmd>Lazy<cr>",          { desc = "Lazy" })
map("n", "<leader>xn", "<cmd>Mason<cr>",         { desc = "Mason" })
map("n", "<leader>xx", "<cmd>Trouble<cr>",       { desc = "Trouble diagnostics" })
map("n", "<leader>xu", "<cmd>UndotreeToggle<cr>",{ desc = "Undo tree" })

-- ============================================================================
-- TODO COMMENTS
-- ============================================================================

map("n", "]t", function() require("todo-comments").jump_next() end, { desc = "Next todo" })
map("n", "[t", function() require("todo-comments").jump_prev() end,  { desc = "Previous todo" })

-- ============================================================================
-- LANGUAGE-SPECIFIC
-- These are set in language-specific plugin files:
-- Python: <leader>py*  Rust: <leader>rs*  Go: <leader>go*  Java: <leader>jv*
-- ============================================================================
