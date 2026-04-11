-- lua/core/keymaps.lua - Safe Keybindings
--
-- FIX (v2.2.4):
--   • flash.nvim specced in editor.lua; `s` map has clear fallback notify.
--
-- FIX (v2.3.3):
--   • <leader>xx was "<cmd>Trouble<cr>" — ":Trouble" with no subcommand is
--     not valid in trouble.nvim v3+; it requires a mode argument. Changed to
--     "<cmd>Trouble diagnostics toggle<cr>" matching the spec in ui.lua.
--   • <C-j>/<C-k> are mapped here to window navigation. completion.lua now
--     also maps <C-j>/<C-k> as blink.cmp aliases. These are insert-mode only
--     in blink (InsertEnter event) while the window nav maps are normal-mode
--     only — no conflict. Added a comment to make this explicit.
--
-- FIX (v2.3.1b):
--   • <leader>xg (Neogen) REMOVED from this file. It was added here in
--     v2.3.3 but advanced.lua already registers it in its keys= table.
--     keys= is the correct place: it handles lazy-loading and avoids a
--     double-registration that makes which-key show the same mapping twice.
--
-- FIX (v2.3.4):
--   • Removed duplicate keymaps that are already owned by plugin keys= specs:
--       <leader>ee/ef/ec/er  → editor.lua (neo-tree keys=)
--       <leader>uz           → hud.lua (zen-mode.nvim keys=)
--       <leader>uT           → hud.lua (twilight.nvim keys=)
--       <leader>eo           → hud.lua (oil.nvim keys=); "-" alias kept here
--       <leader>.p / .r      → git.lua (gitsigns on_attach, buffer-local)
--                              Global versions fired in non-git buffers — wrong.
--       <leader>.B           → hud.lua (blame.nvim keys=)
--   • <leader>ob overseer fallback block was copy-pasted from workflow.lua and
--     would silently diverge on future changes. Replaced with <cmd>OverseerRun<cr>
--     which is sufficient here; workflow.lua keys= handles the smart fallback.

local map = vim.keymap.set
local opts = { noremap = true, silent = true }

-- ============================================================================
-- BASIC EDITING
-- ============================================================================

map("v", "<", "<gv", opts)
map("v", ">", ">gv", opts)

map("n", "<A-j>", "<cmd>move .+1<cr>==", opts)
map("n", "<A-k>", "<cmd>move .-2<cr>==", opts)
map("x", "<A-j>", ":move '>+1<cr>gv=gv", opts)
map("x", "<A-k>", ":move '<-2<cr>gv=gv", opts)

map("n", "<Esc>", "<cmd>nohlsearch<cr>", opts)

-- ============================================================================
-- WINDOW MANAGEMENT
-- ============================================================================

map("n", "<leader>wq", "<cmd>wq<cr>",  { desc = "Save & Quit" })
map("n", "<leader>ww", "<cmd>w<cr>",   { desc = "Save" })
map("n", "<leader>qq", "<cmd>q<cr>",   { desc = "Quit" })
map("n", "<leader>qa", "<cmd>qa<cr>",  { desc = "Quit all" })

map("n", "<leader>sv", "<cmd>vsplit<cr>",          { desc = "Vertical split" })
map("n", "<leader>sh", "<cmd>split<cr>",           { desc = "Horizontal split" })
map("n", "<leader>se", "<C-w>=",                   { desc = "Equal splits" })
map("n", "<leader>sx", "<cmd>close<cr>",           { desc = "Close split" })
map("n", "<leader>sm", "<cmd>MaximizerToggle<cr>", { desc = "Maximize split" })

-- NOTE: <C-j>/<C-k> are normal-mode only here. completion.lua maps them in
-- insert-mode only (via blink.cmp's InsertEnter event). No conflict.
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
-- FIX: <leader>ee/ef/ec/er removed — owned by editor.lua (neo-tree keys=).
-- Registering them here too caused which-key to show each entry twice and
-- bypassed lazy-loading of neo-tree on first keypress.
-- ============================================================================

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

map("n", "<C-s>", "<cmd>Telescope live_grep<cr>", { desc = "Live grep" })

-- ============================================================================
-- GIT OPERATIONS
-- FIX: <leader>.p (preview_hunk) and <leader>.r (reset_hunk) removed.
--   git.lua's gitsigns on_attach registers these as BUFFER-LOCAL maps, which
--   is correct — they only exist inside git-tracked buffers.
--   The global versions here fired in every buffer (including non-git ones),
--   producing a no-op call to gitsigns outside git context.
-- FIX: <leader>.B (BlameToggle) removed — owned by hud.lua blame.nvim keys=.
-- ============================================================================

map("n", "<leader>.g", "<cmd>LazyGit<cr>",                { desc = "LazyGit" })
map("n", "<leader>.b", "<cmd>Telescope git_branches<cr>", { desc = "Git branches" })
map("n", "<leader>.c", "<cmd>Telescope git_commits<cr>",  { desc = "Git commits" })
map("n", "<leader>.s", "<cmd>Telescope git_status<cr>",   { desc = "Git status" })
map("n", "<leader>.d", "<cmd>DiffviewOpen<cr>",           { desc = "Git diff" })
map("n", "<leader>.h", "<cmd>DiffviewFileHistory<cr>",    { desc = "File history" })

-- ============================================================================
-- DEBUG (DAP)
-- ============================================================================

map("n", "<leader>;b", "<cmd>lua require('dap').toggle_breakpoint()<cr>",
  { desc = "Toggle breakpoint" })
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

map("n", "<F5>",  "<cmd>lua require('dap').continue()<cr>",         { desc = "Continue" })
map("n", "<F6>",  "<cmd>lua require('dap').toggle_breakpoint()<cr>",{ desc = "Toggle breakpoint" })
map("n", "<F7>",  "<cmd>lua require('dap').step_into()<cr>",        { desc = "Step into" })
map("n", "<F8>",  "<cmd>lua require('dap').step_over()<cr>",        { desc = "Step over" })
map("n", "<F9>",  "<cmd>lua require('dap').step_out()<cr>",         { desc = "Step out" })
map("n", "<F10>", "<cmd>lua require('dap').run_to_cursor()<cr>",    { desc = "Run to cursor" })
map("n", "<F11>", "<cmd>lua require('dap').terminate()<cr>",        { desc = "Terminate" })

-- ============================================================================
-- RUN & TEST
-- ============================================================================

map("n", "<leader>'r", "<cmd>lua require('core.util.runner').run_file()<cr>",
  { desc = "Run file" })

map("x", "<leader>'s",
  ":<C-u>lua require('core.util.runner').run_selection(vim.fn.line(\"'<\"), vim.fn.line(\"'>\"))<CR>",
  { desc = "Run selection" })

map("n", "<leader>'t", "<cmd>lua require('core.util.runner').run_tests()<cr>",
  { desc = "Run tests" })

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
-- FIX: <leader>uz (ZenMode) removed — owned by hud.lua zen-mode.nvim keys=.
-- FIX: <leader>uT (Twilight) removed — owned by hud.lua twilight.nvim keys=.
-- Both were causing which-key duplicates and bypassing lazy-load triggers.
-- ============================================================================

map("n", "<leader>ut", "<cmd>lua require('core.theme').toggle()<cr>",
  { desc = "Toggle theme" })
map("n", "<leader>uw", "<cmd>ToggleWrap<cr>",
  { desc = "Toggle wrap" })
map("n", "<leader>us", "<cmd>ToggleSpell<cr>",
  { desc = "Toggle spell" })
map("n", "<leader>ul", "<cmd>set number! relativenumber!<cr>",
  { desc = "Toggle line numbers" })

-- ============================================================================
-- SEARCH & REPLACE
-- ============================================================================

map("n", "<leader>/s", function() require("spectre").open() end,
  { desc = "Search & replace" })
map("n", "<leader>/w", function() require("spectre").open_visual({ select_word = true }) end,
  { desc = "Replace word" })
map("n", "<leader>/f", function() require("spectre").open_file_search() end,
  { desc = "Replace in file" })

-- ============================================================================
-- HARPOON
-- ============================================================================

map("n", "<leader>ha", function() require("harpoon"):list():add() end,
  { desc = "Harpoon add" })
map("n", "<leader>hm", function()
  require("harpoon").ui:toggle_quick_menu(require("harpoon"):list())
end, { desc = "Harpoon menu" })
map("n", "<leader>h1", function() require("harpoon"):list():select(1) end, { desc = "Harpoon 1" })
map("n", "<leader>h2", function() require("harpoon"):list():select(2) end, { desc = "Harpoon 2" })
map("n", "<leader>h3", function() require("harpoon"):list():select(3) end, { desc = "Harpoon 3" })
map("n", "<leader>h4", function() require("harpoon"):list():select(4) end, { desc = "Harpoon 4" })

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
    vim.notify(
      "flash.nvim not loaded — run :Lazy install and restart.",
      vim.log.levels.WARN
    )
  end
end, { desc = "Flash jump" })

-- ============================================================================
-- MISC UTILITIES
-- ============================================================================

map("n", "<leader>xc", "<cmd>CopyPath<cr>",      { desc = "Copy file path" })
map("n", "<leader>xr", "<cmd>CopyRelPath<cr>",   { desc = "Copy relative path" })
map("n", "<leader>xd", "<cmd>cd %:p:h<cr>",      { desc = "Change to file dir" })
map("n", "<leader>xe", "<cmd>!chmod +x %<cr>",   { desc = "Make executable" })
map("n", "<leader>xm", "<cmd>CleanUp<cr>",        { desc = "Clean memory" })
map("n", "<leader>xh", "<cmd>Health<cr>",         { desc = "Health check" })
map("n", "<leader>xp", "<cmd>ProjectRoot<cr>",    { desc = "Go to project root" })
map("n", "<leader>xl", "<cmd>Lazy<cr>",           { desc = "Lazy" })
map("n", "<leader>xn", "<cmd>Mason<cr>",          { desc = "Mason" })
map("n", "<leader>xx", "<cmd>Trouble diagnostics toggle<cr>", { desc = "Trouble diagnostics" })
map("n", "<leader>xu", "<cmd>UndotreeToggle<cr>", { desc = "Undo tree" })
-- NOTE: <leader>xg (Neogen) is intentionally NOT registered here.
-- It is registered in advanced.lua via keys= which also handles lazy-loading.
-- A duplicate here would cause which-key to show the same entry twice.

-- ============================================================================
-- TODO COMMENTS
-- ============================================================================

map("n", "]t", function() require("todo-comments").jump_next() end, { desc = "Next todo" })
map("n", "[t", function() require("todo-comments").jump_prev() end,  { desc = "Previous todo" })

-- ============================================================================
-- OVERSEER
-- FIX: <leader>ob fallback block was a copy-paste of workflow.lua logic.
--   Any future change to the fallback in workflow.lua would silently diverge.
--   Replaced with a plain OverseerRun which opens the picker — sufficient
--   for a global keymap. workflow.lua keys= handles the smart template logic.
-- ============================================================================

map("n", "<leader>ot", "<cmd>OverseerToggle<cr>", { desc = "Task list" })
map("n", "<leader>or", "<cmd>OverseerRun<cr>",    { desc = "Run task" })
map("n", "<leader>ob", "<cmd>OverseerRun<cr>",    { desc = "Build" })

-- ============================================================================
-- OIL
-- FIX: <leader>eo removed — owned by hud.lua oil.nvim keys=.
--   The "-" alias is kept here as a convenience binding that does not conflict.
-- ============================================================================

map("n", "-", "<cmd>Oil<cr>", { desc = "Open parent dir" })

-- ============================================================================
-- NOICE
-- ============================================================================

map("n", "<leader>un", "<cmd>Noice dismiss<cr>", { desc = "Dismiss notifications" })
map("n", "<leader>uN", "<cmd>Noice history<cr>", { desc = "Notification history" })

-- ============================================================================
-- FOCUS & TWILIGHT
-- ============================================================================

map("n", "<leader>uF", function() require("core.focus").toggle() end,
  { desc = "Deep focus mode" })
