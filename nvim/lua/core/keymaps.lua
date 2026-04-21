-- lua/core/keymaps.lua - Safe Keybindings
--
-- OPT (v2.3.13):
--   • DAP <leader>;* maps consolidated from 13 hand-rolled blocks into a
--     single declarative table + one loop.  The dap_fn() factory is now
--     used for ALL dap/dapui/widgets calls uniformly (F-keys + leader).
--   • Spectre maps consolidated: 3 hand-rolled blocks → 1 factory + table.
--   • Todo-comments maps consolidated: 2 hand-rolled blocks → 1 factory.
--   • Removed empty section headers that contained only FIX comments and
--     no active code (FILE EXPLORER, OVERSEER).

local map = vim.keymap.set
local opts = { noremap = true, silent = true }

-- ── Lazy-require factory ────────────────────────────────────────────────────
-- Returns a function that pcall-requires `mod`, then calls `fn(loaded_mod)`.
-- On failure it notifies with `tag` and does nothing else.
local function lazy(mod, tag)
  return function(fn)
    return function()
      local ok, m = pcall(require, mod)
      if ok then pcall(fn, m)
      else vim.notify(tag .. " not loaded", vim.log.levels.WARN) end
    end
  end
end

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

map("n", "<leader>sv", "<cmd>vsplit<cr>", { desc = "Vertical split" })
map("n", "<leader>sh", "<cmd>split<cr>",  { desc = "Horizontal split" })
map("n", "<leader>se", "<C-w>=",          { desc = "Equal splits" })
map("n", "<leader>sx", "<cmd>close<cr>",  { desc = "Close split" })

-- Native maximize toggle — no plugin dependency (v2.3.5)
map("n", "<leader>sm", function()
  if vim.w._maximized then
    vim.w._maximized = false
    vim.cmd("wincmd =")
  else
    vim.w._maximized = true
    vim.cmd("wincmd | wincmd _")
  end
end, { desc = "Maximize / restore split" })

-- NOTE: <C-j>/<C-k> are normal-mode only. completion.lua maps them in insert
-- mode via blink.cmp's InsertEnter event — no conflict.
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
map("n", "<C-s>",      "<cmd>Telescope live_grep<cr>",   { desc = "Live grep" })

-- ============================================================================
-- GIT OPERATIONS
-- NOTE: <leader>.p/.r/.S are BUFFER-LOCAL in git.lua (gitsigns on_attach).
--       <leader>.B is owned by hud.lua (blame.nvim keys=).
-- ============================================================================

map("n", "<leader>.g", "<cmd>LazyGit<cr>",                { desc = "LazyGit" })
map("n", "<leader>.b", "<cmd>Telescope git_branches<cr>", { desc = "Git branches" })
map("n", "<leader>.c", "<cmd>Telescope git_commits<cr>",  { desc = "Git commits" })
map("n", "<leader>.s", "<cmd>Telescope git_status<cr>",   { desc = "Git status" })
map("n", "<leader>.d", "<cmd>DiffviewOpen<cr>",           { desc = "Git diff" })
map("n", "<leader>.h", "<cmd>DiffviewFileHistory<cr>",    { desc = "File history" })

-- ============================================================================
-- DEBUG (DAP)  — consolidated table-driven registration (v2.3.13)
--
-- Three module families: "dap", "dapui", "dap.ui.widgets".
-- Special cases (;B, ;l, ;r) need custom args — they are listed separately
-- after the table loop.
-- ============================================================================

-- Generic safe-call helpers for each DAP module family
local dap_call     = lazy("dap",            "[dap] nvim-dap")
local dapui_call   = lazy("dapui",          "[dap] nvim-dap-ui")
local widget_call  = lazy("dap.ui.widgets", "[dap] nvim-dap")

-- Simple method dispatch: key → { module_family, method_or_fn, desc }
-- module families: "d"=dap  "u"=dapui  "w"=widgets
local _dap_maps = {
  { "<leader>;b", "d", function(d) d.toggle_breakpoint() end,  "Toggle breakpoint" },
  { "<leader>;c", "d", function(d) d.continue() end,           "Continue/Start" },
  { "<leader>;i", "d", function(d) d.step_into() end,          "Step into" },
  { "<leader>;o", "d", function(d) d.step_over() end,          "Step over" },
  { "<leader>;O", "d", function(d) d.step_out() end,           "Step out" },
  { "<leader>;L", "d", function(d) d.run_last() end,           "Run last" },
  { "<leader>;x", "d", function(d) d.terminate() end,          "Terminate debug" },
  { "<leader>;t", "u", function(u) u.toggle() end,             "Toggle debug UI" },
  { "<leader>;h", "w", function(w) w.hover() end,              "Debug hover" },
  { "<leader>;p", "w", function(w) w.preview() end,            "Debug preview" },
}

local _family = { d = dap_call, u = dapui_call, w = widget_call }
for _, spec in ipairs(_dap_maps) do
  local lhs, fam, fn, desc = spec[1], spec[2], spec[3], spec[4]
  map("n", lhs, _family[fam](fn), { desc = desc })
end

-- Special cases: require user input before the dap call
map("n", "<leader>;B", function()
  local ok, d = pcall(require, "dap")
  if ok then pcall(d.set_breakpoint, vim.fn.input("Breakpoint condition: "))
  else vim.notify("[dap] nvim-dap not loaded", vim.log.levels.WARN) end
end, { desc = "Conditional breakpoint" })

map("n", "<leader>;l", function()
  local ok, d = pcall(require, "dap")
  if ok then pcall(d.set_breakpoint, nil, nil, vim.fn.input("Log point message: "))
  else vim.notify("[dap] nvim-dap not loaded", vim.log.levels.WARN) end
end, { desc = "Log point" })

map("n", "<leader>;r", function()
  local ok, d = pcall(require, "dap")
  if ok then pcall(function() d.repl.toggle() end)
  else vim.notify("[dap] nvim-dap not loaded", vim.log.levels.WARN) end
end, { desc = "Toggle REPL" })

-- F-keys reuse dap_call helper
local _fkey_maps = {
  { "<F5>",  "continue"         },
  { "<F6>",  "toggle_breakpoint"},
  { "<F7>",  "step_into"        },
  { "<F8>",  "step_over"        },
  { "<F9>",  "step_out"         },
  { "<F10>", "run_to_cursor"    },
  { "<F11>", "terminate"        },
}
for _, spec in ipairs(_fkey_maps) do
  local lhs, method = spec[1], spec[2]
  map("n", lhs, dap_call(function(d) d[method]() end), { desc = method:gsub("_", " ") })
end

-- ============================================================================
-- RUN & TEST
-- ============================================================================

map("n", "<leader>'r", function()
  pcall(function() require("core.util.runner").run_file() end)
end, { desc = "Run file" })

map("x", "<leader>'s", function()
  local s = vim.fn.line("'<")
  local e = vim.fn.line("'>")
  pcall(function() require("core.util.runner").run_selection(s, e) end)
end, { desc = "Run selection" })

map("n", "<leader>'t", function()
  pcall(function() require("core.util.runner").run_tests() end)
end, { desc = "Run tests" })

-- ============================================================================
-- TERMINAL
-- ============================================================================

map("n", "<leader>\\t", "<cmd>ToggleTerm<cr>",                      { desc = "Terminal" })
map("n", "<leader>\\f", "<cmd>ToggleTerm direction=float<cr>",      { desc = "Float terminal" })
map("n", "<leader>\\h", "<cmd>ToggleTerm direction=horizontal<cr>", { desc = "Horizontal terminal" })
map("n", "<leader>\\v", "<cmd>ToggleTerm direction=vertical<cr>",   { desc = "Vertical terminal" })
map("t", "<Esc>",  "<C-\\><C-n>",      opts)
map("n", "<C-\\>", "<cmd>ToggleTerm<cr>", opts)
map("t", "<C-\\>", "<cmd>ToggleTerm<cr>", opts)

-- ============================================================================
-- UI TOGGLES
-- NOTE: <leader>uz (ZenMode) owned by hud.lua zen-mode.nvim keys=.
--       <leader>uT (Twilight)  owned by hud.lua twilight.nvim keys=.
-- ============================================================================

map("n", "<leader>ut", "<cmd>lua require('core.theme').toggle()<cr>", { desc = "Toggle theme" })
map("n", "<leader>uw", "<cmd>ToggleWrap<cr>",                          { desc = "Toggle wrap" })
map("n", "<leader>us", "<cmd>ToggleSpell<cr>",                         { desc = "Toggle spell" })
map("n", "<leader>ul", "<cmd>set number! relativenumber!<cr>",         { desc = "Toggle line numbers" })

-- ============================================================================
-- SEARCH & REPLACE  — factory-driven (v2.3.13)
-- ============================================================================

local _spectre_maps = {
  { "<leader>/s", function(s) s.open() end,                          "Search & replace" },
  { "<leader>/w", function(s) s.open_visual({ select_word = true }) end, "Replace word" },
  { "<leader>/f", function(s) s.open_file_search() end,              "Replace in file" },
}
local spectre_call = lazy("spectre", "[spectre] not loaded — try :Lazy load nvim-spectre")
for _, spec in ipairs(_spectre_maps) do
  map("n", spec[1], spectre_call(spec[2]), { desc = spec[3] })
end

-- ============================================================================
-- HARPOON
-- ============================================================================

local function harpoon_call(fn)
  return function()
    local ok, h = pcall(require, "harpoon")
    if ok then pcall(fn, h)
    else vim.notify("[harpoon] not loaded — try :Lazy load harpoon", vim.log.levels.WARN) end
  end
end

map("n", "<leader>ha", harpoon_call(function(h) h:list():add() end),               { desc = "Harpoon add" })
map("n", "<leader>hm", harpoon_call(function(h) h.ui:toggle_quick_menu(h:list()) end), { desc = "Harpoon menu" })
for i = 1, 4 do
  map("n", "<leader>h" .. i, harpoon_call(function(h) h:list():select(i) end), { desc = "Harpoon " .. i })
  map("n", "<M-" .. i .. ">", harpoon_call(function(h) h:list():select(i) end), opts)
end

-- ============================================================================
-- FLASH
-- ============================================================================

map({ "n", "x", "o" }, "s", function()
  local ok, flash = pcall(require, "flash")
  if ok and flash.jump then flash.jump()
  else vim.notify("flash.nvim not loaded — run :Lazy install and restart.", vim.log.levels.WARN) end
end, { desc = "Flash jump" })

-- ============================================================================
-- MISC UTILITIES
-- ============================================================================

map("n", "<leader>xc", "<cmd>CopyPath<cr>",    { desc = "Copy file path" })
map("n", "<leader>xr", "<cmd>CopyRelPath<cr>", { desc = "Copy relative path" })
map("n", "<leader>xd", "<cmd>cd %:p:h<cr>",    { desc = "Change to file dir" })
map("n", "<leader>xe", "<cmd>!chmod +x %<cr>", { desc = "Make executable" })
map("n", "<leader>xm", "<cmd>CleanUp<cr>",     { desc = "Clean memory" })
map("n", "<leader>xh", "<cmd>Health<cr>",      { desc = "Health check" })
map("n", "<leader>xp", "<cmd>ProjectRoot<cr>", { desc = "Go to project root" })
map("n", "<leader>xl", "<cmd>Lazy<cr>",        { desc = "Lazy" })
map("n", "<leader>xn", "<cmd>Mason<cr>",       { desc = "Mason" })
map("n", "<leader>xx", "<cmd>Trouble diagnostics toggle<cr>", { desc = "Trouble diagnostics" })
map("n", "<leader>xu", "<cmd>UndotreeToggle<cr>", { desc = "Undo tree" })
-- NOTE: <leader>xg owned by advanced.lua (Neogen keys= + lazy-load).

-- ============================================================================
-- TODO COMMENTS — factory-driven (v2.3.13)
-- ============================================================================

local todo_call = lazy("todo-comments", "[todo-comments]")
map("n", "]t", todo_call(function(tc) tc.jump_next() end), { desc = "Next todo" })
map("n", "[t", todo_call(function(tc) tc.jump_prev() end), { desc = "Previous todo" })

-- ============================================================================
-- OIL  (convenience alias — <leader>eo owned by hud.lua oil.nvim keys=)
-- ============================================================================

map("n", "-", "<cmd>Oil<cr>", { desc = "Open parent dir" })

-- ============================================================================
-- NOICE
-- ============================================================================

map("n", "<leader>un", "<cmd>Noice dismiss<cr>", { desc = "Dismiss notifications" })
map("n", "<leader>uN", "<cmd>Noice history<cr>", { desc = "Notification history" })

-- ============================================================================
-- FOCUS
-- ============================================================================

map("n", "<leader>uF", function() require("core.focus").toggle() end, { desc = "Deep focus mode" })
