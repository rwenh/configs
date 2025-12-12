-- ~/.config/nvim/lua/core/keymaps.lua
-- All keybindings

local helpers = require("utils.helpers")

local function map(mode, lhs, rhs, opts)
  opts = vim.tbl_extend("force", { silent = true, noremap = true }, opts or {})
  vim.keymap.set(mode, lhs, rhs, opts)
end

-- Clear space in normal mode
map("n", "<Space>", "<Nop>")

-- Insert mode escapes
map("i", "jk", "<Esc>", { desc = "Exit insert mode" })
map("i", "kj", "<Esc>", { desc = "Exit insert mode" })
map("t", "<Esc>", [[<C-\><C-n>]], { desc = "Exit terminal mode" })
map("t", "jk", [[<C-\><C-n>]], { desc = "Exit terminal mode" })

-- Window navigation
map("n", "<C-h>", "<C-w>h", { desc = "Left window" })
map("n", "<C-j>", "<C-w>j", { desc = "Lower window" })
map("n", "<C-k>", "<C-w>k", { desc = "Upper window" })
map("n", "<C-l>", "<C-w>l", { desc = "Right window" })

-- Window resize
map("n", "<C-Up>", "<cmd>resize +2<cr>", { desc = "Increase height" })
map("n", "<C-Down>", "<cmd>resize -2<cr>", { desc = "Decrease height" })
map("n", "<C-Left>", "<cmd>vertical resize -2<cr>", { desc = "Decrease width" })
map("n", "<C-Right>", "<cmd>vertical resize +2<cr>", { desc = "Increase width" })

-- Buffer navigation
map("n", "<S-h>", "<cmd>bprevious<cr>", { desc = "Previous buffer" })
map("n", "<S-l>", "<cmd>bnext<cr>", { desc = "Next buffer" })
map("n", "[b", "<cmd>bprevious<cr>", { desc = "Previous buffer" })
map("n", "]b", "<cmd>bnext<cr>", { desc = "Next buffer" })

-- File operations
map("n", "<leader>w", "<cmd>write<cr>", { desc = "Save file" })
map("n", "<leader>W", "<cmd>wall<cr>", { desc = "Save all files" })
map("n", "<leader>q", "<cmd>quit<cr>", { desc = "Quit" })
map("n", "<leader>Q", "<cmd>qall!<cr>", { desc = "Force quit all" })
map("n", "<leader>bd", helpers.smart_buf_delete, { desc = "Delete buffer" })
map("n", "<leader>bD", "<cmd>%bd|e#|bd#<cr>", { desc = "Delete other buffers" })

-- Splits
map("n", "<leader>sv", "<cmd>vsplit<cr>", { desc = "Split vertically" })
map("n", "<leader>sh", "<cmd>split<cr>", { desc = "Split horizontally" })
map("n", "<leader>se", "<C-w>=", { desc = "Make splits equal" })
map("n", "<leader>sx", "<cmd>close<cr>", { desc = "Close current split" })
map("n", "<leader>so", "<cmd>only<cr>", { desc = "Close other splits" })

-- Search
map("n", "<leader>nh", "<cmd>nohlsearch<cr>", { desc = "Clear search highlights" })
map("n", "n", "nzzzv", { desc = "Next search result" })
map("n", "N", "Nzzzv", { desc = "Previous search result" })
map("n", "*", "*zzzv", { desc = "Search word forward" })
map("n", "#", "#zzzv", { desc = "Search word backward" })

-- Replace
map("n", "<leader>rw", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]], { desc = "Replace word" })
map("v", "<leader>rw", [["hy:%s/\V<C-r>h/<C-r>h/gc<left><left><left>]], { desc = "Replace selection" })

-- Visual mode
map("v", "<", "<gv", { desc = "Indent left" })
map("v", ">", ">gv", { desc = "Indent right" })
map("v", "p", '"_dP', { desc = "Paste without yanking" })
map("x", "<leader>p", [["_dP]], { desc = "Paste without yanking" })

-- Move lines
map("n", "<A-j>", "<cmd>move .+1<cr>==", { desc = "Move line down" })
map("n", "<A-k>", "<cmd>move .-2<cr>==", { desc = "Move line up" })
map("i", "<A-j>", "<Esc><cmd>move .+1<cr>==gi", { desc = "Move line down" })
map("i", "<A-k>", "<Esc><cmd>move .-2<cr>==gi", { desc = "Move line up" })
map("v", "<A-j>", ":move '>+1<cr>gv=gv", { desc = "Move selection down" })
map("v", "<A-k>", ":move '<-2<cr>gv=gv", { desc = "Move selection up" })

-- Clipboard
map({ "n", "v" }, "<leader>y", [["+y]], { desc = "Copy to system clipboard" })
map("n", "<leader>Y", [["+Y]], { desc = "Copy line to clipboard" })
map({ "n", "v" }, "<leader>d", [["_d]], { desc = "Delete without yanking" })

-- Center movements
map("n", "<C-d>", "<C-d>zz", { desc = "Half page down and center" })
map("n", "<C-u>", "<C-u>zz", { desc = "Half page up and center" })
map("n", "G", "Gzz", { desc = "Go to end and center" })
map("n", "gg", "ggzz", { desc = "Go to start and center" })

-- Diagnostics
map("n", "[d", vim.diagnostic.goto_prev, { desc = "Previous diagnostic" })
map("n", "]d", vim.diagnostic.goto_next, { desc = "Next diagnostic" })
map("n", "[e", function() vim.diagnostic.goto_prev({ severity = vim.diagnostic.severity.ERROR }) end, { desc = "Previous error" })
map("n", "]e", function() vim.diagnostic.goto_next({ severity = vim.diagnostic.severity.ERROR }) end, { desc = "Next error" })
map("n", "<leader>e", vim.diagnostic.open_float, { desc = "Show diagnostics" })
map("n", "<leader>xl", vim.diagnostic.setloclist, { desc = "Diagnostic loclist" })

-- Telescope
map("n", "<leader>ff", "<cmd>Telescope find_files<cr>", { desc = "Find files" })
map("n", "<leader>fg", "<cmd>Telescope live_grep<cr>", { desc = "Live grep" })
map("n", "<leader>fb", "<cmd>Telescope buffers<cr>", { desc = "Find buffers" })
map("n", "<leader>fh", "<cmd>Telescope help_tags<cr>", { desc = "Help tags" })
map("n", "<C-p>", "<cmd>Telescope find_files<cr>", { desc = "Find files" })
map("n", "<C-f>", "<cmd>Telescope live_grep<cr>", { desc = "Live grep" })

-- File explorer
map("n", "<leader>e", "<cmd>NvimTreeToggle<cr>", { desc = "Toggle file explorer" })
map("n", "<leader>E", "<cmd>NvimTreeFindFile<cr>", { desc = "Find current file" })

-- Terminal
map("n", "<C-\\>", "<cmd>ToggleTerm<cr>", { desc = "Toggle terminal" })
map("n", "<leader>tf", "<cmd>ToggleTerm direction=float<cr>", { desc = "Float terminal" })
map("n", "<leader>th", "<cmd>ToggleTerm direction=horizontal<cr>", { desc = "Horizontal terminal" })

-- Git
map("n", "<leader>gs", "<cmd>Git<cr>", { desc = "Git status" })
map("n", "<leader>gc", "<cmd>Git commit<cr>", { desc = "Git commit" })
map("n", "<leader>gp", "<cmd>Git push<cr>", { desc = "Git push" })

-- Lazygit
map("n", "<leader>gg", function()
  local Terminal = require("toggleterm.terminal").Terminal
  local lazygit = Terminal:new({ cmd = "lazygit", direction = "float", hidden = true })
  lazygit:toggle()
end, { desc = "Lazygit" })

-- Code execution
map("n", "<F5>", function() require("utils.runner").run_file() end, { desc = "Run current file" })
map("n", "<leader>rr", function() require("utils.runner").run_file() end, { desc = "Run current file" })
map("n", "<leader>rb", function() require("utils.runner").build_project() end, { desc = "Build project" })

-- Project root
map("n", "<leader>cd", function()
  local root = helpers.find_project_root()
  vim.cmd("cd " .. root)
  vim.notify("Changed to: " .. root)
end, { desc = "Change to project root" })

-- Theme toggles
map("n", "<leader>tt", function()
  local theme = require("core.theme")
  local current_bg = vim.o.background
  local new_bg = current_bg == "dark" and "light" or "dark"
  vim.o.background = new_bg
  pcall(vim.cmd.colorscheme, "solarized")
  vim.schedule(function()
    theme.apply_highlights()
  end)
  vim.notify("Switched to Solarized " .. new_bg, vim.log.levels.INFO)
  theme.auto_switching_enabled = false
end, { desc = "Toggle theme" })

map("n", "<leader>td", function()
  vim.o.background = "dark"
  pcall(vim.cmd.colorscheme, "solarized")
  require("core.theme").apply_highlights()
end, { desc = "Solarized dark" })

map("n", "<leader>tl", function()
  vim.o.background = "light"
  pcall(vim.cmd.colorscheme, "solarized")
  require("core.theme").apply_highlights()
end, { desc = "Solarized light" })

-- Misc
map("n", "<leader>cc", function()
  vim.cmd("edit " .. vim.fn.stdpath("config") .. "/init.lua")
end, { desc = "Edit config" })

map("n", "<leader>rn", function()
  vim.opt.relativenumber = not vim.opt.relativenumber:get()
end, { desc = "Toggle relative numbers" })
