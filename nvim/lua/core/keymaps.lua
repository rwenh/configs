-- ~/.config/nvim/lua/core/keymaps.lua

local map = vim.keymap.set
local opts = { silent = true }

-- Better escape
map("i", "jk", "<Esc>", opts)
map("i", "kj", "<Esc>", opts)

-- Window navigation
map("n", "<C-h>", "<C-w>h", opts)
map("n", "<C-j>", "<C-w>j", opts)
map("n", "<C-k>", "<C-w>k", opts)
map("n", "<C-l>", "<C-w>l", opts)

-- Resize windows
map("n", "<C-Up>", "<cmd>resize +2<cr>", opts)
map("n", "<C-Down>", "<cmd>resize -2<cr>", opts)
map("n", "<C-Left>", "<cmd>vertical resize -2<cr>", opts)
map("n", "<C-Right>", "<cmd>vertical resize +2<cr>", opts)

-- Buffer navigation
map("n", "<S-h>", "<cmd>bprev<cr>", opts)
map("n", "<S-l>", "<cmd>bnext<cr>", opts)
map("n", "<leader>bd", "<cmd>bd<cr>", opts)

-- File operations
map("n", "<leader>w", "<cmd>w<cr>", opts)
map("n", "<leader>q", "<cmd>q<cr>", opts)

-- Better indenting
map("v", "<", "<gv", opts)
map("v", ">", ">gv", opts)

-- Move lines
map("n", "<A-j>", "<cmd>m .+1<cr>==", opts)
map("n", "<A-k>", "<cmd>m .-2<cr>==", opts)
map("v", "<A-j>", ":m '>+1<cr>gv=gv", opts)
map("v", "<A-k>", ":m '<-2<cr>gv=gv", opts)

-- Better paste
map("v", "p", '"_dP', opts)

-- Center movements
map("n", "<C-d>", "<C-d>zz", opts)
map("n", "<C-u>", "<C-u>zz", opts)
map("n", "n", "nzzzv", opts)
map("n", "N", "Nzzzv", opts)

-- Clear search
map("n", "<Esc>", "<cmd>noh<cr>", opts)

-- Diagnostics
map("n", "[d", vim.diagnostic.goto_prev, opts)
map("n", "]d", vim.diagnostic.goto_next, opts)
map("n", "<leader>e", vim.diagnostic.open_float, opts)

-- Telescope
map("n", "<leader>ff", "<cmd>Telescope find_files<cr>", opts)
map("n", "<leader>fg", "<cmd>Telescope live_grep<cr>", opts)
map("n", "<leader>fb", "<cmd>Telescope buffers<cr>", opts)
map("n", "<leader>fh", "<cmd>Telescope help_tags<cr>", opts)
map("n", "<leader>fr", "<cmd>Telescope oldfiles<cr>", opts)
map("n", "<leader>fw", "<cmd>Telescope grep_string<cr>", opts)

-- Quick access
map("n", "<C-p>", "<cmd>Telescope find_files<cr>", opts)
map("n", "<C-f>", "<cmd>Telescope live_grep<cr>", opts)

-- File explorer
map("n", "<leader>e", "<cmd>NvimTreeToggle<cr>", opts)
map("n", "<leader>o", "<cmd>NvimTreeFindFile<cr>", opts)

-- Terminal
map("t", "<Esc>", [[<C-\><C-n>]], opts)
map("t", "jk", [[<C-\><C-n>]], opts)
map("n", "<C-\\>", "<cmd>ToggleTerm<cr>", opts)
map("t", "<C-\\>", [[<C-\><C-n><cmd>ToggleTerm<cr>]], opts)

-- Git
map("n", "<leader>gg", function()
  local term = require("toggleterm.terminal").Terminal
  local lazygit = term:new({ cmd = "lazygit", direction = "float", hidden = true })
  lazygit:toggle()
end, opts)

-- Code execution
map("n", "<F5>", function() require("utils.runner").run_file() end, opts)
map("n", "<leader>rr", function() require("utils.runner").run_file() end, opts)

-- Split management
map("n", "<leader>sv", "<cmd>vsplit<cr>", opts)
map("n", "<leader>sh", "<cmd>split<cr>", opts)
map("n", "<leader>sx", "<cmd>close<cr>", opts)

-- Theme toggle
map("n", "<leader>tt", function()
  vim.o.background = vim.o.background == "dark" and "light" or "dark"
  require("core.theme").apply_highlights()
end, opts)

-- Leap mappings (replaces deprecated add_default_mappings)
local leap_ok, leap = pcall(require, "leap")
if leap_ok then
  map({'n', 'x', 'o'}, 's', '<Plug>(leap-forward)', opts)
  map({'n', 'x', 'o'}, 'S', '<Plug>(leap-backward)', opts)
  map({'n', 'x', 'o'}, 'gs', '<Plug>(leap-from-window)', opts)
end

-- LSP (set in lsp.lua on_attach)
-- DAP (set in dap.lua)
-- Testing (set in testing.lua)