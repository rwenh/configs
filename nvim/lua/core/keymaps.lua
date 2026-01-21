-- lua/core/keymaps.lua - Keybindings

local map = vim.keymap.set
local opts = { noremap = true, silent = true }

-- Quick escape from insert mode
map("i", "jk", "<Esc>", opts)
map("i", "kj", "<Esc>", opts)

-- Window management
map("n", "<leader>q", "<cmd>q<cr>", { desc = "Quit" })
map("n", "<leader>w", "<cmd>w<cr>", { desc = "Save" })
map("n", "<leader>wq", "<cmd>wq<cr>", { desc = "Save & Quit" })

-- Window splitting
map("n", "<leader>vsw", "<cmd>vsplit<cr>", { desc = "Vertical split" })
map("n", "<leader>hsw", "<cmd>split<cr>", { desc = "Horizontal split" })

-- Window navigation
map("n", "<C-h>", "<C-w>h", opts)
map("n", "<C-j>", "<C-w>j", opts)
map("n", "<C-k>", "<C-w>k", opts)
map("n", "<C-l>", "<C-w>l", opts)

-- Window resizing
map("n", "<C-Up>", "<cmd>resize +2<cr>", opts)
map("n", "<C-Down>", "<cmd>resize -2<cr>", opts)
map("n", "<C-Left>", "<cmd>vertical resize -2<cr>", opts)
map("n", "<C-Right>", "<cmd>vertical resize +2<cr>", opts)

-- Buffer navigation
map("n", "]b", "<cmd>bnext<cr>", { desc = "Next buffer" })
map("n", "[b", "<cmd>bprev<cr>", { desc = "Prev buffer" })
map("n", "<leader>bd", "<cmd>bdelete<cr>", { desc = "Delete buffer" })
map("n", "<leader>bo", "<cmd>BufOnly<cr>", { desc = "Delete other buffers" })

-- Better indenting
map("v", "<", "<gv", opts)
map("v", ">", ">gv", opts)

-- Move lines
map("n", "<A-j>", "<cmd>move .+1<cr>==", opts)
map("n", "<A-k>", "<cmd>move .-2<cr>==", opts)
map("v", "<A-j>", ":move '>+1<cr>gv=gv", opts)
map("v", "<A-k>", ":move '<-2<cr>gv=gv", opts)

-- Search
map("n", "<C-s>", "<cmd>Telescope live_grep<cr>", { desc = "Live grep" })
map("n", "<leader>ff", "<cmd>Telescope find_files<cr>", { desc = "Find files" })
map("n", "<leader>fg", "<cmd>Telescope git_files<cr>", { desc = "Git files" })
map("n", "<leader>fb", "<cmd>Telescope buffers<cr>", { desc = "Buffers" })
map("n", "<leader>fh", "<cmd>Telescope help_tags<cr>", { desc = "Help" })
map("n", "<leader>fm", "<cmd>Telescope marks<cr>", { desc = "Marks" })
map("n", "<leader>fc", "<cmd>Telescope commands<cr>", { desc = "Commands" })
map("n", "<leader>fk", "<cmd>Telescope keymaps<cr>", { desc = "Keymaps" })
map("n", "<leader>fr", "<cmd>TelescopeResume<cr>", { desc = "Resume" })

-- File explorer
map("n", "<leader>e", "<cmd>NvimTreeToggle<cr>", { desc = "File explorer" })
map("n", "<leader>E", "<cmd>NvimTreeFindFile<cr>", { desc = "Find file" })

-- Code
map("n", "gd", "<cmd>lua vim.lsp.buf.definition()<cr>", { desc = "Go to definition" })
map("n", "gD", "<cmd>lua vim.lsp.buf.declaration()<cr>", { desc = "Go to declaration" })
map("n", "gi", "<cmd>lua vim.lsp.buf.implementation()<cr>", { desc = "Go to implementation" })
map("n", "gr", "<cmd>lua vim.lsp.buf.references()<cr>", { desc = "References" })
map("n", "K", "<cmd>lua vim.lsp.buf.hover()<cr>", { desc = "Hover" })
map("n", "<leader>rn", "<cmd>lua vim.lsp.buf.rename()<cr>", { desc = "Rename" })
map("n", "<leader>ca", "<cmd>lua vim.lsp.buf.code_action()<cr>", { desc = "Code action" })
map("v", "<leader>ca", "<cmd>lua vim.lsp.buf.code_action()<cr>", { desc = "Code action" })
map("n", "<leader>f", "<cmd>lua vim.lsp.buf.format()<cr>", { desc = "Format" })

-- Diagnostics
map("n", "]d", "<cmd>lua vim.diagnostic.goto_next()<cr>", { desc = "Next diagnostic" })
map("n", "[d", "<cmd>lua vim.diagnostic.goto_prev()<cr>", { desc = "Prev diagnostic" })
map("n", "<leader>dd", "<cmd>lua vim.diagnostic.open_float()<cr>", { desc = "Diagnostic" })
map("n", "<leader>dq", "<cmd>lua vim.diagnostic.setloclist()<cr>", { desc = "Diagnostics" })

-- Git
map("n", "<leader>gg", "<cmd>LazyGit<cr>", { desc = "LazyGit" })
map("n", "<leader>gb", "<cmd>Telescope git_branches<cr>", { desc = "Branches" })
map("n", "<leader>gc", "<cmd>Telescope git_commits<cr>", { desc = "Commits" })
map("n", "<leader>gs", "<cmd>Telescope git_status<cr>", { desc = "Status" })
map("n", "]h", "<cmd>Gitsigns next_hunk<cr>", { desc = "Next hunk" })
map("n", "[h", "<cmd>Gitsigns prev_hunk<cr>", { desc = "Prev hunk" })
map("n", "<leader>gp", "<cmd>Gitsigns preview_hunk<cr>", { desc = "Preview hunk" })
map("n", "<leader>gr", "<cmd>Gitsigns reset_hunk<cr>", { desc = "Reset hunk" })

-- Debug
map("n", "<leader>db", "<cmd>lua require('dap').toggle_breakpoint()<cr>", { desc = "Breakpoint" })
map("n", "<leader>dc", "<cmd>lua require('dap').continue()<cr>", { desc = "Continue" })
map("n", "<leader>di", "<cmd>lua require('dap').step_into()<cr>", { desc = "Step into" })
map("n", "<leader>do", "<cmd>lua require('dap').step_over()<cr>", { desc = "Step over" })
map("n", "<leader>dO", "<cmd>lua require('dap').step_out()<cr>", { desc = "Step out" })
map("n", "<leader>dr", "<cmd>lua require('dap').repl.toggle()<cr>", { desc = "REPL" })
map("n", "<leader>dl", "<cmd>lua require('dap').run_last()<cr>", { desc = "Run last" })
map("n", "<leader>dt", "<cmd>lua require('dapui').toggle()<cr>", { desc = "Toggle UI" })

-- Terminal
map("n", "<leader>tt", "<cmd>terminal<cr>", { desc = "Terminal" })
map("t", "<Esc>", "<C-\\><C-n>", opts)

-- Theme
map("n", "<leader>ut", "<cmd>lua require('core.theme').toggle()<cr>", { desc = "Toggle theme" })

-- Misc
map("n", "<leader>*", "<cmd>keepjumps normal! *<cr>", { desc = "Search word" })
map("n", "<leader>h", "<cmd>nohlsearch<cr>", { desc = "No highlight" })
map("n", "<leader>cd", "<cmd>cd %:p:h<cr>", { desc = "Change dir" })
map("n", "<leader>x", "<cmd>!chmod +x %<cr>", { desc = "Make executable" })
