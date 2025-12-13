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
map("n", "<leader>bd", "<cmd>bd<cr>", { silent = true, desc = "Delete buffer" })
map("n", "<leader>bo", "<cmd>%bd|e#|bd#<cr>", { silent = true, desc = "Delete other buffers" })

-- File operations
map("n", "<leader>w", "<cmd>w<cr>", { silent = true, desc = "Save" })
map("n", "<leader>q", "<cmd>q<cr>", { silent = true, desc = "Quit" })
map("n", "<leader>Q", "<cmd>qa<cr>", { silent = true, desc = "Quit all" })

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
map("n", "[d", vim.diagnostic.goto_prev, { desc = "Previous diagnostic" })
map("n", "]d", vim.diagnostic.goto_next, { desc = "Next diagnostic" })
map("n", "<leader>e", vim.diagnostic.open_float, { desc = "Show diagnostic" })
map("n", "<leader>cl", vim.diagnostic.setloclist, { desc = "Diagnostic loclist" })

-- Telescope (Find group)
map("n", "<leader>ff", "<cmd>Telescope find_files<cr>", { desc = "Find files" })
map("n", "<leader>fg", "<cmd>Telescope live_grep<cr>", { desc = "Live grep" })
map("n", "<leader>fb", "<cmd>Telescope buffers<cr>", { desc = "Find buffers" })
map("n", "<leader>fh", "<cmd>Telescope help_tags<cr>", { desc = "Help tags" })
map("n", "<leader>fr", "<cmd>Telescope oldfiles<cr>", { desc = "Recent files" })
map("n", "<leader>fw", "<cmd>Telescope grep_string<cr>", { desc = "Find word" })
map("n", "<leader>fc", "<cmd>Telescope commands<cr>", { desc = "Commands" })
map("n", "<leader>fk", "<cmd>Telescope keymaps<cr>", { desc = "Keymaps" })
map("n", "<leader>fd", "<cmd>Telescope diagnostics<cr>", { desc = "Diagnostics" })

-- Quick access
map("n", "<C-p>", "<cmd>Telescope find_files<cr>", opts)
map("n", "<C-f>", "<cmd>Telescope live_grep<cr>", opts)

-- File explorer
map("n", "<leader>e", "<cmd>NvimTreeToggle<cr>", { desc = "Explorer" })
map("n", "<leader>o", "<cmd>NvimTreeFindFile<cr>", { desc = "Find in explorer" })

-- Terminal
map("t", "<Esc>", [[<C-\><C-n>]], opts)
map("t", "jk", [[<C-\><C-n>]], opts)
map("n", "<C-\\>", "<cmd>ToggleTerm<cr>", { desc = "Terminal" })
map("t", "<C-\\>", [[<C-\><C-n><cmd>ToggleTerm<cr>]], opts)
map("n", "<leader>tf", "<cmd>ToggleTerm direction=float<cr>", { desc = "Float terminal" })
map("n", "<leader>th", "<cmd>ToggleTerm direction=horizontal<cr>", { desc = "Horizontal terminal" })

-- Git (conditional)
vim.api.nvim_create_autocmd("User", {
  pattern = "LazyLoad",
  callback = function(event)
    if event.data == "toggleterm.nvim" then
      map("n", "<leader>gg", function()
        local term = require("toggleterm.terminal").Terminal
        local lazygit = term:new({ cmd = "lazygit", direction = "float", hidden = true })
        lazygit:toggle()
      end, { desc = "Lazygit" })
    end
  end,
})

-- Code execution (Run group)
map("n", "<F5>", function() require("utils.runner").run_file() end, { desc = "Run file" })
map("n", "<leader>rr", function() require("utils.runner").run_file() end, { desc = "Run file" })
map("n", "<leader>rb", function() require("utils.runner").build_project() end, { desc = "Build project" })

-- Split management
map("n", "<leader>sv", "<cmd>vsplit<cr>", { desc = "Vertical split" })
map("n", "<leader>sh", "<cmd>split<cr>", { desc = "Horizontal split" })
map("n", "<leader>sx", "<cmd>close<cr>", { desc = "Close split" })
map("n", "<leader>se", "<C-w>=", { desc = "Equal splits" })

-- Theme toggle
map("n", "<leader>tt", function()
  vim.o.background = vim.o.background == "dark" and "light" or "dark"
  require("core.theme").apply_highlights()
end, { desc = "Toggle theme" })

-- Session management (conditional)
vim.api.nvim_create_autocmd("User", {
  pattern = "LazyLoad",
  callback = function(event)
    if event.data == "persistence.nvim" then
      map("n", "<leader>qs", function() require("persistence").load() end, { desc = "Restore session" })
      map("n", "<leader>ql", function() require("persistence").load({ last = true }) end, { desc = "Last session" })
      map("n", "<leader>qd", function() require("persistence").stop() end, { desc = "Don't save session" })
    end
  end,
})

-- DAP keymaps (conditional) - FIXED: Removed duplicate F5 binding
vim.api.nvim_create_autocmd("User", {
  pattern = "LazyLoad",
  callback = function(event)
    if event.data == "nvim-dap" then
      local dap = require("dap")
      -- Note: F5 is used for run_file above, DAP uses F5 only when in debug context
      map("n", "<leader>dc", dap.continue, { desc = "Debug: Continue" })
      map("n", "<F10>", dap.step_over, { desc = "Debug: Step Over" })
      map("n", "<F11>", dap.step_into, { desc = "Debug: Step Into" })
      map("n", "<F12>", dap.step_out, { desc = "Debug: Step Out" })
      map("n", "<leader>db", dap.toggle_breakpoint, { desc = "Toggle breakpoint" })
      map("n", "<leader>dB", function()
        dap.set_breakpoint(vim.fn.input('Breakpoint condition: '))
      end, { desc = "Conditional breakpoint" })
      map("n", "<leader>dr", dap.repl.open, { desc = "Open REPL" })
      map("n", "<leader>dl", dap.run_last, { desc = "Run last" })
      map("n", "<leader>dt", function() require("dapui").toggle() end, { desc = "Toggle UI" })
      map("n", "<leader>dx", dap.terminate, { desc = "Terminate" })
    end
  end,
})

-- Testing keymaps (conditional)
vim.api.nvim_create_autocmd("User", {
  pattern = "LazyLoad",
  callback = function(event)
    if event.data == "neotest" then
      local neotest = require("neotest")
      map("n", "<leader>tn", neotest.run.run, { desc = "Run nearest test" })
      map("n", "<leader>tf", function() neotest.run.run(vim.fn.expand("%")) end, { desc = "Run file tests" })
      map("n", "<leader>ta", function() neotest.run.run({ suite = true }) end, { desc = "Run all tests" })
      map("n", "<leader>ts", neotest.summary.toggle, { desc = "Toggle summary" })
      map("n", "<leader>to", function() neotest.output.open({ enter = true }) end, { desc = "Show output" })
      map("n", "<leader>tO", neotest.output_panel.toggle, { desc = "Toggle output panel" })
      map("n", "<leader>tS", neotest.run.stop, { desc = "Stop test" })
      map("n", "[t", neotest.jump.prev, { desc = "Previous test" })
      map("n", "]t", neotest.jump.next, { desc = "Next test" })
    end
  end,
})

-- REMOVED: Leap keymaps (now in plugins/editor.lua config)
