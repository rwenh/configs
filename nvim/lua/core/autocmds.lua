-- lua/core/autocmds.lua - Autocommands (autoformat removed - conform.nvim handles it)

local au = vim.api.nvim_create_autocmd
local ag = vim.api.nvim_create_augroup

-- Highlight on yank
au("TextYankPost", {
  group    = ag("HighlightYank", { clear = true }),
  callback = function()
    vim.highlight.on_yank({ timeout = 200 })
  end,
})

-- Restore cursor position
au("BufReadPost", {
  group    = ag("RestoreCursor", { clear = true }),
  callback = function(e)
    local mark   = vim.api.nvim_buf_get_mark(e.buf, '"')
    local lcount = vim.api.nvim_buf_line_count(e.buf)
    if mark[1] > 0 and mark[1] <= lcount then
      pcall(vim.api.nvim_win_set_cursor, 0, mark)
    end
  end,
})

-- Resize splits on window resize
au("VimResized", {
  group    = ag("ResizeSplits", { clear = true }),
  callback = function()
    local tab = vim.fn.tabpagenr()
    vim.cmd("tabdo wincmd =")
    vim.cmd("tabnext " .. tab)
  end,
})

-- Close certain windows with q
au("FileType", {
  group   = ag("CloseWithQ", { clear = true }),
  pattern = { "help", "man", "qf", "lspinfo", "checkhealth", "notify", "startuptime", "query" },
  callback = function(e)
    vim.bo[e.buf].buflisted = false
    vim.keymap.set("n", "q", "<cmd>close<cr>", { buffer = e.buf, silent = true })
  end,
})

-- Remove trailing whitespace on save
au("BufWritePre", {
  group    = ag("TrimWhitespace", { clear = true }),
  callback = function()
    if vim.tbl_contains({ "markdown", "diff" }, vim.bo.filetype) then return end
    local view = vim.fn.winsaveview()
    pcall(function() vim.cmd([[%s/\s\+$//e]]) end)
    vim.fn.winrestview(view)
  end,
})

-- Check for external file changes
au({ "FocusGained", "TermClose", "TermLeave" }, {
  group    = ag("Checktime", { clear = true }),
  callback = function()
    if vim.o.buftype ~= "nofile" then vim.cmd("checktime") end
  end,
})

-- Language-specific indent settings
au("FileType", {
  group   = ag("WebDev", { clear = true }),
  pattern = { "html", "css", "javascript", "typescript", "json", "yaml" },
  callback = function()
    vim.opt_local.shiftwidth = 2
    vim.opt_local.tabstop    = 2
  end,
})

au("FileType", {
  group   = ag("GoDev", { clear = true }),
  pattern = { "go" },
  callback = function()
    vim.opt_local.shiftwidth = 4
    vim.opt_local.tabstop    = 4
    vim.opt_local.expandtab  = false
  end,
})

au("FileType", {
  group   = ag("Markdown", { clear = true }),
  pattern = "markdown",
  callback = function()
    vim.opt_local.wrap  = true
    vim.opt_local.spell = true
  end,
})

-- Terminal: clean UI
au("TermOpen", {
  group    = ag("Terminal", { clear = true }),
  callback = function()
    vim.opt_local.number         = false
    vim.opt_local.relativenumber = false
    vim.opt_local.signcolumn     = "no"
  end,
})

-- Auto cd to project root (opt-in via vim.g.auto_cd_root = true)
au("BufEnter", {
  group    = ag("AutoCdRoot", { clear = true }),
  callback = function()
    if vim.g.auto_cd_root then
      local root = require("core.util.path").find_root()
      if root then vim.cmd.cd(root) end
    end
  end,
})

-- COBOL filetype detection
au({ "BufRead", "BufNewFile" }, {
  group   = ag("CobolFileType", { clear = true }),
  pattern = { "*.cob", "*.cbl", "*.cpy", "*.CBL", "*.COB" },
  callback = function() vim.bo.filetype = "cobol" end,
})

-- VHDL filetype detection
au({ "BufRead", "BufNewFile" }, {
  group   = ag("VhdlFileType", { clear = true }),
  pattern = { "*.vhd", "*.vhdl", "*.vho" },
  callback = function() vim.bo.filetype = "vhdl" end,
})

-- Large file optimisation (>500KB: disable slow features)
au("BufReadPre", {
  group    = ag("LargeFile", { clear = true }),
  callback = function(e)
    local ok, stat = pcall(vim.uv.fs_stat, vim.api.nvim_buf_get_name(e.buf))
    if ok and stat and stat.size > 500 * 1024 then
      vim.b[e.buf].large_file = true
      vim.opt_local.foldmethod  = "manual"
      vim.opt_local.spell       = false
      vim.opt_local.cursorline  = false
      vim.opt_local.swapfile    = false
      vim.notify("Large file — some features disabled", vim.log.levels.WARN)
    end
  end,
})
