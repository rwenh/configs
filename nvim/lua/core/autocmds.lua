-- ~/.config/nvim/lua/core/autocmds.lua
-- Autocommands

local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup

-- Highlight on yank
autocmd("TextYankPost", {
  group = augroup("HighlightYank", { clear = true }),
  callback = function()
    vim.highlight.on_yank({ timeout = 200 })
  end,
})

-- Restore cursor position
autocmd("BufReadPost", {
  group = augroup("RestoreCursor", { clear = true }),
  callback = function(args)
    local mark = vim.api.nvim_buf_get_mark(args.buf, '"')
    local lcount = vim.api.nvim_buf_line_count(args.buf)
    if mark[1] > 0 and mark[1] <= lcount then
      pcall(vim.api.nvim_win_set_cursor, 0, mark)
    end
  end,
})

-- Resize windows on vim resize
autocmd("VimResized", {
  group = augroup("ResizeWindows", { clear = true }),
  callback = function()
    local current_tab = vim.fn.tabpagenr()
    vim.cmd("tabdo wincmd =")
    vim.cmd("tabnext " .. current_tab)
  end,
})

-- Close with q
autocmd("FileType", {
  group = augroup("CloseWithQ", { clear = true }),
  pattern = { "help", "man", "qf", "lspinfo", "checkhealth", "notify" },
  callback = function(event)
    vim.bo[event.buf].buflisted = false
    vim.keymap.set("n", "q", "<cmd>close<cr>", {
      buffer = event.buf,
      silent = true,
      desc = "Close window"
    })
  end,
})

-- Trim whitespace
autocmd("BufWritePre", {
  group = augroup("TrimWhitespace", { clear = true }),
  callback = function()
    local exclude_ft = { "markdown", "diff", "gitcommit" }
    if vim.tbl_contains(exclude_ft, vim.bo.filetype) then return end
    
    local save_cursor = vim.fn.getpos(".")
    pcall(function() vim.cmd([[%s/\s\+$//e]]) end)
    vim.fn.setpos(".", save_cursor)
  end,
})

-- Language-specific settings
autocmd("FileType", {
  group = augroup("LanguageSettings", { clear = true }),
  pattern = { "html", "css", "javascript", "typescript", "json", "yaml" },
  callback = function()
    vim.opt_local.shiftwidth = 2
    vim.opt_local.tabstop = 2
    vim.opt_local.softtabstop = 2
  end,
})

autocmd("FileType", {
  group = augroup("PythonSettings", { clear = true }),
  pattern = "python",
  callback = function()
    vim.opt_local.colorcolumn = "88,120"
  end,
})

autocmd("FileType", {
  group = augroup("FortranSettings", { clear = true }),
  pattern = "fortran",
  callback = function()
    vim.opt_local.colorcolumn = "72,132"
  end,
})

autocmd("FileType", {
  group = augroup("MarkdownSettings", { clear = true }),
  pattern = "markdown",
  callback = function()
    vim.opt_local.wrap = true
    vim.opt_local.spell = true
    vim.opt_local.linebreak = true
  end,
})

-- Large file handling
autocmd("BufReadPre", {
  group = augroup("LargeFile", { clear = true }),
  callback = function(args)
    local ok, stats = pcall(vim.uv.fs_stat, vim.api.nvim_buf_get_name(args.buf))
    if ok and stats and stats.size > 1024 * 1024 then
      vim.b[args.buf].large_file = true
      vim.opt_local.foldmethod = "manual"
      vim.opt_local.undolevels = -1
    end
  end,
})

-- Terminal settings
autocmd("TermOpen", {
  group = augroup("TerminalSettings", { clear = true }),
  callback = function()
    vim.opt_local.number = false
    vim.opt_local.relativenumber = false
    vim.cmd("startinsert")
  end,
})

-- Auto reload files
autocmd({ "FocusGained", "BufEnter", "CursorHold" }, {
  group = augroup("AutoReload", { clear = true }),
  callback = function()
    if vim.fn.mode() ~= 'c' then
      vim.cmd("checktime")
    end
  end,
})

-- Prevent lazyredraw from being enabled (critical for noice.nvim)
autocmd("OptionSet", {
  pattern = "lazyredraw",
  callback = function()
    if vim.opt.lazyredraw:get() then
      vim.schedule(function()
        vim.opt.lazyredraw = false
      end)
    end
  end,
})
