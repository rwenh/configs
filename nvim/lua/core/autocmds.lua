-- ~/.config/nvim/lua/core/autocmds.lua

local au = vim.api.nvim_create_autocmd
local ag = vim.api.nvim_create_augroup

-- Highlight on yank
au("TextYankPost", {
  group = ag("YankHighlight", { clear = true }),
  callback = function()
    vim.highlight.on_yank({ timeout = 150 })
  end,
})

-- Restore cursor position
au("BufReadPost", {
  group = ag("RestoreCursor", { clear = true }),
  callback = function(args)
    local mark = vim.api.nvim_buf_get_mark(args.buf, '"')
    local lcount = vim.api.nvim_buf_line_count(args.buf)
    if mark[1] > 0 and mark[1] <= lcount then
      pcall(vim.api.nvim_win_set_cursor, 0, mark)
    end
  end,
})

-- Resize windows on vim resize
au("VimResized", {
  group = ag("ResizeWindows", { clear = true }),
  callback = function()
    vim.cmd("tabdo wincmd =")
  end,
})

-- Close certain filetypes with q
au("FileType", {
  group = ag("CloseWithQ", { clear = true }),
  pattern = {
    "help", "man", "qf", "lspinfo", "checkhealth",
    "notify", "Trouble", "startuptime", "tsplayground"
  },
  callback = function(e)
    vim.bo[e.buf].buflisted = false
    vim.keymap.set("n", "q", "<cmd>close<cr>", { buffer = e.buf, silent = true })
  end,
})

-- Trim trailing whitespace
au("BufWritePre", {
  group = ag("TrimWhitespace", { clear = true }),
  callback = function()
    if vim.tbl_contains({ "markdown", "diff" }, vim.bo.filetype) then
      return
    end
    local pos = vim.fn.getpos(".")
    pcall(function() vim.cmd([[%s/\s\+$//e]]) end)
    vim.fn.setpos(".", pos)
  end,
})

-- Language-specific settings
au("FileType", {
  group = ag("WebSettings", { clear = true }),
  pattern = { "html", "css", "javascript", "typescript", "json", "yaml" },
  callback = function()
    vim.opt_local.shiftwidth = 2
    vim.opt_local.tabstop = 2
    vim.opt_local.softtabstop = 2
  end,
})

au("FileType", {
  group = ag("PythonSettings", { clear = true }),
  pattern = "python",
  callback = function()
    vim.opt_local.colorcolumn = "88"
    vim.opt_local.shiftwidth = 4
    vim.opt_local.tabstop = 4
    vim.opt_local.softtabstop = 4
  end,
})

au("FileType", {
  group = ag("GoSettings", { clear = true }),
  pattern = "go",
  callback = function()
    vim.opt_local.shiftwidth = 4
    vim.opt_local.tabstop = 4
    vim.opt_local.expandtab = false
    vim.opt_local.colorcolumn = "120"
  end,
})

au("FileType", {
  group = ag("MarkdownSettings", { clear = true }),
  pattern = "markdown",
  callback = function()
    vim.opt_local.wrap = true
    vim.opt_local.spell = true
    vim.opt_local.linebreak = true
    vim.opt_local.conceallevel = 2
  end,
})

au("FileType", {
  group = ag("RustSettings", { clear = true }),
  pattern = "rust",
  callback = function()
    vim.opt_local.colorcolumn = "100"
  end,
})

-- Large file handling
au("BufReadPre", {
  group = ag("LargeFile", { clear = true }),
  callback = function(args)
    local ok, stats = pcall(vim.uv.fs_stat, vim.api.nvim_buf_get_name(args.buf))
    local max_filesize = 1024 * 1024

    if ok and stats and stats.size > max_filesize then
      vim.b[args.buf].large_file = true
      vim.opt_local.foldmethod = "manual"
      vim.opt_local.undolevels = -1
      vim.opt_local.swapfile = false
      vim.opt_local.loadplugins = false

      vim.schedule(function()
        vim.bo[args.buf].syntax = ""
        vim.notify("Large file detected, features disabled", vim.log.levels.WARN)
      end)
    end
  end,
})

-- Terminal settings
au("TermOpen", {
  group = ag("TermSettings", { clear = true }),
  callback = function()
    vim.opt_local.number = false
    vim.opt_local.relativenumber = false
    vim.opt_local.signcolumn = "no"
    vim.cmd("startinsert")
  end,
})

-- Auto reload files
au({ "FocusGained", "BufEnter" }, {
  group = ag("AutoReload", { clear = true }),
  callback = function()
    if vim.fn.mode() ~= 'c' then
      vim.cmd("checktime")
    end
  end,
})

-- Check if buffer changed outside vim
au({ "CursorHold", "CursorHoldI" }, {
  group = ag("AutoRead", { clear = true }),
  callback = function()
    if vim.fn.getcmdwintype() == '' then
      vim.cmd("checktime")
    end
  end,
})

-- Auto create directories on save
au("BufWritePre", {
  group = ag("AutoCreateDir", { clear = true }),
  callback = function(args)
    if args.match:match("^%w%w+://") then
      return
    end
    local file = vim.uv.fs_realpath(args.match) or args.match
    vim.fn.mkdir(vim.fn.fnamemodify(file, ":p:h"), "p")
  end,
})

-- Clear command line after some time
au("CmdlineLeave", {
  group = ag("ClearCmdline", { clear = true }),
  callback = function()
    vim.defer_fn(function()
      if vim.fn.mode() == 'n' then
        vim.cmd('echo ""')
      end
    end, 5000)
  end,
})
