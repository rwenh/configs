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
    -- FIX #4: Skip non-normal buffers (help, man, terminal, quickfix, etc.).
    -- The mark[1] > 0 guard handled most cases silently, but this is explicit
    -- and prevents any edge case on special buffer types.
    if vim.bo[e.buf].buftype ~= "" then return end
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
  -- FIX #6: Accept event arg and read filetype from e.buf, not vim.bo.filetype.
  -- vim.bo.filetype reads the current window's buffer; if a plugin triggers
  -- BufWritePre on a background buffer, the wrong filetype would be checked.
  callback = function(e)
    if vim.tbl_contains({ "markdown", "diff" }, vim.bo[e.buf].filetype) then return end
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
  callback = function(e)
    if not vim.g.auto_cd_root then return end

    -- FIX #5 & #8: Guard against special buffers (NvimTree, terminal, Telescope,
    -- LazyGit, etc.) — these have no real file path and find_root() would do
    -- unnecessary filesystem I/O on every buffer switch.
    --
    -- FIX #8 (cross-file): Only call vim.cmd.cd() when the resolved root
    -- actually differs from cwd. Previously cd() fired unconditionally, which
    -- triggered DirChanged on every BufEnter → cleared path.lua's cache →
    -- forced a cold filesystem walk on the very next BufEnter. With this guard,
    -- cd() only fires when crossing a project boundary, so the cache stays warm.
    if vim.bo[e.buf].buftype ~= "" then return end
    local name = vim.api.nvim_buf_get_name(e.buf)
    if name == "" then return end

    local root = require("core.util.path").find_root()
    if root and root ~= vim.fn.getcwd() then
      vim.cmd.cd(root)
    end
  end,
})

-- FIX #9: Replaced BufRead/BufNewFile autocmds for COBOL and VHDL with
-- vim.filetype.add(). This is the idiomatic Neovim approach — it integrates
-- with the filetype detection system and is evaluated before autocmds, avoiding
-- a race with any plugin that also listens on BufRead for these filetypes.
vim.filetype.add({
  extension = {
    cob  = "cobol",
    cbl  = "cobol",
    cpy  = "cobol",
    CBL  = "cobol",
    COB  = "cobol",
    vhd  = "vhdl",
    vhdl = "vhdl",
    vho  = "vhdl",
  },
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
      -- FIX #7: Defer the notification — BufReadPre fires before the buffer is
      -- displayed. On UIs like noice.nvim the message would flash and vanish
      -- before the user can read it. vim.schedule() defers until after render.
      vim.schedule(function()
        vim.notify("Large file — some features disabled", vim.log.levels.WARN)
      end)
    end
  end,
})
