-- lua/core/autocmds.lua - Autocommands with comprehensive error handling

local au = vim.api.nvim_create_autocmd
local ag = vim.api.nvim_create_augroup

-- ═══════════════════════════════════════════════════════════════════════════
-- HIGHLIGHT ON YANK
-- ═══════════════════════════════════════════════════════════════════════════

au("TextYankPost", {
  group    = ag("HighlightYank", { clear = true }),
  callback = function()
    pcall(function()
      vim.highlight.on_yank({ timeout = 200 })
    end)
  end,
})

-- ═══════════════════════════════════════════════════════════════════════════
-- RESTORE CURSOR POSITION
-- ═══════════════════════════════════════════════════════════════════════════

au("BufReadPost", {
  group    = ag("RestoreCursor", { clear = true }),
  callback = function(e)
    -- FIX #4: Skip non-normal buffers (help, man, terminal, quickfix, etc.).
    if vim.bo[e.buf].buftype ~= "" then return end

    local ok, mark = pcall(function()
      return vim.api.nvim_buf_get_mark(e.buf, '"')
    end)
    if not ok or not mark then return end

    local lcount = vim.api.nvim_buf_line_count(e.buf)
    if mark[1] and mark[1] > 0 and mark[1] <= lcount then
      pcall(vim.api.nvim_win_set_cursor, 0, mark)
    end
  end,
})

-- ═══════════════════════════════════════════════════════════════════════════
-- RESIZE SPLITS ON WINDOW RESIZE
-- ═══════════════════════════════════════════════════════════════════════════

au("VimResized", {
  group    = ag("ResizeSplits", { clear = true }),
  callback = function()
    local ok, tab = pcall(function()
      return vim.fn.tabpagenr()
    end)
    if not ok then return end

    pcall(function()
      vim.cmd("tabdo wincmd =")
      vim.cmd("tabnext " .. tab)
    end)
  end,
})

-- ═══════════════════════════════════════════════════════════════════════════
-- CLOSE CERTAIN WINDOWS WITH 'q'
-- ═══════════════════════════════════════════════════════════════════════════

au("FileType", {
  group   = ag("CloseWithQ", { clear = true }),
  pattern = { "help", "man", "qf", "lspinfo", "checkhealth", "notify", "startuptime", "query" },
  callback = function(e)
    pcall(function()
      vim.bo[e.buf].buflisted = false
      vim.keymap.set("n", "q", "<cmd>close<cr>", { buffer = e.buf, silent = true })
    end)
  end,
})

-- ═══════════════════════════════════════════════════════════════════════════
-- REMOVE TRAILING WHITESPACE ON SAVE
-- ═══════════════════════════════════════════════════════════════════════════

au("BufWritePre", {
  group    = ag("TrimWhitespace", { clear = true }),
  callback = function(e)
    -- FIX #6: Accept event arg and read filetype from e.buf, not vim.bo.filetype.
    if vim.tbl_contains({ "markdown", "diff" }, vim.bo[e.buf].filetype) then return end

    pcall(function()
      local view = vim.fn.winsaveview()
      vim.cmd([[%s/\s\+$//e]])
      vim.fn.winrestview(view)
    end)
  end,
})

-- ═══════════════════════════════════════════════════════════════════════════
-- CHECK FOR EXTERNAL FILE CHANGES
-- ═══════════════════════════════════════════════════════════════════════════

au({ "FocusGained", "TermClose", "TermLeave" }, {
  group    = ag("Checktime", { clear = true }),
  callback = function()
    if vim.o.buftype ~= "nofile" then
      pcall(function() vim.cmd("checktime") end)
    end
  end,
})

-- ═══════════════════════════════════════════════════════════════════════════
-- LANGUAGE-SPECIFIC INDENT SETTINGS
-- ═══════════════════════════════════════��═══════════════════════════════════

au("FileType", {
  group   = ag("WebDev", { clear = true }),
  pattern = { "html", "css", "javascript", "typescript", "json", "yaml" },
  callback = function()
    pcall(function()
      vim.opt_local.shiftwidth = 2
      vim.opt_local.tabstop    = 2
    end)
  end,
})

au("FileType", {
  group   = ag("GoDev", { clear = true }),
  pattern = { "go" },
  callback = function()
    pcall(function()
      vim.opt_local.shiftwidth = 4
      vim.opt_local.tabstop    = 4
      vim.opt_local.expandtab  = false
    end)
  end,
})

au("FileType", {
  group   = ag("Markdown", { clear = true }),
  pattern = "markdown",
  callback = function()
    pcall(function()
      vim.opt_local.wrap  = true
      vim.opt_local.spell = true
    end)
  end,
})

-- ═══════════════════════════════════════════════════════════════════════════
-- TERMINAL UI CLEANUP
-- ═══════════════════════════════════════════════════════════════════════════

au("TermOpen", {
  group    = ag("Terminal", { clear = true }),
  callback = function()
    pcall(function()
      vim.opt_local.number         = false
      vim.opt_local.relativenumber = false
      vim.opt_local.signcolumn     = "no"
    end)
  end,
})

-- ═══════════════════════════════════════════════════════════════════════════
-- AUTO CD TO PROJECT ROOT
-- ═══════════════════════════════════════════════════════════════════════════

au("BufEnter", {
  group    = ag("AutoCdRoot", { clear = true }),
  callback = function(e)
    if not vim.g.auto_cd_root then return end

    -- FIX #5 & #8: Guard against special buffers and check for changes
    if vim.bo[e.buf].buftype ~= "" then return end

    local name = vim.api.nvim_buf_get_name(e.buf)
    if name == "" then return end

    -- FIX #8 (cross-file): Only call cd when root differs from cwd
    local ok, path_util = pcall(require, "core.util.path")
    if not ok then return end

    local root = path_util.find_root()
    if root and root ~= vim.fn.getcwd() then
      pcall(function() vim.cmd.cd(root) end)
    end
  end,
})

-- ═══════════════════════════════════════════════════════════════════════════
-- FILETYPE DETECTION FOR COBOL AND VHDL
-- ═══════════════════════════════════════════════════════════════════════════

-- FIX #9: Replaced BufRead/BufNewFile autocmds with vim.filetype.add().
-- This is the idiomatic Neovim approach — it integrates with the filetype
-- detection system and is evaluated before autocmds.
pcall(function()
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
end)

-- ═══════════════════════════════════════════════════════════════════════════
-- LARGE FILE OPTIMIZATION
-- ═══════════════════════════════════════════════════════════════════════════

au("BufReadPre", {
  group    = ag("LargeFile", { clear = true }),
  callback = function(e)
    local ok, stat = pcall(vim.uv.fs_stat, vim.api.nvim_buf_get_name(e.buf))
    if ok and stat and stat.size > 500 * 1024 then
      vim.b[e.buf].large_file = true

      pcall(function()
        vim.opt_local.foldmethod  = "manual"
        vim.opt_local.spell       = false
        vim.opt_local.cursorline  = false
        vim.opt_local.swapfile    = false
      end)

      -- FIX #7: Defer the notification — BufReadPre fires before the buffer is
      -- displayed. vim.schedule() defers until after render.
      vim.schedule(function()
        vim.notify("Large file — some features disabled", vim.log.levels.WARN)
      end)
    end
  end,
})
