-- lua/core/autocmds.lua - Autocommands with comprehensive error handling
--
-- FIX (v2.2.2):
--   • TrimWhitespace: replaced vim.cmd([[%s/\s\+$//e]]) with a Lua line
--     iterator. The regex substitution dirtied the undo tree on every save
--     even when there was nothing to trim, and moved the cursor despite
--     winsaveview/winrestview. The Lua approach only writes back lines that
--     actually changed, so no undo entry is created for clean files.

local au = vim.api.nvim_create_autocmd
local ag = vim.api.nvim_create_augroup

-- ═══════════════════════════════════════════════════════════════════════════
-- HIGHLIGHT ON YANK
-- ═══════════════════════════════════════════════════════════════════════════

au("TextYankPost", {
  group    = ag("HighlightYank", { clear = true }),
  callback = function()
    pcall(function() vim.highlight.on_yank({ timeout = 200 }) end)
  end,
})

-- ═══════════════════════════════════════════════════════════════════════════
-- RESTORE CURSOR POSITION
-- ═══════════════════════════════════════════════════════════════════════════

au("BufReadPost", {
  group    = ag("RestoreCursor", { clear = true }),
  callback = function(e)
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
    local ok_tab, tab = pcall(function() return vim.fn.tabpagenr() end)
    if not ok_tab then return end

    pcall(function()
      vim.cmd("tabdo wincmd =")
      local tab_count = vim.fn.tabpagenr("$")
      if tab > tab_count then tab = tab_count end
      if tab > 0 then vim.cmd("tabnext " .. tab) end
    end)
  end,
})

-- ═══════════════════════════════════════════════════════════════════════════
-- CLOSE CERTAIN WINDOWS WITH 'q'
-- ═══════════════════════════════════════════════════════════════════════════

au("FileType", {
  group    = ag("CloseWithQ", { clear = true }),
  pattern  = { "help", "man", "qf", "lspinfo", "checkhealth", "notify", "startuptime" },
  callback = function(e)
    pcall(function()
      vim.bo[e.buf].buflisted = false
      vim.keymap.set("n", "q", "<cmd>close<cr>", { buffer = e.buf, silent = true })
    end)
  end,
})

-- ═══════════════════════════════════════════════════════════════════════════
-- REMOVE TRAILING WHITESPACE ON SAVE
-- FIX: Lua line-by-line approach:
--   • No undo entry when nothing changes (nvim_buf_set_lines only called
--     for dirty lines).
--   • Cursor never moves — no winsaveview/winrestview needed.
--   • Still skips filetypes where trailing whitespace is significant.
-- ═══════════════════════════════════════════════════════════════════════════

au("BufWritePre", {
  group    = ag("TrimWhitespace", { clear = true }),
  callback = function(e)
    local ft = vim.bo[e.buf].filetype
    if vim.tbl_contains(
      { "markdown", "markdown_inline", "diff", "rst", "asciidoc", "mail" }, ft
    ) then
      return
    end

    pcall(function()
      local lines = vim.api.nvim_buf_get_lines(e.buf, 0, -1, false)
      local dirty = {}
      for i, line in ipairs(lines) do
        local trimmed = line:gsub("%s+$", "")
        if trimmed ~= line then
          dirty[i] = trimmed
        end
      end
      -- Only write back changed lines — no undo entry for untouched buffers
      for i, trimmed in pairs(dirty) do
        vim.api.nvim_buf_set_lines(e.buf, i - 1, i, false, { trimmed })
      end
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
-- ═══════════════════════════════════════════════════════════════════════════

au("FileType", {
  group   = ag("WebDev", { clear = true }),
  pattern = { "html", "css", "javascript", "typescript", "json", "yaml",
              "javascriptreact", "typescriptreact" },
  callback = function()
    pcall(function()
      vim.opt_local.shiftwidth = 2
      vim.opt_local.tabstop    = 2
    end)
  end,
})

au("FileType", {
  group    = ag("GoDev", { clear = true }),
  pattern  = { "go" },
  callback = function()
    pcall(function()
      vim.opt_local.shiftwidth = 4
      vim.opt_local.tabstop    = 4
      vim.opt_local.expandtab  = false
    end)
  end,
})

au("FileType", {
  group    = ag("Markdown", { clear = true }),
  pattern  = { "markdown", "markdown_inline" },
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
    if vim.bo[e.buf].buftype ~= "" then return end

    local name = vim.api.nvim_buf_get_name(e.buf)
    if name == "" then return end

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
        vim.opt_local.foldmethod = "manual"
        vim.opt_local.spell      = false
        vim.opt_local.cursorline = false
        vim.opt_local.swapfile   = false
        vim.opt_local.syntax     = "off"
      end)

      vim.schedule(function()
        vim.notify("Large file — some features disabled", vim.log.levels.WARN)
      end)
    end
  end,
})
