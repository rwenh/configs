-- lua/core/autocmds.lua - Autocommands with comprehensive error handling
--
-- FIX (v2.3.1):
--   • TrimWhitespace: the previous Lua line-iterator called nvim_buf_set_lines
--     once PER dirty line inside a loop. Each individual call IS a separate
--     undo entry in Neovim's undo tree (nvim_buf_set_lines always records),
--     so saving a file with 5 dirty lines created 5 undo states — contrary to
--     the comment that claimed otherwise. Fixed: collect all dirty lines first,
--     then replace the entire buffer in ONE nvim_buf_set_lines call covering
--     the full range [0, -1]. When nothing changed the call is skipped
--     entirely, so truly clean files still produce zero undo entries.

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
--
-- FIX (v2.3.1): batch all dirty lines in a single nvim_buf_set_lines call.
--   The previous implementation called nvim_buf_set_lines once per dirty line,
--   which created one undo entry per line — the opposite of the intention.
--   New approach:
--     1. Read all lines.
--     2. Build a new table with whitespace trimmed from every line.
--     3. Compare: if the table is identical (no dirty lines), return early.
--     4. If any line changed, replace the ENTIRE buffer in one call.
--   One call = one undo entry. If nothing changed = zero undo entries.
--   The cursor never moves because we are not using vim.cmd.
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
      local lines   = vim.api.nvim_buf_get_lines(e.buf, 0, -1, false)
      local trimmed = {}
      local dirty   = false

      for i, line in ipairs(lines) do
        local t = line:gsub("%s+$", "")
        trimmed[i] = t
        if t ~= line then dirty = true end
      end

      -- FIX: single call covers the whole buffer — one undo entry total.
      -- Skipped entirely when nothing changed — zero undo entries for clean files.
      if dirty then
        vim.api.nvim_buf_set_lines(e.buf, 0, -1, false, trimmed)
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
