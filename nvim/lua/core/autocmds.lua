-- lua/core/autocmds.lua — autocommands
--

local au = vim.api.nvim_create_autocmd
local ag = vim.api.nvim_create_augroup

-- ── Shared helpers ─────────────────────────────────────────────────────────────

---@param buf integer
---@return boolean  true when buf is a normal editable file buffer
local function is_real_buf(buf)
  local ok, bt = pcall(function() return vim.bo[buf].buftype end)
  return ok and bt == ""
end

-- ── Filetypes that should close with 'q' ──────────────────────────────────────

local EPHEMERAL_FT = {
  "help", "man", "qf", "lspinfo", "checkhealth",
  "notify", "startuptime", "OverseerList",
}

-- ═══════════════════════════════════════════════════════════════════════════
-- HIGHLIGHT ON YANK
-- ═══════════════════════════════════════════════════════════════════════════

au("TextYankPost", {
  group    = ag("HighlightYank", { clear = true }),
  callback = function()
    pcall(vim.highlight.on_yank, { timeout = 200 })
  end,
})

-- ═══════════════════════════════════════════════════════════════════════════
-- RESTORE CURSOR POSITION
-- ═══════════════════════════════════════════════════════════════════════════

au("BufReadPost", {
  group    = ag("RestoreCursor", { clear = true }),
  callback = function(e)
    if not is_real_buf(e.buf) then return end

    local ok, mark = pcall(vim.api.nvim_buf_get_mark, e.buf, '"')
    if not ok or not mark then return end

    local lcount = vim.api.nvim_buf_line_count(e.buf)
    if mark[1] and mark[1] > 0 and mark[1] <= lcount then
      -- Target the window that actually shows this buffer, not window 0.
      local wins = vim.fn.win_findbuf(e.buf)
      if #wins > 0 then
        pcall(vim.api.nvim_win_set_cursor, wins[1], mark)
      end
    end
  end,
})

-- ═══════════════════════════════════════════════════════════════════════════
-- RESIZE SPLITS ON WINDOW RESIZE
-- ═══════════════════════════════════════════════════════════════════════════

au("VimResized", {
  group    = ag("ResizeSplits", { clear = true }),
  callback = function()
    local tab_before = vim.fn.tabpagenr()
    pcall(function()
      vim.cmd("tabdo wincmd =")
      -- Re-query: tabdo may have closed or reordered tabs.
      local total  = vim.fn.tabpagenr("$")
      local target = math.min(tab_before, total)
      if target > 0 then vim.cmd("tabnext " .. target) end
    end)
  end,
})

-- ═══════════════════════════════════════════════════════════════════════════
-- CLOSE CERTAIN WINDOWS WITH 'q'
-- ═══════════════════════════════════════════════════════════════════════════

au("FileType", {
  group    = ag("CloseWithQ", { clear = true }),
  pattern  = EPHEMERAL_FT,
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
    if not is_real_buf(e.buf) then return end

    -- Skip binary files — byte-level content must not be altered.
    if vim.bo[e.buf].binary then return end

    local ft = vim.bo[e.buf].filetype
    if vim.tbl_contains(
      { "markdown", "markdown_inline", "diff", "rst", "asciidoc", "mail" }, ft
    ) then return end

    if #vim.api.nvim_buf_get_lines(e.buf, 0, -1, false) == 0 then return end

    pcall(function()
      local lines   = vim.api.nvim_buf_get_lines(e.buf, 0, -1, false)
      local trimmed = {}
      local dirty   = false

      for i, line in ipairs(lines) do
        local t = line:gsub("%s+$", "")
        trimmed[i] = t
        if t ~= line then dirty = true end
      end

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
  callback = function(e)
    local buf = e.buf or vim.api.nvim_get_current_buf()
    local ok, bt = pcall(function() return vim.bo[buf].buftype end)
    if ok and bt == "" then
      pcall(vim.cmd, "checktime")
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
    if not is_real_buf(e.buf) then return end

    local name = vim.api.nvim_buf_get_name(e.buf)
    if name == "" then return end

    local ok, path_util = pcall(require, "core.util.path")
    if not ok then return end

    local buf_dir = vim.fn.fnamemodify(name, ":h")
    local root    = path_util.find_root(buf_dir)

    if root and root ~= vim.fn.getcwd() then
      pcall(vim.cmd.cd, root)
    end
  end,
})

-- ═══════════════════════════════════════════════════════════════════════════
-- FILETYPE DETECTION FOR COBOL AND VHDL
-- ═══════════════════════════════════════════════════════════════════════════

pcall(function()
  vim.filetype.add({
    extension = {
      cob  = "cobol", cbl  = "cobol", cpy = "cobol",
      CBL  = "cobol", COB  = "cobol",
      vhd  = "vhdl",  vhdl = "vhdl",  vho = "vhdl",
    },
  })
end)

-- ═══════════════════════════════════════════════════════════════════════════
-- LARGE FILE OPTIMISATION
-- ═══════════════════════════════════════════════════════════════════════════

local large_file_group = ag("LargeFile", { clear = true })

au("BufReadPre", {
  group    = large_file_group,
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

au("BufReadPost", {
  group    = large_file_group,
  callback = function(e)
    if vim.b[e.buf] and vim.b[e.buf].large_file then
      pcall(vim.treesitter.stop, e.buf)
    end
  end,
})

-- Re-stamp foldmethod=manual on BufWinEnter for large files.
-- options.lua sets foldmethod="expr" globally; that default re-applies when
-- a buffer enters a new window, overriding the manual fold set above.
au("BufWinEnter", {
  group    = large_file_group,
  callback = function(e)
    if vim.b[e.buf] and vim.b[e.buf].large_file then
      pcall(function() vim.opt_local.foldmethod = "manual" end)
    end
  end,
})
