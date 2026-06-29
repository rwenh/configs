-- lua/core/autocmds.lua — autocommands
--

local au = vim.api.nvim_create_autocmd
local ag = vim.api.nvim_create_augroup

local function is_real_buf(buf)
  local ok, bt = pcall(function() return vim.bo[buf].buftype end)
  return ok and bt == ""
end

local EPHEMERAL_FT = {
  "help", "man", "qf", "lspinfo", "checkhealth",
  "notify", "startuptime", "OverseerList",
}

-- ── Highlight on yank ─────────────────────────────────────────────────────────
au("TextYankPost", {
  group    = ag("HighlightYank", { clear = true }),
  callback = function() pcall(vim.highlight.on_yank, { timeout = 200 }) end,
})

-- ── Restore cursor position ───────────────────────────────────────────────────
au("BufReadPost", {
  group    = ag("RestoreCursor", { clear = true }),
  callback = function(e)
    if not is_real_buf(e.buf) then return end
    local ok, mark = pcall(vim.api.nvim_buf_get_mark, e.buf, '"')
    if not ok or not mark then return end
    local lcount = vim.api.nvim_buf_line_count(e.buf)
    if mark[1] and mark[1] > 0 and mark[1] <= lcount then
      local wins = vim.fn.win_findbuf(e.buf)
      if #wins > 0 then pcall(vim.api.nvim_win_set_cursor, wins[1], mark) end
    end
  end,
})

-- ── Resize splits (phase 2 fix #2: no tab flicker) ───────────────────────────
au("VimResized", {
  group    = ag("ResizeSplits", { clear = true }),
  callback = function()
    local current_tab = vim.api.nvim_get_current_tabpage()
    for _, tabpage in ipairs(vim.api.nvim_list_tabpages()) do
      local wins = vim.api.nvim_tabpage_list_wins(tabpage)
      if #wins > 1 then
        pcall(vim.api.nvim_win_call, wins[1], function()
          vim.cmd("wincmd =")
        end)
      end
    end
    pcall(vim.api.nvim_set_current_tabpage, current_tab)
  end,
})

-- ── Close ephemeral windows with 'q' ─────────────────────────────────────────
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

-- ── Trim trailing whitespace (phase 2 fix #3: in-memory byte count) ──────────
local function buf_byte_size(buf)
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local total = 0
  for _, line in ipairs(lines) do total = total + #line + 1 end
  return total
end

au("BufWritePre", {
  group    = ag("TrimWhitespace", { clear = true }),
  callback = function(e)
    if not is_real_buf(e.buf) then return end
    if vim.bo[e.buf].binary then return end
    if vim.b[e.buf] and vim.b[e.buf].large_file then return end
    if buf_byte_size(e.buf) > 500 * 1024 then return end
    local ft = vim.bo[e.buf].filetype
    if vim.tbl_contains({ "markdown","markdown_inline","diff","rst","asciidoc","mail" }, ft) then return end
    if #vim.api.nvim_buf_get_lines(e.buf, 0, -1, false) == 0 then return end
    local scan_ok, match_line = pcall(vim.api.nvim_buf_call, e.buf, function()
      return vim.fn.search([[\s\+$]], "nw")
    end)
    if not scan_ok or match_line == 0 then return end
    pcall(function()
      local lines   = vim.api.nvim_buf_get_lines(e.buf, 0, -1, false)
      local trimmed = {}
      local dirty   = false
      for i, line in ipairs(lines) do
        local t = line:gsub("%s+$", "")
        trimmed[i] = t
        if t ~= line then dirty = true end
      end
      if dirty then vim.api.nvim_buf_set_lines(e.buf, 0, -1, false, trimmed) end
    end)
  end,
})

-- ── Check for external file changes (phase 3 fix #22) ────────────────────────
--
au({ "FocusGained", "TermClose", "TermLeave" }, {
  group    = ag("Checktime", { clear = true }),
  callback = function(e)
    local buf = e.buf or vim.api.nvim_get_current_buf()
    local ok, bt = pcall(function() return vim.bo[buf].buftype end)
    if not ok or bt ~= "" then return end

    if vim.bo[buf].modified then
      vim.notify(
        "[autocmds] Skipped automatic checktime — buffer has unsaved changes.\n"
        .. "Run :checktime manually when you are ready to reload.",
        vim.log.levels.DEBUG
      )
      return
    end

    pcall(vim.cmd, "checktime")
  end,
})

-- ── Terminal UI cleanup ───────────────────────────────────────────────────────
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

-- ── Auto CD to project root ───────────────────────────────────────────────────
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
    if root and root ~= vim.fn.getcwd() then pcall(vim.cmd.lcd, root) end
  end,
})

-- ── Filetype detection for COBOL and VHDL ────────────────────────────────────
pcall(function()
  vim.filetype.add({
    extension = {
      cob="cobol", cbl="cobol", cpy="cobol", CBL="cobol", COB="cobol",
      vhd="vhdl",  vhdl="vhdl", vho="vhdl",
    },
  })
end)

-- ── Large file optimisation ───────────────────────────────────────────────────
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
    if vim.b[e.buf] and vim.b[e.buf].large_file then pcall(vim.treesitter.stop, e.buf) end
  end,
})
au("BufWinEnter", {
  group    = large_file_group,
  callback = function(e)
    if vim.b[e.buf] and vim.b[e.buf].large_file then
      pcall(function() vim.opt_local.foldmethod = "manual" end)
    end
  end,
})

-- ── NvimIdeReady user event (phase 2 fix #1: fires after LazyDone) ───────────
vim.api.nvim_create_autocmd("User", {
  pattern  = "LazyDone",
  once     = true,
  group    = ag("NvimIdeReadyBridge", { clear = true }),
  callback = function()
    vim.api.nvim_exec_autocmds("User", { pattern = "NvimIdeReady", modeline = false })
  end,
  desc = "Fire User NvimIdeReady after all lazy.nvim plugins are initialised",
})
