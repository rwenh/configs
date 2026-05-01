-- lua/core/rain.lua — Matrix rain animation engine
--
-- Extracted from ui.lua (was 500 lines inside snacks config()).
-- This module is self-contained and independently testable.
--

local M = {}

-- ── Constants ─────────────────────────────────────────────────────────────────
local DECODE_MS      = 1200   -- logo decode phase duration (ms)
local DRAIN_MS       = 420    -- drain animation duration (ms)
local TICK_MS        = 80     -- normal frame interval (ms)
local TICK_DRAIN_MS  = 22     -- accelerated frame during drain
local LOGO_COLS      = 64     -- display width of the logo box

-- ── Seed RNG ──────────────────────────────────────────────────────────────────

math.randomseed(os.time() + vim.fn.getpid())

-- ── Character pool ────────────────────────────────────────────────────────────
local POOL = {
  "ア","イ","ウ","エ","オ","カ","キ","ク","ケ","コ",
  "サ","シ","ス","セ","ソ","タ","チ","ツ","テ","ト",
  "ナ","ニ","ヌ","ネ","ノ","ハ","ヒ","フ","ヘ","ホ",
  "0","1","2","3","4","5","6","7","8","9",
  "A","B","C","D","E","F","λ","Ψ","Ω","∑","∂","∇",
  "{","}","[","]","<",">","/","\\","=","#",
  "░","▒","▓","│","┼","╋","╬","╔","╗","╚","╝",
}
local function rc() return POOL[math.random(#POOL)] end

-- ── Tiered_random helper ─────────────────────────────────────────────
-- tiers = { { cumulative_prob, value_fn }, ... }
local function tiered_random(tiers)
  local r = math.random()
  for _, tier in ipairs(tiers) do
    if r < tier[1] then return tier[2]() end
  end
  return tiers[#tiers][2]()
end

local function rand_speed()
  return tiered_random({
    { 0.30, function() return 0.12 + math.random() * 0.10 end },
    { 0.65, function() return 0.30 + math.random() * 0.20 end },
    { 1.00, function() return 0.65 + math.random() * 0.45 end },
  })
end

local function rand_trail()
  return tiered_random({
    { 0.20, function() return  7 + math.random(7) end },
    { 0.70, function() return  3 + math.random(3) end },
    { 1.00, function() return  1 + math.random(1) end },
  })
end

-- ── Logo defined once; header derived from it ────────────────────────
local LOGO_LINES = {
  "╔══════════════════════════════════════════════════════════════╗",
  "║  ███╗   ██╗██╗   ██╗██╗███╗   ███╗ ██╗██████╗ ███████╗     ║",
  "║  ████╗  ██║██║   ██║██║████╗ ████║ ██║██╔══██╗██╔════╝     ║",
  "║  ██╔██╗ ██║██║   ██║██║██╔████╔██║ ██║██║  ██║█████╗       ║",
  "║  ██║╚██╗██║╚██╗ ██╔╝██║██║╚██╔╝██║ ██║██║  ██║██╔══╝       ║",
  "║  ██║ ╚████║ ╚████╔╝ ██║██║ ╚═╝ ██║ ██║██████╔╝███████╗     ║",
  "║  ╚═╝  ╚═══╝  ╚═══╝  ╚═╝╚═╝     ╚═╝ ╚═╝╚═════╝ ╚══════╝    ║",
  "║                                                              ║",
  "║  [ LSP ]  [ DAP ]  [ TREESITTER ]  [ 20+ LANGS ]           ║",
  "╚══════════════════════════════════════════════════════════════╝",
}
local LOGO_ROWS = #LOGO_LINES

function M.logo_header(version)
  local lines = vim.deepcopy(LOGO_LINES)
  -- Replace the info line with version stamp
  lines[9] = ("║  [ LSP ]  [ DAP ]  [ TREESITTER ]  [ 20+ LANGS ]  v%-4s║"):format(version or "?")
  return table.concat(lines, "\n")
end

local QUOTES = {
  '"Fix one thing, two more break.  That\'s growth."',
  '"Debugging is twice as hard as writing the code."  — Kernighan',
  '"First, solve the problem. Then, write the code."  — Johnson',
  '"The best error message is the one that never shows."  — Fuchs',
  '"Programs must be written for people to read."  — Abelson',
  '"Any fool can write code a computer understands."  — Fowler',
}
function M.random_quote() return QUOTES[math.random(#QUOTES)] end

-- ── Named namespaces (idempotent on reload) ───────────────────────────
local _ns   = vim.api.nvim_create_namespace("MatrixRain")
local _rns  = vim.api.nvim_create_namespace("MatrixRefl")
local _logo_ns = vim.api.nvim_create_namespace("MatrixLogo")

-- ── Engine state ──────────────────────────────────────────────────────────────
local _phase        = "idle"
local _timer        = nil
local _rbuf, _rwin  = nil, nil   -- main rain float
local _xbuf, _xwin  = nil, nil   -- reflection float
local _drain_start  = 0
local _decode_start = 0
local _decode_cells = 0
local _logo_row_offset = 0
local _wake         = {}

local R_ROWS, R_COLS = 0, 0
local rain_cols  = {}
local refl_cols  = {}

-- ── Destroy_float helper ─────────────────────────────────────────────
local function destroy_float(win, buf)
  if win and pcall(vim.api.nvim_win_is_valid, win) and vim.api.nvim_win_is_valid(win) then
    pcall(vim.api.nvim_win_close, win, true)
  end
  if buf and pcall(vim.api.nvim_buf_is_valid, buf) and vim.api.nvim_buf_is_valid(buf) then
    pcall(vim.api.nvim_buf_delete, buf, { force = true })
  end
end

local function close_rain()
  if _timer then
    pcall(function() _timer:stop(); _timer:close() end)
    _timer = nil
  end
  destroy_float(_rwin, _rbuf)
  destroy_float(_xwin, _xbuf)
  _rwin = nil; _rbuf = nil
  _xwin = nil; _xbuf = nil
  _phase = "idle"
end
M.close = close_rain

-- ── Column initialisation ────────────────────────────────────────────────────
local function new_col(rows)
  return {
    head   = math.random() * rows,
    speed  = rand_speed(),
    trail  = rand_trail(),
    chars  = {},
    active = math.random() > 0.25,
    pause  = math.random(20),
    flick  = 0,
  }
end

local function get_char(col, r)
  if not col.chars[r] then col.chars[r] = rc() end
  return col.chars[r]
end

local function init_cols(rows, cols)
  rain_cols = {}
  for c = 1, cols do rain_cols[c] = new_col(rows) end
end

local function init_wake(rows, cols)
  _wake = {}
  for r = 1, rows do
    _wake[r] = {}
    for c = 1, cols do _wake[r][c] = 0 end
  end
end

local function init_refl(rows, cols)
  refl_cols = {}
  for c = 1, cols do
    refl_cols[c] = {
      head   = math.random() * rows,
      speed  = -(0.08 + math.random() * 0.12),
      trail  = 2 + math.random(3),
      chars  = {},
      active = math.random() > 0.50,
      pause  = math.random(30),
    }
  end
end

-- ── Tick ──────────────────────────────────────────────────────────────────────
local function tick(rows, cols, drain_factor)
  drain_factor = drain_factor or 1.0
  for c = 1, cols do
    local col = rain_cols[c]
    if col.active then
      col.head = col.head + col.speed * drain_factor
      if math.random() < 0.30 then
        local r = math.random(rows)
        col.chars[r] = rc()
      end
      if col.head > rows + col.trail then
        col.active = false
        col.pause  = math.random(15)
        col.head   = -(col.trail)
        col.trail  = rand_trail()
        col.speed  = rand_speed()
      end
    else
      col.pause = col.pause - 1
      if col.pause <= 0 then col.active = true end
    end
    if col.flick > 0 then col.flick = col.flick - 1
    elseif math.random() < 0.004 then col.flick = 1 + math.random(2) end
  end

  local str_lines    = {}
  local class_matrix = {}
  for r = 1, rows do
    local row_chars, row_cls = {}, {}
    for c = 1, cols do
      local col   = rain_cols[c]
      local hdist = math.floor(col.head) - r
      local ch, cls
      if hdist == 0 then
        ch  = get_char(col, r)
        cls = (col.flick > 0) and 6 or 4
      elseif hdist > 0 and hdist <= col.trail then
        ch = get_char(col, r)
        if     hdist == 1 then cls = (col.flick > 0) and 6 or 3
        elseif hdist <= 3 then cls = 2
        else                   cls = 1
        end
      else
        ch = " "; cls = 0
      end
      row_chars[c] = ch
      row_cls[c]   = cls
    end
    str_lines[r]    = table.concat(row_chars)
    class_matrix[r] = row_cls
  end
  return str_lines, class_matrix
end

-- ── Paint_layer() unifies the 4 paint functions ──────────────────────
-- get_hl_fn(r, c) → { hl_group, char } | nil
local CLASS_HL = {
  [1] = "MatrixDim", [2] = "MatrixMid", [3] = "MatrixTrail",
  [4] = "MatrixHead", [5] = "MatrixGlow", [6] = "MatrixFlick",
}

local function paint_layer(buf, ns, rows, line_fn, hl_fn)
  if not buf or not vim.api.nvim_buf_is_valid(buf) then return end
  local lines = {}
  for r = 1, rows do lines[r] = line_fn(r) end
  pcall(vim.api.nvim_buf_set_lines, buf, 0, rows, false, lines)
  pcall(vim.api.nvim_buf_clear_namespace, buf, ns, 0, rows)
  for r = 1, rows do
    local byte_col = 0
    local row_data = hl_fn(r)
    if row_data then
      for _, entry in ipairs(row_data) do
        local ch, group = entry[1], entry[2]
        local blen = #ch
        if group then
          pcall(vim.api.nvim_buf_set_extmark, buf, ns, r - 1, byte_col, {
            end_col  = byte_col + blen,
            hl_group = group,
            priority = entry[3] or 500,
          })
        end
        byte_col = byte_col + blen
      end
    end
  end
end

-- ── Paint rain ────────────────────────────────────────────────────────────────
local function paint_rain(str_lines, class_matrix, rows, cols, wake_grid)
  if not _rbuf or not vim.api.nvim_buf_is_valid(_rbuf) then return end
  pcall(vim.api.nvim_buf_set_lines, _rbuf, 0, rows, false, str_lines)
  pcall(vim.api.nvim_buf_clear_namespace, _rbuf, _ns, 0, rows)
  for r = 1, rows do
    local byte_col = 0
    for c = 1, cols do
      local cls = class_matrix[r][c]
      local ch  = (cls == 0) and " " or get_char(rain_cols[c], r)
      local blen = #ch
      local eff_cls = cls
      if wake_grid and wake_grid[r] and (wake_grid[r][c] or 0) > 0 then
        eff_cls = 4
        if cls == 0 then ch = rc() end
      end
      if eff_cls > 0 then
        pcall(vim.api.nvim_buf_set_extmark, _rbuf, _ns, r - 1, byte_col, {
          end_col  = byte_col + blen,
          hl_group = CLASS_HL[eff_cls],
          priority = 500,
        })
      end
      if cls == 4 and c < cols - 1 then
        pcall(vim.api.nvim_buf_set_extmark, _rbuf, _ns, r - 1, byte_col + blen, {
          end_col  = byte_col + blen * 2,
          hl_group = "MatrixGlow",
          priority = 400,
        })
      end
      byte_col = byte_col + blen
    end
  end
end

-- ── Paint reflection ──────────────────────────────────────────────────────────
local function paint_refl(rows, cols)
  if not _xbuf or not vim.api.nvim_buf_is_valid(_xbuf) then return end
  for c = 1, cols do
    local col = refl_cols[c]
    if col.active then
      col.head = col.head + col.speed
      if col.head < -(col.trail) then
        col.active = false; col.pause = math.random(40)
        col.head = rows + col.trail
        col.speed = -(0.08 + math.random() * 0.12)
      end
    else
      col.pause = col.pause - 1
      if col.pause <= 0 then col.active = true end
    end
  end
  local str_lines = {}
  for r = 1, rows do
    local row = {}
    for c = 1, cols do
      local col = refl_cols[c]
      local hdist = math.floor(col.head) - r
      row[c] = (hdist >= 0 and hdist < col.trail) and (col.chars[r] or rc()) or " "
    end
    str_lines[r] = table.concat(row)
  end
  pcall(vim.api.nvim_buf_set_lines, _xbuf, 0, rows, false, str_lines)
  pcall(vim.api.nvim_buf_clear_namespace, _xbuf, _rns, 0, rows)
  for r = 1, rows do
    local byte_col = 0
    for c = 1, cols do
      local col   = refl_cols[c]
      local hdist = math.floor(col.head) - r
      local ch    = (hdist >= 0 and hdist < col.trail) and (col.chars[r] or " ") or " "
      local blen  = #ch
      if hdist >= 0 and hdist < col.trail then
        pcall(vim.api.nvim_buf_set_extmark, _xbuf, _rns, r - 1, byte_col, {
          end_col = byte_col + blen, hl_group = "MatrixRefl", priority = 300,
        })
      end
      byte_col = byte_col + blen
    end
  end
end

-- ── Paint logo ────────────────────────────────────────────────────────────────

local function logo_chars(line)
  local codes = vim.fn.str2list(line)
  local chars  = {}
  for _, code in ipairs(codes) do
    table.insert(chars, vim.fn.nr2char(code))
  end
  return chars
end

local function paint_logo_decode(revealed_cells)
  if not _rbuf or not vim.api.nvim_buf_is_valid(_rbuf) then return end
  pcall(vim.api.nvim_buf_clear_namespace, _rbuf, _logo_ns, 0, -1)
  for li = 1, LOGO_ROWS do
    local r = _logo_row_offset + li
    if r >= 1 and r <= R_ROWS then
      local chars      = logo_chars(LOGO_LINES[li])
      local line_start = (li - 1) * LOGO_COLS
      local byte_col   = 0
      for ci, ch in ipairs(chars) do
        local cell_idx = line_start + ci
        local blen     = #ch
        local hl
        if cell_idx <= revealed_cells then
          local is_border = ch == "║" or ch == "╔" or ch == "╗"
                         or ch == "╚" or ch == "╝" or ch == "═"
          hl = is_border and "MatrixBorder" or "MatrixLogo"
        else
          ch = rc(); hl = "MatrixHead"
        end
        if ch ~= " " then
          pcall(vim.api.nvim_buf_set_extmark, _rbuf, _logo_ns, r - 1, byte_col, {
            end_col = byte_col + blen, hl_group = hl, priority = 600,
          })
        end
        byte_col = byte_col + blen
      end
    end
  end
end

local function paint_logo_final()
  if not _rbuf or not vim.api.nvim_buf_is_valid(_rbuf) then return end
  pcall(vim.api.nvim_buf_clear_namespace, _rbuf, _logo_ns, 0, -1)
  for li = 1, LOGO_ROWS do
    local r = _logo_row_offset + li
    if r >= 1 and r <= R_ROWS then
      local chars    = logo_chars(LOGO_LINES[li])
      local byte_col = 0
      for _, ch in ipairs(chars) do
        local blen = #ch
        if ch ~= " " then
          local is_border = ch == "║" or ch == "╔" or ch == "╗"
                         or ch == "╚" or ch == "╝" or ch == "═"
          pcall(vim.api.nvim_buf_set_extmark, _rbuf, _logo_ns, r - 1, byte_col, {
            end_col = byte_col + blen,
            hl_group = is_border and "MatrixBorder" or "MatrixLogo",
            priority = 600,
          })
        end
        byte_col = byte_col + blen
      end
    end
  end
end

-- ── Wake grid ─────────────────────────────────────────────────────────────────
local WAKE_FRAMES = 6
local function update_wake(rows, cols)
  if not _wake or #_wake == 0 then return end
  for r = 1, rows do
    if _wake[r] then
      for c = 1, cols do
        if (_wake[r][c] or 0) > 0 then _wake[r][c] = _wake[r][c] - 1 end
      end
    end
  end
  for c = 1, cols do
    local col = rain_cols[c]
    if col.active then
      local hr = math.floor(col.head)
      for li = 1, LOGO_ROWS do
        local lr = _logo_row_offset + li
        if math.abs(hr - lr) <= 2 and _wake[lr] then
          _wake[lr][c] = WAKE_FRAMES
          if c > 1    then _wake[lr][c-1] = math.max(_wake[lr][c-1] or 0, WAKE_FRAMES-1) end
          if c < cols then _wake[lr][c+1] = math.max(_wake[lr][c+1] or 0, WAKE_FRAMES-1) end
        end
      end
    end
  end
end

-- ── Main frame ────────────────────────────────────────────────────────────────
local function frame()
  -- FIX: idle-phase bail (v2.3.16 guard preserved)
  if _phase == "idle" then return end

  if not _rbuf or not vim.api.nvim_buf_is_valid(_rbuf)
  or not _rwin or not vim.api.nvim_win_is_valid(_rwin) then
    close_rain(); return
  end

  local now = vim.uv.hrtime() / 1e6

  if _phase == "decode" then
    local elapsed  = now - _decode_start
    local progress = math.min(elapsed / DECODE_MS, 1.0)
    local t        = progress < 0.5
      and (2 * progress * progress)
      or  (1 - 2*(1-progress)*(1-progress))
    _decode_cells = math.floor(t * (LOGO_ROWS * LOGO_COLS))
    local sl, cm = tick(R_ROWS, R_COLS)
    paint_rain(sl, cm, R_ROWS, R_COLS, nil)
    paint_logo_decode(_decode_cells)
    paint_refl(R_ROWS, R_COLS)
    if progress >= 1.0 then _phase = "rain" end

  elseif _phase == "rain" then
    update_wake(R_ROWS, R_COLS)
    local sl, cm = tick(R_ROWS, R_COLS)
    paint_rain(sl, cm, R_ROWS, R_COLS, _wake)
    paint_logo_final()
    paint_refl(R_ROWS, R_COLS)

  elseif _phase == "drain" then
    local elapsed = now - _drain_start
    local factor  = 1.0 + (elapsed / DRAIN_MS) * 8.0
    local alpha   = math.max(0, 1.0 - elapsed / DRAIN_MS)
    local sl, cm  = tick(R_ROWS, R_COLS, factor)
    paint_rain(sl, cm, R_ROWS, R_COLS, nil)
    paint_refl(R_ROWS, R_COLS)

    local blend = math.floor((1.0 - alpha) * 100)
    if _rwin and vim.api.nvim_win_is_valid(_rwin) then
      pcall(function() vim.wo[_rwin].winblend = blend end)
    end
    if _xwin and vim.api.nvim_win_is_valid(_xwin) then
      pcall(function()
        vim.wo[_xwin].winblend = math.min(100, 92 + math.floor((1-alpha)*8))
      end)
    end

    if elapsed >= DRAIN_MS then
      close_rain()
      _phase = "enter"
      vim.defer_fn(function()
        local dash_bufs = {}
        for _, buf in ipairs(vim.api.nvim_list_bufs()) do
          if pcall(function() return vim.bo[buf].filetype == "snacks_dashboard" end) then
            table.insert(dash_bufs, buf)
          end
        end
        local ens = vim.api.nvim_create_namespace("MatrixEnter")
        for _, buf in ipairs(dash_bufs) do
          local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
          for li, line in ipairs(lines) do
            if line ~= "" then
              pcall(vim.api.nvim_buf_set_extmark, buf, ens, li-1, 0, {
                end_col = #line, hl_group = "MatrixTrail", priority = 50,
              })
            end
          end
        end
        local steps, step = 6, 0
        local fade = vim.uv.new_timer()
        fade:start(50, 50, vim.schedule_wrap(function()
          step = step + 1
          if step >= steps then
            fade:stop(); fade:close()
            for _, buf in ipairs(dash_bufs) do
              pcall(vim.api.nvim_buf_clear_namespace, buf, ens, 0, -1)
            end
            _phase = "idle"
          end
        end))
      end, 50)
    end
  end
end

-- ── Trigger drain ─────────────────────────────────────────────────────────────
local function trigger_drain()
  if _phase ~= "rain" and _phase ~= "decode" then return end
  _phase       = "drain"
  _drain_start = vim.uv.hrtime() / 1e6

  if _rwin and vim.api.nvim_win_is_valid(_rwin) then
    pcall(function() vim.wo[_rwin].winblend = 0 end)
  end
  if _xwin and vim.api.nvim_win_is_valid(_xwin) then
    pcall(function() vim.wo[_xwin].winblend = 0 end)
  end

  if _timer then
    pcall(function() _timer:stop(); _timer:close() end)
    _timer = nil
  end

  _timer = vim.uv.new_timer()
  _timer:start(0, TICK_DRAIN_MS, vim.schedule_wrap(function()
    if _phase == "idle" then
      if _timer then pcall(function() _timer:stop(); _timer:close() end); _timer = nil end
      return
    end
    frame()
  end))
end
M.trigger_drain = trigger_drain

-- ── Open rain ─────────────────────────────────────────────────────────────────
local function open_rain()
  close_rain()

  local total_rows = vim.o.lines   - 1
  local total_cols = vim.o.columns

  R_ROWS = total_rows
  R_COLS = total_cols

  init_cols(R_ROWS, R_COLS)
  init_wake(R_ROWS, R_COLS)
  init_refl(R_ROWS, R_COLS)

  _logo_row_offset = math.max(0, math.floor((total_rows - LOGO_ROWS) / 2) - 2)

  -- ── Main float ──────────────────────────────────────────────────────────────
  _rbuf = vim.api.nvim_create_buf(false, true)
  vim.bo[_rbuf].buftype = "nofile"; vim.bo[_rbuf].bufhidden = "wipe"
  vim.bo[_rbuf].swapfile = false; vim.bo[_rbuf].modifiable = true

  local blank = {}
  for r = 1, R_ROWS do blank[r] = string.rep(" ", R_COLS) end
  vim.api.nvim_buf_set_lines(_rbuf, 0, -1, false, blank)

  local ok_rwin
  ok_rwin, _rwin = pcall(vim.api.nvim_open_win, _rbuf, false, {
    relative = "editor", row = 0, col = 0,
    width = total_cols, height = total_rows,
    style = "minimal", focusable = false, zindex = 200,
  })
  if not ok_rwin then
    close_rain()
    vim.notify("[rain] terminal too small for rain overlay", vim.log.levels.INFO)
    return
  end
  vim.wo[_rwin].winblend   = 100
  vim.wo[_rwin].wrap       = false
  vim.wo[_rwin].cursorline = false
  vim.wo[_rwin].number     = false
  vim.wo[_rwin].signcolumn = "no"
  vim.wo[_rwin].winhighlight = "Normal:MatrixRainBg"

  -- ── Reflection float ────────────────────────────────────────────────────────
  _xbuf = vim.api.nvim_create_buf(false, true)
  vim.bo[_xbuf].buftype = "nofile"; vim.bo[_xbuf].bufhidden = "wipe"
  vim.bo[_xbuf].swapfile = false; vim.bo[_xbuf].modifiable = true
  vim.api.nvim_buf_set_lines(_xbuf, 0, -1, false, blank)

  local ok_xwin
  ok_xwin, _xwin = pcall(vim.api.nvim_open_win, _xbuf, false, {
    relative = "editor", row = 0, col = 0,
    width = total_cols, height = total_rows,
    style = "minimal", focusable = false, zindex = 190,
  })
  if not ok_xwin then close_rain(); return end
  vim.wo[_xwin].winblend   = 92
  vim.wo[_xwin].wrap       = false
  vim.wo[_xwin].cursorline = false
  vim.wo[_xwin].number     = false
  vim.wo[_xwin].signcolumn = "no"

  _phase        = "decode"
  _decode_start = vim.uv.hrtime() / 1e6
  _decode_cells = 0

  frame()

  _timer = vim.uv.new_timer()
  _timer:start(TICK_MS, TICK_MS, vim.schedule_wrap(function()
    if _phase == "idle" or _phase == "drain" then return end
    frame()
  end))
end
M.open = open_rain

-- ── VimResized guard ──────────────────────────────────────────────────────────

vim.api.nvim_create_autocmd("VimResized", {
  group    = vim.api.nvim_create_augroup("MatrixRainResize", { clear = true }),
  callback = function()
    if _phase ~= "idle" then
      close_rain()
      -- Brief delay so the terminal has finished resizing before we re-open.
      vim.defer_fn(function()
        -- Only reopen if the dashboard is still visible.
        for _, buf in ipairs(vim.api.nvim_list_bufs()) do
          local ok, ft = pcall(function() return vim.bo[buf].filetype end)
          if ok and ft == "snacks_dashboard" then
            open_rain()
            return
          end
        end
      end, 100)
    end
  end,
})

return M
