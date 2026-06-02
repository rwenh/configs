-- lua/core/util/icons.lua — shared Nerd Font icon constants
--

local M = {}

-- ── LSP symbol kinds ──────────────────────────────────────────────────────────
M.kinds = {
  File          = "",
  Module        = "",
  Namespace     = "",
  Package       = "",
  Class         = "",
  Method        = "",
  Property      = "",
  Field         = "",
  Constructor   = "",
  Enum          = "",
  Interface     = "",
  Function      = "",
  Variable      = "",
  Constant      = "",
  String        = "",
  Number        = "",
  Boolean       = "",
  Array         = "",
  Object        = "",
  Key           = "",
  Null          = "",
  EnumMember    = "",
  Struct        = "",
  Event         = "",
  Operator      = "",
  TypeParameter = "",
}

-- ── Diagnostics ───────────────────────────────────────────────────────────────
M.diagnostics = {
  Error = " ",
  Warn  = " ",
  Hint  = " ",
  Info  = " ",
}

-- ── Git diff (lualine) ────────────────────────────────────────────────────────
M.git = {
  added    = "  ",
  modified = "  ",
  removed  = "  ",
}

-- ── DAP ───────────────────────────────────────────────────────────────────────
M.dap = {
  breakpoint          = "●",
  breakpoint_cond     = "◆",
  breakpoint_rejected = "✖",
  stopped             = "▶",
  log_point           = "◉",
}

-- ── File explorer (neo-tree) ──────────────────────────────────────────────────
M.explorer = {
  folder_closed = "",
  folder_open   = "",
  folder_empty  = "󰜌",
  default_file  = "*",
}

-- ── AST role icons (clangd_extensions) ───────────────────────────────────────
M.ast = {
  type                  = "",
  declaration           = "",
  expression            = "",
  specifier             = "",
  statement             = "",
  template_argument     = "",
}

-- ── Lualine mode labels ───────────────────────────────────────────────────────
M.modes = {
  NORMAL      = "󰰓 N",
  INSERT      = "󰰄 I",
  VISUAL      = "󰰫 V",
  ["V-LINE"]  = "󰰫 VL",
  ["V-BLOCK"] = "󰰫 VB",
  COMMAND     = " C",
  TERMINAL    = " T",
  REPLACE     = "󰰟 R",
}

-- ── Markdown heading signs (render-markdown) ──────────────────────────────────
M.headings = {
  "󰲡 ",  -- H1
  "󰲣 ",  -- H2
  "󰲥 ",  -- H3
  "󰲧 ",  -- H4
  "󰲩 ",  -- H5
  "󰲫 ",  -- H6
}

-- ── Progress / spinner ────────────────────────────────────────────────────────
-- Braille spinner for LSP progress indicators and async task feedback.
M.progress = {
  frames  = { "⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏" },
  done    = "✓",
  failed  = "✗",
  pending = "○",
  running = "◌",
}

---@param idx integer  1-based frame index (wrap with modulo)
---@return string
function M.progress.frame(idx)
  local frames = M.progress.frames
  return frames[((idx - 1) % #frames) + 1]
end

-- ── Statusline extras ─────────────────────────────────────────────────────────
-- Small icons for optional statusline components (lualine, etc.).
M.status = {
  lsp_ok       = " ",
  lsp_off      = " ",
  format_on    = "󰉿 ",
  format_off   = "󰉾 ",
  focus        = "󰈈 ",
  readonly     = " ",
  modified     = " ",
  saved        = " ",
  session      = "󱂬 ",
  auto_cd      = "󱉭 ",
  large_file   = "󰈚 ",
  word_count   = "󰈭 ",
}

return M
