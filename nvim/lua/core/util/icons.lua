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
-- Keyed by the mode string returned by lualine's "mode" component.
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
-- Distinct icons per heading level (H1–H6).
M.headings = {
  "󰲡 ",  -- H1
  "󰲣 ",  -- H2
  "󰲥 ",  -- H3
  "󰲧 ",  -- H4
  "󰲩 ",  -- H5
  "󰲫 ",  -- H6
}

return M
