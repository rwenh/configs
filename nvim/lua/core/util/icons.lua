-- lua/core/util/icons.lua — shared Nerd Font icon constants
--

local M = {}

local function icon(codepoint) return vim.fn.nr2char(codepoint, true) end

-- ── LSP symbol kinds ──────────────────────────────────────────────────────────
M.kinds = {
  File          = icon(0xf0219),
  Module        = icon(0xe624),
  Namespace     = icon(0xf0317),
  Package       = icon(0xe624),
  Class         = icon(0xf0317),
  Method        = icon(0xf01a7),
  Property      = icon(0xe79b),
  Field         = icon(0xe716),
  Constructor   = icon(0xf425),
  Enum          = icon(0xf0558),
  Interface     = icon(0xf0558),
  Function      = icon(0xf0295),
  Variable      = icon(0xf01a7),
  Constant      = icon(0xf03ff),
  String        = icon(0xf002c),
  Number        = icon(0xf03a0),
  Boolean       = icon(0x25e9),
  Array         = icon(0xf016a),
  Object        = icon(0xf0169),
  Key           = icon(0xf030b),
  Null          = icon(0xf07e2),
  EnumMember    = icon(0xf15d),
  Struct        = icon(0xf0317),
  Event         = icon(0xf0e7),
  Operator      = icon(0xf0195),
  TypeParameter = icon(0xf0284),
}

-- ── Diagnostics ───────────────────────────────────────────────────────────────
M.diagnostics = {
  Error = icon(0xf057) .. " ",
  Warn  = icon(0xf071) .. " ",
  Hint  = icon(0xf0eb) .. " ",
  Info  = icon(0xf05a) .. " ",
}

-- ── Git diff (lualine) ────────────────────────────────────────────────────────
M.git = {
  added    = icon(0xf0fe) .. "  ",
  modified = icon(0xf14b) .. "  ",
  removed  = icon(0xf146) .. "  ",
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
  folder_closed = icon(0xe5ff),
  folder_open   = icon(0xe5fe),
  folder_empty  = icon(0xf070c),
  default_file  = "*",
}

-- ── AST role icons (clangd_extensions) ───────────────────────────────────────
M.ast = {
  type                    = icon(0x1f123),
  declaration             = icon(0x1f113),
  expression              = icon(0x1f114),
  specifier               = icon(0x1f122),
  statement               = ";",
  ["template argument"]  = icon(0x1f183),
}

-- ── Lualine mode labels ───────────────────────────────────────────────────────
M.modes = {
  NORMAL      = icon(0xf0c13) .. " N",
  INSERT      = icon(0xf0c04) .. " I",
  VISUAL      = icon(0xf0c2b) .. " V",
  ["V-LINE"]  = icon(0xf0c2b) .. " VL",
  ["V-BLOCK"] = icon(0xf0c2b) .. " VB",
  COMMAND     = icon(0xf0142) .. " C",
  TERMINAL    = icon(0xf018d) .. " T",
  REPLACE     = icon(0xf0c1f) .. " R",
}

-- ── Markdown heading signs (render-markdown) ──────────────────────────────────
M.headings = {
  icon(0xf0ca1) .. " ",  -- H1
  icon(0xf0ca3) .. " ",  -- H2
  icon(0xf0ca5) .. " ",  -- H3
  icon(0xf0ca7) .. " ",  -- H4
  icon(0xf0ca9) .. " ",  -- H5
  icon(0xf0cab) .. " ",  -- H6
}

-- ── Progress / spinner ────────────────────────────────────────────────────────
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
M.status = {
  lsp_ok       = icon(0xf06a5) .. " ",
  lsp_off      = icon(0xf06a6) .. " ",
  format_on    = icon(0xf027f) .. " ",
  format_off   = icon(0xf027e) .. " ",
  focus        = icon(0xf0208) .. " ",
  readonly     = icon(0xf033e) .. " ",
  modified     = icon(0xf03eb) .. " ",
  saved        = icon(0xf0193) .. " ",
  session      = icon(0xf10ac) .. " ",
  auto_cd      = icon(0xf126d) .. " ",
  large_file   = icon(0xf021a) .. " ",
  word_count   = icon(0xf022d) .. " ",
}

-- ── User overrides ────────────────────────────────────────────────────────────
--
--   vim.g.icon_overrides = {
--     kinds = { Function = "ƒ" },
--     git   = { added = "+ " },
--   }
--
local OVERRIDABLE_GROUPS = {
  "kinds", "diagnostics", "git", "dap", "explorer",
  "ast", "modes", "headings", "status",
}

-- Validated by M.validate() in addition to the overridable groups above.
local ALL_GROUPS = vim.list_extend(vim.deepcopy(OVERRIDABLE_GROUPS), { "progress" })

local function apply_overrides()
  local overrides = vim.g.icon_overrides
  if overrides == nil then return end
  if type(overrides) ~= "table" then
    vim.notify(
      string.format("[icons] vim.g.icon_overrides must be a table, got %s — ignored.", type(overrides)),
      vim.log.levels.WARN
    )
    return
  end

  for _, group in ipairs(OVERRIDABLE_GROUPS) do
    local patch = overrides[group]
    if patch ~= nil then
      if type(patch) ~= "table" then
        vim.notify(
          string.format(
            "[icons] vim.g.icon_overrides.%s must be a table, got %s — ignored.",
            group, type(patch)
          ),
          vim.log.levels.WARN
        )
      else
        M[group] = vim.tbl_deep_extend("force", M[group], patch)
      end
    end
  end

  for key in pairs(overrides) do
    if not vim.tbl_contains(OVERRIDABLE_GROUPS, key) then
      vim.notify(
        string.format(
          "[icons] vim.g.icon_overrides.%s does not match any overridable icon group.\n"
          .. "Valid groups: %s%s",
          key, table.concat(OVERRIDABLE_GROUPS, ", "),
          key == "progress" and "\n(progress is intentionally excluded — see comment in icons.lua)" or ""
        ),
        vim.log.levels.WARN
      )
    end
  end
end

-- ── M.validate ────────────────────────────────────────────────────────────────
--
local ASCII_OK = {
  ["explorer.default_file"] = true,  -- see M.explorer above
  ["ast.statement"] = true,          -- see M.ast above; clangd_extensions' own default
}

local function has_glyph_byte(s)
  for i = 1, #s do
    if s:byte(i) >= 0x80 then return true end
  end
  return false
end

local function validate_group(tbl, issues, path)
  for key, val in pairs(tbl) do
    local subpath = path .. "." .. tostring(key)
    if type(val) == "string" then
      if val == "" then
        table.insert(issues, string.format("  %s is an empty string", subpath))
      elseif not has_glyph_byte(val) and not ASCII_OK[(subpath:gsub("^M%.", ""))] then
        table.insert(issues, string.format(
          "  %s = %q has no glyph byte (ASCII only) — likely lost its Nerd Font icon, not just padding",
          subpath, val
        ))
      end
    elseif type(val) == "table" then
      validate_group(val, issues, subpath)
    elseif type(val) ~= "function" then
      table.insert(issues, string.format("  %s is %s (expected string)", subpath, type(val)))
    end
  end
end

function M.validate()
  local issues = {}

  for _, group in ipairs(ALL_GROUPS) do
    local tbl = M[group]
    if type(tbl) ~= "table" then
      table.insert(issues, string.format("  M.%s is not a table (got %s)", group, type(tbl)))
    else
      validate_group(tbl, issues, "M." .. group)
    end
  end

  if #issues > 0 then
    vim.notify(
      "[icons] M.validate(): problems found after applying overrides:\n"
      .. table.concat(issues, "\n"),
      vim.log.levels.WARN
    )
  else
    vim.notify("[icons] M.validate(): all icon groups OK.", vim.log.levels.DEBUG)
  end
end

-- ── M.check_nerd_font ─────────────────────────────────────────────────────────
--
---@return boolean|nil ok  true/false if determinable, nil if not (e.g. terminal Neovim)
---@return string? font    the detected guifont value, when ok is not nil
function M.check_nerd_font()
  local is_gui = vim.g.neovide == true
    or vim.g.goneovim == true
    or vim.fn.has("gui_running") == 1
  if not is_gui then return nil end

  local font = vim.o.guifont or ""
  if font == "" then return nil end

  return font:lower():find("nerd", 1, true) ~= nil, font
end

apply_overrides()
vim.schedule(M.validate)

return M
