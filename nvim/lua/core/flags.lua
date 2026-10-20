-- lua/core/flags.lua — registry of every user-facing escape hatch (vim.g.*)
--
-- Set flags at the TOP of init.lua, before require("core.bootstrap").
-- Add a flag here when you add a `vim.g.<name>` read anywhere in the config.

local M = {}

---@class FlagSpec
---@field name    string
---@field type    "boolean"|"number"|"string"|"table"
---@field default string   human-readable default (docs only)
---@field effect  string
---@field group   string

M.groups = {
  "Core display & behaviour",
  "LSP & formatting",
  "DAP & testing",
  "Task runner & workflow",
  "Project root & paths",
  "UI extras",
  "Language-specific",
}

---@type FlagSpec[]
M.list = {
  -- ── Core display & behaviour ────────────────────────────────────────────────
  { group = "Core display & behaviour", name = "disable_highlight_overrides", type = "boolean", default = "false", effect = "Skip all highlight overrides" },
  { group = "Core display & behaviour", name = "disable_tint",               type = "boolean", default = "false", effect = "Disable tint.nvim inactive-window dimming" },
  { group = "Core display & behaviour", name = "disable_smear_cursor",       type = "boolean", default = "false", effect = "Disable smear-cursor (re-enables mini.animate's cursor animation)" },
  { group = "Core display & behaviour", name = "disable_mini_pairs",         type = "boolean", default = "false", effect = "Disable mini.pairs auto-close brackets" },
  { group = "Core display & behaviour", name = "disable_autoformat",         type = "boolean", default = "false", effect = "Disable format-on-save globally (`:ToggleAutoformat` flips it live)" },
  { group = "Core display & behaviour", name = "disable_treesitter_folds",   type = "boolean", default = "false", effect = "Use indent folding instead of treesitter" },
  { group = "Core display & behaviour", name = "large_file_bytes", type = "number", default = "512000", effect = "Size in bytes above which buffers drop treesitter, folds, syntax and LSP-heavy features (default 500 KB)" },
  { group = "Core display & behaviour", name = "disable_trim_whitespace",    type = "boolean", default = "false", effect = "Don't trim trailing whitespace on save (per buffer: `vim.b.disable_trim_whitespace`)" },
  { group = "Core display & behaviour", name = "disable_theme_autoswitch",   type = "boolean", default = "false", effect = "Don't flip dark/light at `day_start`/`day_end` (startup choice only)" },
  { group = "Core display & behaviour", name = "theme_option_patches",       type = "table",   default = "nil",   effect = "Per-theme option patches, e.g. `{ tokyonight = { background = \"dark\" } }`" },
  { group = "Core display & behaviour", name = "icon_overrides",             type = "table",   default = "nil",   effect = "Override icon groups, e.g. `{ kinds = { Function = \"ƒ\" } }`" },
  { group = "Core display & behaviour", name = "auto_cd_root",               type = "boolean", default = "false", effect = "Auto-cd to project root on `BufEnter`" },
  { group = "Core display & behaviour", name = "runner_autosave",            type = "boolean", default = "true",  effect = "Auto-save before running a file with the runner" },
  { group = "Core display & behaviour", name = "disable_project_configs",    type = "boolean", default = "false", effect = "Never load per-project Lua files (`.lspconfig.lua`, `.runner.lua`, `.nvim-dap.lua`, `.overseer.lua`); otherwise they load only after you `:trust` them" },
  { group = "Core display & behaviour", name = "spell_wordlist",             type = "string",  default = "nil",   effect = "Path to a personal spell file" },
  { group = "Core display & behaviour", name = "filetype_options",           type = "table",   default = "nil",   effect = "Per-filetype option overrides, e.g. `{ rust = { colorcolumn = \"100\" } }`" },

  -- ── LSP & formatting ────────────────────────────────────────────────────────
  { group = "LSP & formatting", name = "format_timeout_ms",        type = "number",  default = "3000", effect = "Global format timeout (ms)" },
  { group = "LSP & formatting", name = "format_timeout_by_ft",     type = "table",   default = "{}",   effect = "Per-filetype timeout, e.g. `{ python = 5000, cpp = 4000 }`" },
  { group = "LSP & formatting", name = "enable_pylint",            type = "boolean", default = "false", effect = "Opt-in pylint alongside ruff" },
  { group = "LSP & formatting", name = "lsp_on_attach_overrides",  type = "table",   default = "nil",  effect = "Per-server on_attach overrides, e.g. `{ lua_ls = function(c, b) end }`" },
  { group = "LSP & formatting", name = "completion_sources_by_ft", type = "table",   default = "nil",  effect = "Per-filetype blink.cmp sources, e.g. `{ sql = { \"lsp\", \"dadbod\", \"buffer\" } }` (SQL defaults to dadbod)" },
  { group = "LSP & formatting", name = "debugpy_python",           type = "string",  default = "nil",  effect = "Pin the Python interpreter for debugpy. It is probed for `import debugpy`; if the probe fails, auto-detection continues" },
  { group = "LSP & formatting", name = "ts_import_preference",     type = "string",  default = "\"non-relative\"", effect = "TypeScript import module-specifier preference" },

  -- ── DAP & testing ───────────────────────────────────────────────────────────
  { group = "DAP & testing", name = "dap_bp_autosave_ms", type = "number", default = "60000", effect = "DAP breakpoint autosave interval (ms); `0` disables" },
  { group = "DAP & testing", name = "neotest_concurrency", type = "number", default = "4",     effect = "Parallel test-suite concurrency (`<leader>'P`)" },

  -- ── Task runner & workflow ──────────────────────────────────────────────────
  { group = "Task runner & workflow", name = "mason_install_timeout_ms",      type = "number",  default = "120000", effect = "Per-install timeout for `:MasonInstallAll` (ms)" },
  { group = "Task runner & workflow", name = "mason_extras",                  type = "table",   default = "nil",    effect = "Extra Mason packages appended to the extras list" },
  { group = "Task runner & workflow", name = "workflow_template_debounce_ms", type = "number",  default = "300",    effect = "Overseer `.overseer.lua` load debounce on `BufEnter` (ms)" },
  { group = "Task runner & workflow", name = "overseer_auto_scroll",          type = "boolean", default = "true",   effect = "Auto-scroll Overseer task output" },
  { group = "Task runner & workflow", name = "term_max_runs",                 type = "number",  default = "3",      effect = "Finished one-shot run terminals kept before older ones are shut down" },
  { group = "Task runner & workflow", name = "terminal_tui_programs",         type = "table",   default = "nil",    effect = "Extra full-screen programs that keep their own `<Esc>` (lazygit, htop, yazi… are built in)" },

  -- ── Project root & paths ────────────────────────────────────────────────────
  { group = "Project root & paths", name = "path_max_walk_depth", type = "number",  default = "20",      effect = "Upward-walk limit for root detection" },
  { group = "Project root & paths", name = "path_cache_ttl",      type = "number",  default = "30",      effect = "Root-cache lifetime (seconds)" },
  { group = "Project root & paths", name = "path_debug",          type = "boolean", default = "false",   effect = "Log cwd fallbacks at DEBUG" },
  { group = "Project root & paths", name = "path_prefer_vcs",     type = "boolean", default = "false",   effect = "Prefer the `.git`/`.hg`/`.svn` root over a nearer `Makefile`/`package.json`" },
  { group = "Project root & paths", name = "path_ignore_dirs",    type = "table",   default = "nil",     effect = "Extra directory names (exact match) to skip during the root walk" },
  { group = "Project root & paths", name = "cmake_build_dir",     type = "string",  default = "\"build\"", effect = "CMake build directory name (C/C++/CTest)" },
  { group = "Project root & paths", name = "lazy_cache_path",     type = "string",  default = "nil",     effect = "Custom path for the offline lazy.nvim cache" },

  -- ── UI extras ───────────────────────────────────────────────────────────────
  { group = "UI extras", name = "symbol_usage_max_lines",    type = "number",  default = "2000",  effect = "Disable symbol-usage.nvim above this line count" },
  { group = "UI extras", name = "force_mini_animate_scroll", type = "boolean", default = "false", effect = "Force mini.animate scroll even when neoscroll is active" },
  { group = "UI extras", name = "octo_timeout_ms",           type = "number",  default = "10000", effect = "Octo.nvim request timeout (ms)" },

  -- ── Language-specific ───────────────────────────────────────────────────────
  { group = "Language-specific", name = "disable_vsg_format",       type = "boolean", default = "false",  effect = "Disable the vsg VHDL formatter" },
  { group = "Language-specific", name = "elixir_use_nextls",        type = "boolean", default = "false",  effect = "Use NextLS instead of elixir-ls (needs the `nextls` binary)" },
  { group = "Language-specific", name = "nextls_bin",               type = "string",  default = "\"nextls\"", effect = "Path to the NextLS binary" },
  { group = "Language-specific", name = "kotlin_spring_cache_ttl",  type = "number",  default = "300",    effect = "Spring Boot detection cache TTL (seconds; `0` = no cache)" },
  { group = "Language-specific", name = "jdtls_heap_mb", type = "number", default = "1024", effect = "Initial heap for the jdtls JVM (`-Xms`), in MB" },
  { group = "Language-specific", name = "python_venv_auto_refresh", type = "boolean", default = "true",  effect = "Auto-activate the cached venv for a project when a Python buffer opens (venv-selector `cached_venv_automatic_activation`); `false` turns it off" },
  { group = "Language-specific", name = "python_probe_timeout_ms", type = "number", default = "4000", effect = "Timeout for each interpreter probe (`import debugpy`), in ms" },
  { group = "Language-specific", name = "python_probe_ttl", type = "number", default = "120", effect = "Seconds a verified debugpy interpreter, or a miss, is cached per project root" },
  { group = "Language-specific", name = "python_docstring_style",   type = "string",  default = "\"google_docstrings\"", effect = "Neogen Python docstring convention" },
  { group = "Language-specific", name = "c_build_flags",            type = "string",  default = "\"-Wall -Wextra -g\"", effect = "GCC flags for `<leader>cb` (sanitised to `[A-Za-z0-9 -._=/+:,@]`)" },
  { group = "Language-specific", name = "cpp_build_flags",          type = "string",  default = "\"-Wall -std=c++17 -g\"", effect = "G++ flags for `<leader>ccx` and the runner" },
  { group = "Language-specific", name = "fortran_build_flags",      type = "string",  default = "\"-Wall\"", effect = "gfortran flags for `<leader>ftb`" },
  { group = "Language-specific", name = "cobol_format",             type = "string",  default = "\"fixed\"", effect = "`\"free\"` adds `-free` to cobc" },
  { group = "Language-specific", name = "rustaceanvim_features",    type = "table",   default = "nil",    effect = "Active Cargo features (toggle with `<leader>rf`; applied live)" },
  { group = "Language-specific", name = "rest_auth_tokens",         type = "table",   default = "nil",    effect = "Seed auth tokens for the REST client, `{ name = \"Bearer …\" }`" },
  { group = "Language-specific", name = "ts_disable",               type = "table",   default = "nil",    effect = "Treesitter parsers to disable, e.g. `{ \"vhdl\", \"fortran\" }`" },
  { group = "Language-specific", name = "ts_auto_install",          type = "boolean", default = "true",   effect = "Auto-install missing parsers when the tree-sitter CLI is present" },
}

---@type table<string, FlagSpec>
M.by_name = {}
for _, f in ipairs(M.list) do M.by_name[f.name] = f end

--- Flags whose current value has the wrong type (they would silently fall back to defaults).
---@return string[] issues
function M.type_issues()
  local issues = {}
  for _, f in ipairs(M.list) do
    local val = vim.g[f.name]
    if val ~= nil and type(val) ~= f.type then
      table.insert(issues, string.format("  vim.g.%-35s expected %-8s got %s", f.name, f.type, type(val)))
    end
  end
  return issues
end

--- Vim-script variables / plugin settings that the config reads but are not user escape hatches.
--- (The docs test uses this to tell "undocumented flag" from "plugin variable".)
M.not_flags = {
  mapleader = true, maplocalleader = true, nvim_ide_version = true,
  db = true, neovide = true, goneovim = true,
  auto_cd_root_internal = true,
}

return M
