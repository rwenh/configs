-- lua/plugins/specs/lang/shared.lua — shared constants, spec helpers and per-project linter gating
--

local M = {}

-- ── Filetype lists ────────────────────────────────────────────────────────────

M.JS_FT    = { "javascript", "javascriptreact" }
M.TS_FT    = { "typescript", "typescriptreact" }
M.JS_TS_FT = { "javascript", "javascriptreact", "typescript", "typescriptreact" }

M.WEB_FT = {
  "html", "htmldjango", "jinja.html",
  "css", "scss", "less",
  "javascript", "javascriptreact",
  "typescript", "typescriptreact",
  "vue", "svelte",
}

M.TAILWIND_FT = {
  "html", "css",
  "javascript", "javascriptreact",
  "typescript", "typescriptreact",
  "vue", "svelte",
}

-- ── Spec helpers ──────────────────────────────────────────────────────────────

---@param parsers string[]
---@return table
function M.treesitter(parsers)
  return {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        local seen = {}
        for _, p in ipairs(opts.ensure_installed) do seen[p] = true end
        for _, p in ipairs(parsers) do
          if not seen[p] then
            table.insert(opts.ensure_installed, p)
            seen[p] = true
          end
        end
      end
    end,
  }
end

-- ── Build flags ───────────────────────────────────────────────────────────────

---@param flags string?
---@return string sanitized
---@return boolean changed  true if any characters were stripped
function M.sanitize_build_flags(flags)
  return require("core.util.exec").sanitize_build_flags(flags)
end

-- ── Project root ──────────────────────────────────────────────────────────────

---@param start string?
---@return string
function M.project_root(start)
  local root
  local ok, path = pcall(require, "core.util.path")
  if ok then
    local ok_find, found = pcall(path.find_root, start)
    if ok_find then root = found end
  end
  return root or vim.fn.getcwd()
end

-- ── Project configuration detection ───────────────────────────────────────────

local CONFIG_TTL = 30
local _config_cache = {}

---@class ProjectConfigSpec
---@field files   string[]?  config files that count, looked up at the project root
---@field pkg_key string?    package.json key that counts as configuration
---@field bin     string?    executable that must also be on PATH before the tool is wanted

---Does `root` configure the tool described by `spec`?
---@param root string
---@param spec ProjectConfigSpec
---@return boolean
function M.has_project_config(root, spec)
  local files = spec.files or {}
  local key = table.concat({ root, table.concat(files, ","), spec.pkg_key or "" }, "\0")
  local now = os.time()
  local cached = _config_cache[key]
  if cached and (now - cached.at) < CONFIG_TTL then return cached.hit end

  local hit = false
  for _, name in ipairs(files) do
    if vim.fn.filereadable(root .. "/" .. name) == 1 then
      hit = true
      break
    end
  end
  if not hit and spec.pkg_key then
    local pkg = root .. "/package.json"
    if vim.fn.filereadable(pkg) == 1 then
      local ok_read, lines = pcall(vim.fn.readfile, pkg)
      if ok_read then
        local ok_json, obj = pcall(vim.json.decode, table.concat(lines, "\n"))
        hit = ok_json and type(obj) == "table" and obj[spec.pkg_key] ~= nil
      end
    end
  end

  _config_cache[key] = { at = now, hit = hit }
  return hit
end

-- ── Per-project linter gating ─────────────────────────────────────────────────

---@param fts    string[]
---@param linter string   nvim-lint linter name
---@param spec   ProjectConfigSpec
---@return boolean wanted
function M.sync_linter(fts, linter, spec)
  local ok, lint = pcall(require, "lint")
  if not ok or type(lint) ~= "table" then return false end

  local wanted = M.has_project_config(M.project_root(), spec)
  if wanted and spec.bin and vim.fn.executable(spec.bin) ~= 1 then wanted = false end

  lint.linters_by_ft = lint.linters_by_ft or {}
  for _, ft in ipairs(fts) do
    local list = lint.linters_by_ft[ft] or {}
    local at
    for i, name in ipairs(list) do
      if name == linter then
        at = i
        break
      end
    end
    if wanted and not at then
      table.insert(list, linter)
    elseif not wanted and at then
      table.remove(list, at)
    end
    lint.linters_by_ft[ft] = list
  end
  return wanted
end

---@param fts    string[]
---@param linter string
---@param spec   ProjectConfigSpec
function M.gate_linter(fts, linter, spec)
  M.sync_linter(fts, linter, spec)
  vim.api.nvim_create_autocmd({ "BufEnter", "BufReadPost", "FileType", "DirChanged" }, {
    group    = vim.api.nvim_create_augroup("LinterGate_" .. linter, { clear = true }),
    callback = function() M.sync_linter(fts, linter, spec) end,
    desc     = "Keep " .. linter .. " in step with the project's configuration",
  })
end

-- ── Build helpers ─────────────────────────────────────────────────────────────

function M.run_make()
  local dir = require("core.util.path").find_up({ "Makefile", "makefile", "GNUmakefile" })
  if not dir then
    vim.notify("[make] no Makefile found above the current file", vim.log.levels.WARN)
    return
  end
  local term = require("core.util.term")
  term.float(term.cd_prefix(dir) .. "make")
end

---@param prefix string  notification tag, e.g. "c" or "cpp"
---@param extra  string[]?  additional candidate paths
function M.symlink_compile_commands(prefix, extra)
  local root = M.project_root()
  local dst = root .. "/compile_commands.json"
  if vim.fn.filereadable(dst) == 1 or vim.fn.isdirectory(dst) == 1 then return end

  local build_dir = vim.g.cmake_build_dir or "build"
  local candidates = {
    root .. "/" .. build_dir .. "/compile_commands.json",
    root .. "/build/Debug/compile_commands.json",
    root .. "/build/Release/compile_commands.json",
    root .. "/.build/compile_commands.json",
  }
  if type(extra) == "table" then vim.list_extend(candidates, extra) end

  for _, src in ipairs(candidates) do
    if vim.fn.filereadable(src) == 1 then
      local shown = vim.fn.fnamemodify(src, ":~:.")
      if vim.fn.executable("ln") ~= 1 then
        vim.notify(
          "[" .. prefix .. "] compile_commands.json found at " .. shown
          .. " but 'ln' is not on PATH — skipping symlink.",
          vim.log.levels.DEBUG
        )
        return
      end
      vim.fn.system({ "ln", "-sf", src, dst })
      if vim.v.shell_error == 0 and vim.fn.filereadable(dst) == 1 then
        vim.notify("[" .. prefix .. "] compile_commands.json linked from " .. shown, vim.log.levels.INFO)
      else
        vim.notify(
          "[" .. prefix .. "] found " .. shown .. " but could not link it into the project root"
          .. " — check write permissions.",
          vim.log.levels.WARN
        )
      end
      return
    end
  end
end

return M
