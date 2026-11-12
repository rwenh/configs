-- lua/core/util/cobol.lua — GnuCOBOL command construction
--
-- Used by core.util.runner (run and syntax check) and the COBOL keymaps. Reads the dialect from
-- .cobol-dialect (or infers it from JCL files), copybook directories from the usual names and
-- .cobol-copypath, and the form from vim.g.cobol_format. Functions return a command; none launch one.

local path = require("core.util.path")

local M = {}
local sq = vim.fn.shellescape

local DIALECT_STD = {
  default = false,
  gnucobol = false,
  cobol85 = "cobol85",
  cobol2002 = "cobol2002",
  cobol2014 = "cobol2014",
  xopen = "xopen",
  ibm = "ibm",
  mvs = "mvs",
  mf = "mf",
  acu = "acu",
  acucobol = "acu",
}

local COPYBOOK_DIRS = { "copy", "copybook", "copybooks", "COPY", "COPYBOOK", "lib", "include", "cpy" }

local function notify(msg, level)
  vim.notify("[cobol] " .. msg, level or vim.log.levels.WARN)
end

local function is_file(p)
  return vim.fn.filereadable(p) == 1
end

local function read_lines(p)
  local ok, lines = pcall(vim.fn.readfile, p)
  return ok and lines or {}
end

--- Project root for a file or directory. Falls back to its own directory.
---@param file_or_dir string
---@return string
function M.root_of(file_or_dir)
  local dir = vim.fn.isdirectory(file_or_dir) == 1 and file_or_dir or vim.fn.fnamemodify(file_or_dir, ":h")
  return path.find_root(dir) or dir
end

---@return string[] accepted dialect names, sorted
function M.dialects()
  local names = vim.tbl_keys(DIALECT_STD)
  table.sort(names)
  return names
end

local function has_jcl(root)
  if is_file(root .. "/JCL") then
    return true
  end
  return #vim.fn.glob(root .. "/*.jcl", false, true) + #vim.fn.glob(root .. "/*.JCL", false, true) > 0
end

---@return string dialect  key of DIALECT_STD
---@return string source   "marker", "jcl" or "default"
function M.detect_dialect(root)
  local marker = root .. "/.cobol-dialect"
  if is_file(marker) then
    local raw = vim.trim(read_lines(marker)[1] or ""):lower()
    if DIALECT_STD[raw] ~= nil then
      return raw, "marker"
    end
    notify(
      ("%s contains '%s'; expected one of: %s. Using the default dialect."):format(
        marker,
        raw,
        table.concat(M.dialects(), ", ")
      )
    )
    return "default", "default"
  end
  if has_jcl(root) then
    return "ibm", "jcl"
  end
  return "default", "default"
end

---@return string[] dirs     existing copybook directories, absolute
---@return string[] missing  .cobol-copypath entries that do not exist
function M.copybook_dirs(root)
  local dirs, missing, seen = {}, {}, {}
  local function add(entry)
    local abs = entry:sub(1, 1) == "/" and entry or (root .. "/" .. entry)
    abs = vim.fn.fnamemodify(abs, ":p"):gsub("/+$", "")
    if vim.fn.isdirectory(abs) ~= 1 then
      missing[#missing + 1] = entry
    elseif not seen[abs] then
      seen[abs] = true
      dirs[#dirs + 1] = abs
    end
  end
  for _, name in ipairs(COPYBOOK_DIRS) do
    if vim.fn.isdirectory(root .. "/" .. name) == 1 then
      add(name)
    end
  end
  for _, line in ipairs(read_lines(root .. "/.cobol-copypath")) do
    local entry = vim.trim((line:gsub("#.*$", "")))
    if entry ~= "" then
      add(entry)
    end
  end
  return dirs, missing
end

--- Flags shared by the run and check commands for `root`.
---@return string
function M.compile_flags(root)
  local format = vim.g.cobol_format
  if format ~= nil and format ~= "fixed" and format ~= "free" then
    notify(("vim.g.cobol_format is '%s'; expected 'fixed' or 'free'. Using fixed form."):format(tostring(format)))
  end
  local parts = {}
  local dialect = M.detect_dialect(root)
  if DIALECT_STD[dialect] then
    parts[#parts + 1] = "-std=" .. DIALECT_STD[dialect]
  end
  if format == "free" then
    parts[#parts + 1] = "-free"
  end
  local dirs, missing = M.copybook_dirs(root)
  if #missing > 0 then
    notify(
      ("%s lists director%s that do not exist: %s"):format(
        root .. "/.cobol-copypath",
        #missing == 1 and "y" or "ies",
        table.concat(missing, ", ")
      )
    )
  end
  for _, d in ipairs(dirs) do
    parts[#parts + 1] = "-I " .. sq(d)
  end
  return table.concat(parts, " ")
end

--- Compile and run `file`; the executable goes to the cache, never beside the source.
---@return string|nil cmd
---@return string|nil reason
function M.run_cmd(file)
  if vim.fn.executable("cobc") ~= 1 then
    return nil, "cobc not found\n  install: sudo zypper in gnucobol"
  end
  local dir = vim.fn.stdpath("cache") .. "/cobol-run"
  vim.fn.mkdir(dir, "p")
  local exe = dir .. "/" .. vim.fn.fnamemodify(file, ":t:r")
  return ("cobc -x %s -o %s %s && %s"):format(M.compile_flags(M.root_of(file)), sq(exe), sq(file), sq(exe))
end

--- Syntax check of `file`. Writes no object or executable.
---@return string|nil cmd
---@return string|nil reason
function M.syntax_cmd(file)
  if vim.fn.executable("cobc") ~= 1 then
    return nil, "cobc not found\n  install: sudo zypper in gnucobol"
  end
  return ("cobc -Wall -fsyntax-only %s %s"):format(M.compile_flags(M.root_of(file)), sq(file))
end

--- One-line summary of dialect and form for `root`.
---@return string
function M.describe(root)
  local dialect, source = M.detect_dialect(root)
  local why = ({ marker = ".cobol-dialect", jcl = "JCL files in the project", default = "default" })[source]
  return ("dialect: %s (%s), %s form"):format(dialect, why, vim.g.cobol_format == "free" and "free" or "fixed")
end

return M
