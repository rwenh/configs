-- lua/plugins/specs/lang/cobol.lua — COBOL development (GnuCOBOL, cobc)
--

local exec = require("core.util.exec")
local path = require("core.util.path")

local function term() return require("core.util.term") end

local function notify(msg, level)
  vim.notify("[cobol] " .. msg, level or vim.log.levels.INFO)
end

local DIALECT_STD = {
  default = false, gnucobol = false,
  cobol85 = "cobol85", cobol2002 = "cobol2002", cobol2014 = "cobol2014", xopen = "xopen",
  ibm = "ibm", mvs = "mvs", mf = "mf", acu = "acu", acucobol = "acu",
}

local COPYBOOK_DIRS = { "copy", "copybook", "copybooks", "COPY", "COPYBOOK", "lib", "include", "cpy" }

---@return boolean
local function is_file(p)
  return vim.fn.filereadable(p) == 1
end

---@return string[]
local function read_lines(p)
  local ok, lines = pcall(vim.fn.readfile, p)
  return ok and lines or {}
end

---@return string  the accepted dialect names, sorted, for messages
local function dialect_names()
  local names = {}
  for k in pairs(DIALECT_STD) do names[#names + 1] = k end
  table.sort(names)
  return table.concat(names, ", ")
end

---@param file string  absolute path of a source file
---@return string
local function project_root_for(file)
  return path.find_root(vim.fn.fnamemodify(file, ":h")) or vim.fn.getcwd()
end

---@return boolean
local function free_form()
  return vim.g.cobol_format == "free"
end

---@param root string
---@return boolean
local function has_jcl(root)
  if is_file(root .. "/JCL") then return true end
  return #vim.fn.glob(root .. "/*.jcl", false, true) + #vim.fn.glob(root .. "/*.JCL", false, true) > 0
end

---@param root string
---@return string dialect  a key of DIALECT_STD
---@return string source   "marker", "jcl" or "default"
local function detect_dialect(root)
  local marker = root .. "/.cobol-dialect"
  if is_file(marker) then
    local lines = read_lines(marker)
    local raw = lines[1] and vim.trim(lines[1]):lower() or ""
    if DIALECT_STD[raw] ~= nil then return raw, "marker" end
    notify(("%s contains '%s'; expected one of: %s. Using the default dialect."):format(
      marker, raw, dialect_names()), vim.log.levels.WARN)
    return "default", "default"
  end
  if has_jcl(root) then return "ibm", "jcl" end
  return "default", "default"
end

---@param root string
---@return string[] dirs
---@return string[] missing  entries as written in .cobol-copypath
local function copybook_dirs(root)
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
    if vim.fn.isdirectory(root .. "/" .. name) == 1 then add(name) end
  end
  if is_file(root .. "/.cobol-copypath") then
    for _, line in ipairs(read_lines(root .. "/.cobol-copypath")) do
      local entry = vim.trim((line:gsub("#.*$", "")))
      if entry ~= "" then add(entry) end
    end
  end
  return dirs, missing
end

---@param root string
---@return string flags
---@return string dialect
---@return string source
local function compile_flags(root)
  local format = vim.g.cobol_format
  if format ~= nil and format ~= "fixed" and format ~= "free" then
    notify(("vim.g.cobol_format is '%s'; expected 'fixed' or 'free'. Using fixed form."):format(
      tostring(format)), vim.log.levels.WARN)
  end

  local dialect, source = detect_dialect(root)
  local parts = {}
  if DIALECT_STD[dialect] then table.insert(parts, "-std=" .. DIALECT_STD[dialect]) end
  if free_form() then table.insert(parts, "-free") end

  local dirs, missing = copybook_dirs(root)
  if #missing > 0 then
    notify(("%s lists director%s that do not exist: %s"):format(
      root .. "/.cobol-copypath", #missing == 1 and "y" or "ies", table.concat(missing, ", ")),
      vim.log.levels.WARN)
  end
  for _, d in ipairs(dirs) do table.insert(parts, "-I " .. vim.fn.shellescape(d)) end

  return table.concat(parts, " "), dialect, source
end

---@return string  a unique path under the cache directory for a compiled test program
local function scratch_exe()
  local dir = vim.fn.stdpath("cache") .. "/cobol-run"
  vim.fn.mkdir(dir, "p")
  return dir .. "/" .. vim.fn.fnamemodify(vim.fn.tempname(), ":t")
end

-- ── Keymaps ───────────────────────────────────────────────────────────────────
local function current_file()
  local file = vim.fn.expand("%:p")
  if file == "" then notify("this buffer has no file on disk", vim.log.levels.WARN) end
  return file
end

local function compile_and_run()
  local file = current_file()
  if file == "" or not exec.require_bin("cobc", "sudo zypper in gnucobol") then return end
  local flags = compile_flags(project_root_for(file))
  local exe = scratch_exe()
  local q = vim.fn.shellescape
  term().float(string.format(
    "if cobc -x %s -o %s %s; then %s; EC=$?; else EC=$?; fi; rm -f %s; exit $EC",
    flags, q(exe), q(file), q(exe), q(exe)))
end

local function syntax_check()
  local file = current_file()
  if file == "" or not exec.require_bin("cobc", "sudo zypper in gnucobol") then return end
  local flags = compile_flags(project_root_for(file))
  -- -fsyntax-only writes no object or executable, so nothing is left in the project.
  term().float(("cobc -Wall -fsyntax-only %s %s"):format(flags, vim.fn.shellescape(file)))
end

local function show_dialect()
  local file = vim.fn.expand("%:p")
  local root = project_root_for(file ~= "" and file or vim.fn.getcwd())
  local dialect, source = detect_dialect(root)
  local why = ({ marker = ".cobol-dialect", jcl = "JCL files in the project", default = "default" })[source]
  notify(("dialect: %s (%s), %s form"):format(dialect, why, free_form() and "free" or "fixed"))
end

-- ── Snippets (queued; flushed once by completion.lua's LuaSnip config) ──────────
require("core.util.snippets").register("cobol", function(s, t, i, _, _ref)
  local a = free_form() and "    " or "       "   -- Area A
  local b = a .. "    "                             -- Area B
  local c = b .. "    "
  return {
    s("skeleton", {
      t({ a .. "IDENTIFICATION DIVISION.", a .. "PROGRAM-ID. " }),
      i(1, "PROGRAM-NAME"), t("."),
      t({ "", a .. "ENVIRONMENT DIVISION.", "", a .. "DATA DIVISION.", a .. "WORKING-STORAGE SECTION.", a .. "01  " }),
      i(2, "WS-VAR"), t("  PIC "), i(3, "X(10)"), t("."),
      t({ "", "", a .. "PROCEDURE DIVISION.", a .. "MAIN-PARA.", b }),
      i(0), t({ "", b .. "STOP RUN." }),
    }),
    s("if", {
      t(b .. "IF "), i(1, "CONDITION"),
      t({ "", c }), i(2, "CONTINUE"),
      t({ "", b .. "END-IF" }),
    }),
    s("perform", { t(b .. "PERFORM "), i(1, "PARA-NAME"), t(" UNTIL "), i(0, "CONDITION") }),
    s("display", { t(b .. 'DISPLAY "'), i(1, "message"), t('"') }),
    s("copy",    { t(b .. "COPY "), i(1, "COPYBOOK-NAME"), t(".") }),
  }
end)

return {
  {
    "akinsho/toggleterm.nvim",
    keys = {
      { "<leader>cob", compile_and_run, desc = "COBOL Compile & Run", ft = "cobol" },
      { "<leader>coc", syntax_check,    desc = "COBOL Syntax Check",  ft = "cobol" },
      { "<leader>cod", show_dialect,    desc = "COBOL Show Dialect",  ft = "cobol" },
    },
  },
}
