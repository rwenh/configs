-- lua/plugins/specs/lang/cobol.lua — COBOL development
--

local shared = require("plugins.specs.lang.shared")

-- ── Dialect detection ──────────────────────────────────────────────────────────
local function detect_dialect()
  local ok_path, path_util = pcall(require, "core.util.path")
  local root = (ok_path and path_util.find_root()) or vim.fn.getcwd()
  if not root or root == "" then return "gnucobol" end

  local marker = root .. "/.cobol-dialect"
  if vim.fn.filereadable(marker) == 1 then
    local lines = vim.fn.readfile(marker)
    local d     = lines[1] and vim.trim(lines[1]):lower() or ""
    local valid = { gnucobol=true, ibm=true, mf=true, acucobol=true }
    if valid[d] then return d end
  end

  if vim.fn.filereadable(root .. "/JCL") == 1
  or vim.fn.glob(root .. "/*.jcl") ~= ""
  or vim.fn.glob(root .. "/*.JCL") ~= "" then
    return "ibm"
  end

  return "gnucobol"
end

-- ── Copybook path resolution ───────────────────────────────────────────────────
local _copybook_cache = {}

local function copybook_include_flags()
  local ok_path, path_util = pcall(require, "core.util.path")
  local root = (ok_path and path_util.find_root()) or vim.fn.getcwd()
  if not root or root == "" then return "" end
  if _copybook_cache[root] then return _copybook_cache[root] end

  local candidates = { "copy","copybook","copybooks","COPY","COPYBOOK","lib","include","cpy" }
  local flags = {}
  for _, name in ipairs(candidates) do
    local dir = root .. "/" .. name
    if vim.fn.isdirectory(dir) == 1 then
      table.insert(flags, "-I " .. vim.fn.shellescape(dir))
    end
  end

  local cpfile = root .. "/.cobol-copypath"
  if vim.fn.filereadable(cpfile) == 1 then
    for _, line in ipairs(vim.fn.readfile(cpfile)) do
      local trimmed = vim.trim(line)
      if trimmed ~= "" and not trimmed:match("^#") then
        table.insert(flags, "-I " .. vim.fn.shellescape(trimmed))
      end
    end
  end

  local result = table.concat(flags, " ")
  _copybook_cache[root] = result
  return result
end

vim.api.nvim_create_autocmd("DirChanged", {
  group    = vim.api.nvim_create_augroup("CobolCopybookCacheInvalidate", { clear = true }),
  callback = function() _copybook_cache = {} end,
})

-- ── Safe temp directory ───────────────────────────────────────────────────────
--
local function cobol_exe_path()
  local dir = vim.fn.stdpath("cache") .. "/cobol-run"
  if vim.fn.isdirectory(dir) ~= 1 then
    vim.fn.mkdir(dir, "p")
  end
  -- Use the basename from tempname() to keep uniqueness.
  local tmp = vim.fn.fnamemodify(vim.fn.tempname(), ":t")
  return dir .. "/" .. tmp
end

return {
  {
    "akinsho/toggleterm.nvim",
    keys = (function()
      local function compile_and_run(file)
        if vim.fn.executable("cobc") ~= 1 then
          vim.notify("[cobol] cobc not found — install gnucobol", vim.log.levels.ERROR)
          return
        end
        local dialect      = detect_dialect()
        local includes     = copybook_include_flags()
        local exe          = cobol_exe_path()
        local dialect_flag = (dialect ~= "gnucobol")
          and ("-std=" .. dialect .. " ") or ""
        require("core.util.term").float(string.format(
          "cobc -x %s%s -o %s %s && %s; rm -f %s",
          dialect_flag, includes,
          vim.fn.shellescape(exe),
          vim.fn.shellescape(file),
          vim.fn.shellescape(exe),
          vim.fn.shellescape(exe)
        ))
      end

      return {
        {
          "<leader>cob",
          function() compile_and_run(vim.fn.expand("%:p")) end,
          desc = "COBOL Compile & Run",
          ft   = "cobol",
        },
        {
          "<leader>coc",
          function()
            if vim.fn.executable("cobc") ~= 1 then
              vim.notify("[cobol] cobc not found", vim.log.levels.ERROR); return
            end
            local includes = copybook_include_flags()
            local tmp_obj  = cobol_exe_path() .. ".o"
            require("core.util.term").float(string.format(
              "cobc -Wall -c %s -o %s %s; EC=$?; rm -f %s; exit $EC",
              includes,
              vim.fn.shellescape(tmp_obj),
              vim.fn.shellescape(vim.fn.expand("%:p")),
              vim.fn.shellescape(tmp_obj)
            ))
          end,
          desc = "COBOL Syntax Check",
          ft   = "cobol",
        },
        {
          "<leader>cod",
          function()
            local dialect = detect_dialect()
            vim.notify("[cobol] Detected dialect: " .. dialect, vim.log.levels.INFO)
          end,
          desc = "COBOL Show Dialect",
          ft   = "cobol",
        },
      }
    end)(),
  },

  {
    "L3MON4D3/LuaSnip",
    optional = true,
    ft       = "cobol",
    config   = function()
      require("core.util.snippets").load("cobol", function(s, t, i, _, _ref)
        return {
          s("skeleton", {
            t({ "       IDENTIFICATION DIVISION.", "       PROGRAM-ID. " }),
            i(1, "PROGRAM-NAME"), t("."),
            t({ "", "       ENVIRONMENT DIVISION.", "",
                "       DATA DIVISION.", "       WORKING-STORAGE SECTION.", "       01  " }),
            i(2, "WS-VAR"), t("  PIC "), i(3, "X(10)"), t("."),
            t({ "", "", "       PROCEDURE DIVISION.", "       MAIN-PARA.", "           " }),
            i(0), t({ "", "           STOP RUN." }),
          }),
          s("if", {
            t("           IF "), i(1, "CONDITION"),
            t({ "", "               " }), i(2, "CONTINUE"),
            t({ "", "           END-IF" }),
          }),
          s("perform", {
            t("           PERFORM "), i(1, "PARA-NAME"),
            t(" UNTIL "), i(0, "CONDITION"),
          }),
          s("display", { t('           DISPLAY "'), i(1, "message"), t('"') }),
          s("copy",    { t("           COPY "), i(1, "COPYBOOK-NAME"), t(".") }),
        }
      end)
    end,
  },

  shared.treesitter({ "cobol" }),
}
