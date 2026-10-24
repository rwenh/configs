-- lua/plugins/specs/lang/fortran.lua — Fortran development
--

local shared = require("plugins.specs.lang.shared")
local exec   = require("core.util.exec")
local path   = require("core.util.path")

local function term() return require("core.util.term") end

local function notify(msg, level)
  vim.notify("[fortran] " .. msg, level or vim.log.levels.INFO)
end

local CONFIG_NAME    = ".fprettify.rc"
local DEFAULT_INDENT = 2   -- fprettify's own default is 3; the other languages in this config use 2

-- ── Filetypes ─────────────────────────────────────────────────────────────────
vim.filetype.add({
  extension = {
    f = "fortran", F = "fortran", f77 = "fortran", F77 = "fortran",
    f90 = "fortran", F90 = "fortran", f95 = "fortran", F95 = "fortran",
    f03 = "fortran", F03 = "fortran", f08 = "fortran", F08 = "fortran",
    f18 = "fortran", F18 = "fortran",
    ["for"] = "fortran", FOR = "fortran", fpp = "fortran",
  },
})

-- ── fprettify ─────────────────────────────────────────────────────────────────

---@param dir string
---@return boolean
local function config_applies(dir)
  if vim.fn.filereadable(vim.fn.expand("~/" .. CONFIG_NAME)) == 1 then return true end
  return path.find_up({ CONFIG_NAME }, dir) ~= nil
end

--- Arguments for in-place formatting of conform's temp copy. "$FILENAME" is conform's own token.
---@param file string  the file being formatted (the temp copy, which sits beside the buffer)
---@return string[]
local function fprettify_args(file)
  local args = {}
  if not config_applies(vim.fn.fnamemodify(file, ":h")) then
    args = { "--indent", tostring(DEFAULT_INDENT) }
  end
  table.insert(args, "$FILENAME")
  return args
end

-- ── Build and check ───────────────────────────────────────────────────────────
---@return string  cache directory for objects and modules (created on demand)
local function run_dir()
  local dir = vim.fn.stdpath("cache") .. "/fortran-run"
  vim.fn.mkdir(dir, "p")
  return dir
end

--- gfortran invocation with the user's build flags (sanitised), modules sent to run_dir().
---@param file string
---@param extra string  extra arguments, already shell-escaped
---@return string
local function gfortran(file, extra)
  local raw = vim.g.fortran_build_flags or "-Wall"
  local flags, changed = shared.sanitize_build_flags(raw)
  if changed then
    notify(("fortran_build_flags contained unsafe characters, stripped.\n  original : %s\n  sanitised: %s")
      :format(raw, flags), vim.log.levels.WARN)
  end
  return ("gfortran %s -J %s %s %s"):format(flags, vim.fn.shellescape(run_dir()), extra, vim.fn.shellescape(file))
end

local function current_file()
  local file = vim.fn.expand("%:p")
  if file == "" then notify("this buffer has no file on disk", vim.log.levels.WARN) end
  return file
end

local function build_and_run()
  local file = current_file()
  if file == "" or not exec.require_bin("gfortran", "sudo zypper in gcc-fortran") then return end
  local exe = run_dir() .. "/" .. vim.fn.fnamemodify(vim.fn.tempname(), ":t")
  local q = vim.fn.shellescape
  term().float(string.format(
    "if %s; then %s; EC=$?; else EC=$?; fi; rm -f %s; exit $EC",
    gfortran(file, "-o " .. q(exe)), q(exe), q(exe)))
end

local function syntax_check()
  local file = current_file()
  if file == "" or not exec.require_bin("gfortran", "sudo zypper in gcc-fortran") then return end
  term().float(gfortran(file, "-fsyntax-only"))
end

local function format_buffer()
  if not exec.require_bin("fprettify", "pip install fprettify") then return end
  require("core.util.format").run({})
end

-- ── Snippets (queued; flushed once by completion.lua's LuaSnip config) ──────────
require("core.util.snippets").register("fortran", function(s, t, i, _, ref)
  return {
    s("program", {
      t("program "), i(1, "name"),
      t({ "", "  implicit none", "  " }), i(2),
      t({ "", "end program " }), ref(1, "name"),
      i(0),
    }),
    s("subroutine", {
      t("subroutine "), i(1, "name"), t("("), i(2), t(")"),
      t({ "", "  implicit none", "  " }), i(3),
      t({ "", "end subroutine " }), ref(1, "name"),
      i(0),
    }),
    s("function", {
      i(1, "real"), t(" function "), i(2, "name"), t("("), i(3), t(")"),
      t({ "", "  implicit none", "  " }), i(0),
      t({ "", "end function " }), ref(2, "name"),
    }),
    s("do", {
      t("do "), i(1, "i"), t(" = "), i(2, "1"), t(", "), i(3, "n"),
      t({ "", "  " }), i(0),
      t({ "", "end do" }),
    }),
    s("module", {
      t("module "), i(1, "name"),
      t({ "", "  implicit none", "  " }), i(0),
      t({ "", "end module " }), ref(1, "name"),
    }),
    s("mpi_init", {
      t({
        "use mpi",
        "integer :: ierr, rank, nprocs",
        "call MPI_Init(ierr)",
        "call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)",
        "call MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)",
      }),
      i(0),
      t({ "", "call MPI_Finalize(ierr)" }),
    }),
  }
end)

return {
  -- ── Conform: fprettify ────────────────────────────────────────────────────
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters = opts.formatters or {}
      opts.formatters.fprettify = {
        command   = "fprettify",
        args      = function(_, ctx) return fprettify_args(ctx.filename) end,
        stdin     = false,
        condition = function() return vim.fn.executable("fprettify") == 1 end,
      }
    end,
  },

  shared.treesitter({ "fortran" }),

  -- ── Build and check keymaps ───────────────────────────────────────────────
  {
    "akinsho/toggleterm.nvim",
    keys = {
      { "<leader>ftb", build_and_run, desc = "Fortran Build & Run",      ft = "fortran" },
      { "<leader>ftc", syntax_check,  desc = "Fortran Check Syntax",     ft = "fortran" },
      { "<leader>ftm", function() shared.run_make() end, desc = "Fortran Make", ft = "fortran" },
      { "<leader>ftf", format_buffer, desc = "Fortran Format (fprettify)", ft = "fortran" },
    },
  },
}
