-- lua/plugins/specs/lang/fortran.lua — Fortran development
--

local shared = require("plugins.specs.lang.shared")

-- ── Fortran filetype registration ──────────────────────────────────────────
--
pcall(function()
  vim.filetype.add({
    extension = {
      f90  = "fortran", f95  = "fortran", f03  = "fortran",
      f08  = "fortran", f18  = "fortran",
      F90  = "fortran", F95  = "fortran", F03  = "fortran",
      -- FIX: was 'for_', now 'for' (standard fixed-form Fortran extension)
      ["for"] = "fortran",
      fpp  = "fortran",
    },
  })
end)

-- ── fprettify config detection ─────────────────────────────────────────────

local _fprettify_args_cache = {}

local function fprettify_args()
  local ok_path, path_util = pcall(require, "core.util.path")
  local root = (ok_path and path_util.find_root()) or vim.fn.getcwd()

  -- Cache per project root to avoid repeated filesystem reads on every format.
  if _fprettify_args_cache[root] then return _fprettify_args_cache[root] end

  local config_candidates = {
    root .. "/.fprettify.toml",
    root .. "/.fprettify.rc",
    root .. "/setup.cfg",
  }

  for _, f in ipairs(config_candidates) do
    if vim.fn.filereadable(f) == 1 then
      if f:find("setup.cfg", 1, true) then
        local lines = vim.fn.readfile(f)
        local has_section = false
        for _, line in ipairs(lines) do
          if line:match("^%[fprettify%]") then has_section = true; break end
        end
        if not has_section then goto continue end
      end
      local result = { "--stdout", "-" }
      _fprettify_args_cache[root] = result
      return result
    end
    ::continue::
  end

  local result = { "--indent", "2", "--stdout", "-" }
  _fprettify_args_cache[root] = result
  return result
end

-- Invalidate the cache when the working directory changes.
vim.api.nvim_create_autocmd("DirChanged", {
  group    = vim.api.nvim_create_augroup("FortranFprettifyCache", { clear = true }),
  callback = function() _fprettify_args_cache = {} end,
})

return {
  -- ── Conform: fprettify custom config ──────────────────────────────────────
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters           = opts.formatters or {}
      opts.formatters.fprettify = {
        command = "fprettify",
        args    = function() return fprettify_args() end,
        stdin   = true,
        condition = function()
          if vim.fn.executable("fprettify") ~= 1 then
            vim.notify(
              "[fortran] fprettify not found — format-on-save disabled.\n"
              .. "Install: pip install fprettify",
              vim.log.levels.DEBUG
            )
            return false
          end
          return true
        end,
      }
    end,
  },

  shared.treesitter({ "fortran" }),

  -- ── Build keymaps ──────────────────────────────────────────────────────────
  {
    "akinsho/toggleterm.nvim",
    keys = (function()
      local function build_and_run(file)
        local exec = require("core.util.exec")
        if not exec.require_bin("gfortran", "sudo zypper in gcc-fortran") then return end
        local exe = vim.fn.tempname()
        require("core.util.term").float(string.format(
          "gfortran -Wall -o %s %s && %s; rm -f %s",
          vim.fn.shellescape(exe),
          vim.fn.shellescape(file),
          vim.fn.shellescape(exe),
          vim.fn.shellescape(exe)
        ))
      end

      return {
        {
          "<leader>ftb",
          function() build_and_run(vim.fn.expand("%:p")) end,
          desc = "Fortran Build & Run",
          ft   = "fortran",
        },
        {
          "<leader>ftc",
          function()
            local exec = require("core.util.exec")
            if not exec.require_bin("gfortran", "sudo zypper in gcc-fortran") then return end
            require("core.util.term").float(
              "gfortran -Wall -fsyntax-only "
              .. vim.fn.shellescape(vim.fn.expand("%:p"))
            )
          end,
          desc = "Fortran Check Syntax",
          ft   = "fortran",
        },
        {
          "<leader>ftm",
          function() require("core.util.term").float_at_root("make") end,
          desc = "Fortran Make",
          ft   = "fortran",
        },
        {
          "<leader>ftf",
          function()
            local exec = require("core.util.exec")
            if not exec.require_bin("fprettify", "pip install fprettify") then return end
            local args = fprettify_args()
            local cmd  = "fprettify " .. table.concat(args, " ")
              .. " " .. vim.fn.shellescape(vim.fn.expand("%:p"))
            require("core.util.term").float(cmd)
          end,
          desc = "Fortran Format (fprettify)",
          ft   = "fortran",
        },
      }
    end)(),
  },

  -- ── LuaSnip snippets ───────────────────────────────────────────────────────
  {
    "L3MON4D3/LuaSnip",
    optional = true,
    ft       = "fortran",
    config   = function()
      require("core.util.snippets").load("fortran", function(s, t, i, _, ref)
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
    end,
  },
}
