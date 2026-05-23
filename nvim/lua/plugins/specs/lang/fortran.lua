-- lua/plugins/specs/lang/fortran.lua — Fortran development
--

local shared = require("plugins.specs.lang.shared")

return {
  -- ── Conform: fprettify custom config ──────────────────────────────────────
  -- This spec only provides the custom formatter invocation arguments.

  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters           = opts.formatters or {}
      opts.formatters.fprettify = {
        command = "fprettify",
        args    = { "--indent", "2", "--stdout", "-" },
        stdin   = true,
      }
    end,
  },

  -- ── Treesitter ─────────────────────────────────────────────────────────────

  shared.treesitter({ "fortran" }),

  -- ── Build keymaps ──────────────────────────────────────────────────────────
  {
    "akinsho/toggleterm.nvim",
    keys = (function()
      local function build_and_run(file)
        local exec = require("core.util.exec")
        if not exec.require_bin("gfortran", "sudo zypper in gcc-fortran") then
          return
        end
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
            if not exec.require_bin("gfortran", "sudo zypper in gcc-fortran") then
              return
            end
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
        }
      end)
    end,
  },
}
