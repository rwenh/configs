-- lua/plugins/specs/lang/fortran.lua - Fortran development
--
-- OPT (v2.3.13):
--   • Build keymaps use core.util.term.float() — 3 × boilerplate removed.

return {
  -- ── Treesitter ────────────────────────────────────────────────────────
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "fortran" })
      end
    end,
  },

  -- ── Conform: fprettify ────────────────────────────────────────────────
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters_by_ft = opts.formatters_by_ft or {}
      opts.formatters_by_ft.fortran = { "fprettify" }
      opts.formatters = opts.formatters or {}
      opts.formatters.fprettify = {
        command = "fprettify",
        args    = { "--indent", "2", "--stdout", "-" },
        stdin   = true,
      }
    end,
  },

  -- ── Build keymaps ─────────────────────────────────────────────────────
  {
    "akinsho/toggleterm.nvim",
    optional = true,
    keys = {
      {
        "<leader>ftb",
        function()
          local file = vim.fn.expand("%:p")
          local exe  = vim.fn.expand("%:p:r")
          require("core.util.term").float(string.format(
            "gfortran -Wall -o %s %s && %s",
            vim.fn.shellescape(exe), vim.fn.shellescape(file), vim.fn.shellescape(exe)
          ))
        end,
        desc = "Fortran Build & Run",
        ft   = "fortran",
      },
      {
        "<leader>ftc",
        function()
          require("core.util.term").float(
            "gfortran -Wall -fsyntax-only " .. vim.fn.shellescape(vim.fn.expand("%:p"))
          )
        end,
        desc = "Fortran Check Syntax",
        ft   = "fortran",
      },
      {
        "<leader>ftm",
        function() require("core.util.term").float("make") end,
        desc = "Fortran Make",
        ft   = "fortran",
      },
    },
  },

  -- ── LuaSnip snippets ──────────────────────────────────────────────────
  {
    "L3MON4D3/LuaSnip",
    optional = true,
    ft = "fortran",
    config = function()
      local ok, ls = pcall(require, "luasnip")
      if not ok then return end

      local s, t, i, f = ls.snippet, ls.text_node, ls.insert_node, ls.function_node

      pcall(function()
        ls.add_snippets("fortran", {
          s("program", {
            t("program "), i(1, "name"),
            t({ "", "  implicit none", "  " }), i(2),
            t({ "", "end program " }), f(function(args) return args[1][1] end, { 1 }),
            i(0),
          }),
          s("subroutine", {
            t("subroutine "), i(1, "name"), t("("), i(2), t(")"),
            t({ "", "  implicit none", "  " }), i(3),
            t({ "", "end subroutine " }), f(function(args) return args[1][1] end, { 1 }),
            i(0),
          }),
          s("do", {
            t("do "), i(1, "i"), t(" = "), i(2, "1"), t(", "), i(3, "n"),
            t({ "", "  " }), i(0),
            t({ "", "end do" }),
          }),
        })
      end)
    end,
  },
}
