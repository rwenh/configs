-- lua/plugins/specs/lang/fortran.lua - Fortran development

return {
  -- Treesitter parser
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "fortran" })
      end
    end,
  },

  -- Conform: fprettify formatter
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = {
      formatters_by_ft = {
        fortran = { "fprettify" },
      },
      formatters = {
        fprettify = {
          command = "fprettify",
          args    = { "--indent", "2", "--stdout", "-" },
          stdin   = true,
        },
      },
    },
  },

  -- Build integration via toggleterm
  {
    "akinsho/toggleterm.nvim",
    optional = true,
    keys = {
      {
        "<leader>fob",
        function()
          local file = vim.fn.expand("%:p")
          local exe  = vim.fn.expand("%:p:r")
          local Terminal = require("toggleterm.terminal").Terminal
          Terminal:new({
            cmd           = string.format("gfortran -Wall -o %s %s && %s", exe, file, exe),
            direction     = "float",
            close_on_exit = false,
          }):toggle()
        end,
        desc = "Fortran Build & Run",
        ft   = "fortran",
      },
      {
        "<leader>foc",
        function()
          local file = vim.fn.expand("%:p")
          local Terminal = require("toggleterm.terminal").Terminal
          Terminal:new({
            cmd           = string.format("gfortran -Wall -fsyntax-only %s", file),
            direction     = "float",
            close_on_exit = false,
          }):toggle()
        end,
        desc = "Fortran Check Syntax",
        ft   = "fortran",
      },
      {
        "<leader>fom",
        function()
          local Terminal = require("toggleterm.terminal").Terminal
          Terminal:new({
            cmd           = "make",
            direction     = "float",
            close_on_exit = false,
          }):toggle()
        end,
        desc = "Fortran Make",
        ft   = "fortran",
      },
    },
  },

  -- Snippets for common Fortran constructs
  {
    "L3MON4D3/LuaSnip",
    optional = true,
    ft = "fortran",
    config = function()
      local ls = require("luasnip")
      local s, t, i = ls.snippet, ls.text_node, ls.insert_node

      ls.add_snippets("fortran", {
        s("program", {
          t("program "), i(1, "name"),
          t({ "", "  implicit none", "  " }), i(0),
          t({ "", "end program " }), i(1),
        }),
        s("subroutine", {
          t("subroutine "), i(1, "name"), t("("), i(2), t(")"),
          t({ "", "  implicit none", "  " }), i(0),
          t({ "", "end subroutine " }), i(1),
        }),
        s("do", {
          t("do "), i(1, "i"), t(" = "), i(2, "1"), t(", "), i(3, "n"),
          t({ "", "  " }), i(0),
          t({ "", "end do" }),
        }),
      })
    end,
  },
}
