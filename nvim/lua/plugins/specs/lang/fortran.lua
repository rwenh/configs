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

  -- Build integration via toggleterm
  {
    "akinsho/toggleterm.nvim",
    optional = true,
    keys = {
      {
        "<leader>ftb",
        function()
          local ok, term = pcall(require, "toggleterm.terminal")
          if not ok then
            vim.notify("toggleterm not available", vim.log.levels.ERROR)
            return
          end

          local file = vim.fn.expand("%:p")
          local exe  = vim.fn.expand("%:p:r")

          term.Terminal:new({
            cmd           = string.format("gfortran -Wall -o %s %s && %s",
              vim.fn.shellescape(exe),
              vim.fn.shellescape(file),
              vim.fn.shellescape(exe)),
            direction     = "float",
            close_on_exit = false,
          }):toggle()
        end,
        desc = "Fortran Build & Run",
        ft   = "fortran",
      },
      {
        "<leader>ftc",
        function()
          local ok, term = pcall(require, "toggleterm.terminal")
          if not ok then
            vim.notify("toggleterm not available", vim.log.levels.ERROR)
            return
          end

          local file = vim.fn.expand("%:p")
          term.Terminal:new({
            cmd           = string.format("gfortran -Wall -fsyntax-only %s",
              vim.fn.shellescape(file)),
            direction     = "float",
            close_on_exit = false,
          }):toggle()
        end,
        desc = "Fortran Check Syntax",
        ft   = "fortran",
      },
      {
        "<leader>ftm",
        function()
          local ok, term = pcall(require, "toggleterm.terminal")
          if not ok then
            vim.notify("toggleterm not available", vim.log.levels.ERROR)
            return
          end

          term.Terminal:new({
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
