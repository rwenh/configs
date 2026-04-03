-- lua/plugins/specs/lang/cobol.lua - COBOL development

return {
  {
    "akinsho/toggleterm.nvim",
    optional = true,
    keys = {
      {
        "<leader>cob",
        function()
          local ok, term = pcall(require, "toggleterm.terminal")
          if not ok then
            vim.notify("toggleterm not available", vim.log.levels.ERROR)
            return
          end

          local file = vim.fn.expand("%:p")
          local exe  = vim.fn.expand("%:p:r")

          term.Terminal:new({
            cmd = string.format("cobc -x -o %s %s && %s",
              vim.fn.shellescape(exe),
              vim.fn.shellescape(file),
              vim.fn.shellescape(exe)),
            direction     = "float",
            close_on_exit = false,
          }):toggle()
        end,
        desc = "COBOL Compile & Run",
        ft   = "cobol",
      },
      {
        "<leader>coc",
        function()
          local ok, term = pcall(require, "toggleterm.terminal")
          if not ok then
            vim.notify("toggleterm not available", vim.log.levels.ERROR)
            return
          end

          local file = vim.fn.expand("%:p")
          term.Terminal:new({
            cmd = string.format("cobc -fsyntax-only %s", vim.fn.shellescape(file)),
            direction     = "float",
            close_on_exit = false,
          }):toggle()
        end,
        desc = "COBOL Syntax Check",
        ft   = "cobol",
      },
    },
  },

  {
    "L3MON4D3/LuaSnip",
    optional = true,
    ft = "cobol",
    config = function()
      local ok, ls = pcall(require, "luasnip")
      if not ok then return end

      local s, t, i = ls.snippet, ls.text_node, ls.insert_node

      ls.add_snippets("cobol", {
        s("skeleton", {
          t({
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. ",
          }), i(1, "PROGRAM-NAME"), t("."),
          t({
            "",
            "       ENVIRONMENT DIVISION.",
            "",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  ",
          }), i(2, "WS-VAR"), t("  PIC "), i(3, "X(10)"), t("."),
          t({
            "",
            "",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           ",
          }), i(0),
          t({
            "",
            "           STOP RUN.",
          }),
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
        s("display", {
          t('           DISPLAY "'), i(1, "message"), t('"'),
        }),
      })
    end,
  },
}
