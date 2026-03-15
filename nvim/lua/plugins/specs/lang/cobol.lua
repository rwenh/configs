-- lua/plugins/specs/lang/cobol.lua - COBOL development
-- LSP (cobol_ls) is configured in lsp.lua.

return {
  -- Compile & Run
  {
    "akinsho/toggleterm.nvim",
    optional = true,
    keys = {
      {
        "<leader>cob",
        function()
          local file = vim.fn.expand("%:p")
          local exe  = vim.fn.expand("%:p:r")
          local Terminal = require("toggleterm.terminal").Terminal
          Terminal:new({
            cmd           = string.format("cobc -x -o %s %s && %s", exe, file, exe),
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
          local file = vim.fn.expand("%:p")
          local Terminal = require("toggleterm.terminal").Terminal
          Terminal:new({
            cmd           = string.format("cobc -fsyntax-only %s", file),
            direction     = "float",
            close_on_exit = false,
          }):toggle()
        end,
        desc = "COBOL Syntax Check",
        ft   = "cobol",
      },
    },
  },

  -- COBOL snippets
  {
    "L3MON4D3/LuaSnip",
    optional = true,
    ft = "cobol",
    config = function()
      local ls = require("luasnip")
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
