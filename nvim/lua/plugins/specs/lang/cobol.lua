-- lua/plugins/specs/lang/cobol.lua — COBOL development
--

local shared = require("plugins.specs.lang.shared")

return {
  -- ── Build keymaps ──────────────────────────────────────────────────────────
  {
    "akinsho/toggleterm.nvim",
    keys = (function()
      local function compile_and_run(file)
        if vim.fn.executable("cobc") ~= 1 then
          vim.notify("[cobol] cobc not found — install gnucobol", vim.log.levels.ERROR)
          return
        end
        local exe = vim.fn.tempname()
        require("core.util.term").float(string.format(
          "cobc -x -o %s %s && %s; rm -f %s",
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
              vim.notify("[cobol] cobc not found", vim.log.levels.ERROR)
              return
            end
            require("core.util.term").float(
              "cobc -fsyntax-only "
              .. vim.fn.shellescape(vim.fn.expand("%:p"))
            )
          end,
          desc = "COBOL Syntax Check",
          ft   = "cobol",
        },
      }
    end)(),
  },

  -- ── LuaSnip snippets ────────────────────────────────────────────────────────

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
            i(0),
            t({ "", "           STOP RUN." }),
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
        }
      end)
    end,
  },

  -- ── Treesitter ─────────────────────────────────────────────────────────────

  shared.treesitter({ "cobol" }),
}
