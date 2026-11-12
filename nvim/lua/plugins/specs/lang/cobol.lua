-- lua/plugins/specs/lang/cobol.lua — COBOL keymaps and snippets (GnuCOBOL)
--
-- Build and check logic lives in core.util.cobol and core.util.runner. This file only binds keys.

local cobol = require("core.util.cobol")
local path = require("core.util.path")

local function runner()
  return require("core.util.runner")
end

local function free_form()
  return vim.g.cobol_format == "free"
end

-- ── Snippets (queued; flushed once by completion.lua's LuaSnip config) ────────
require("core.util.snippets").register("cobol", function(s, t, i, _, _ref)
  local a = free_form() and "    " or "       " -- Area A
  local b = a .. "    " -- Area B
  local c = b .. "    "
  return {
    s("skeleton", {
      t({ a .. "IDENTIFICATION DIVISION.", a .. "PROGRAM-ID. " }),
      i(1, "PROGRAM-NAME"),
      t("."),
      t({ "", a .. "ENVIRONMENT DIVISION.", "", a .. "DATA DIVISION.", a .. "WORKING-STORAGE SECTION.", a .. "01  " }),
      i(2, "WS-VAR"),
      t("  PIC "),
      i(3, "X(10)"),
      t("."),
      t({ "", "", a .. "PROCEDURE DIVISION.", a .. "MAIN-PARA.", b }),
      i(0),
      t({ "", b .. "STOP RUN." }),
    }),
    s("if", {
      t(b .. "IF "),
      i(1, "CONDITION"),
      t({ "", c }),
      i(2, "CONTINUE"),
      t({ "", b .. "END-IF" }),
    }),
    s("perform", { t(b .. "PERFORM "), i(1, "PARA-NAME"), t(" UNTIL "), i(0, "CONDITION") }),
    s("display", { t(b .. 'DISPLAY "'), i(1, "message"), t('"') }),
    s("copy", { t(b .. "COPY "), i(1, "COPYBOOK-NAME"), t(".") }),
  }
end)

local function show_dialect()
  vim.notify("[cobol] " .. cobol.describe(path.find_root() or vim.fn.getcwd()), vim.log.levels.INFO)
end

return {
  {
    "akinsho/toggleterm.nvim",
    keys = {
      {
        "<leader>cob",
        function()
          runner().run_file()
        end,
        desc = "COBOL Compile & Run",
        ft = "cobol",
      },
      {
        "<leader>coc",
        function()
          runner().syntax_check()
        end,
        desc = "COBOL Syntax Check",
        ft = "cobol",
      },
      { "<leader>cod", show_dialect, desc = "COBOL Show Dialect", ft = "cobol" },
    },
  },
}
