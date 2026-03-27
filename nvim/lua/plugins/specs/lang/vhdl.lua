-- lua/plugins/specs/lang/vhdl.lua - VHDL hardware description language
-- LSP (vhdl_ls) is configured in lsp.lua.

return {
  -- Formatter
  {
    "stevearc/conform.nvim",
    optional = true,
    -- FIX #7: Use opts as a function to safely deep-merge nested tables.
    -- Plain table opts causes shallow merge — other specs' top-level
    -- "formatters" entries can be lost when this spec merges last.
    -- NOTE #8: vsg is pip-installed (pip install vsg), not Mason-managed.
    -- Run: pip install vsg  — it will not appear in :MasonInstallAll.
    opts = function(_, opts)
      opts.formatters_by_ft = opts.formatters_by_ft or {}
      opts.formatters_by_ft.vhdl = { "vsg" }
      opts.formatters = opts.formatters or {}
      opts.formatters.vsg = {
        command = "vsg",
        args    = { "--stdin", "--output", "syntastic" },
        stdin   = true,
      }
    end,
  },

  -- Treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "vhdl" })
      end
    end,
  },

  -- GHDL build/sim integration
  {
    "akinsho/toggleterm.nvim",
    optional = true,
    keys = {
      {
        "<leader>vha",
        function()
          local file = vim.fn.expand("%:p")
          local Terminal = require("toggleterm.terminal").Terminal
          Terminal:new({
            cmd           = string.format("ghdl -a %s", vim.fn.shellescape(file)),
            direction     = "float",
            close_on_exit = false,
          }):toggle()
        end,
        desc = "GHDL Analyze",
        ft   = "vhdl",
      },
      {
        "<leader>vhe",
        function()
          local entity = vim.fn.input("Entity name: ")
          if entity == "" then return end
          local Terminal = require("toggleterm.terminal").Terminal
          Terminal:new({
            -- FIX #10: shellescape entity name — raw user input breaks on
            -- names with spaces or special characters.
            cmd           = string.format("ghdl -e %s", vim.fn.shellescape(entity)),
            direction     = "float",
            close_on_exit = false,
          }):toggle()
        end,
        desc = "GHDL Elaborate",
        ft   = "vhdl",
      },
      {
        "<leader>vhr",
        function()
          local entity = vim.fn.input("Entity name: ")
          if entity == "" then return end
          local Terminal = require("toggleterm.terminal").Terminal
          Terminal:new({
            -- FIX #10: shellescape entity name in run command too.
            cmd = string.format(
              "ghdl -r %s --vcd=wave.vcd && gtkwave wave.vcd",
              vim.fn.shellescape(entity)
            ),
            direction     = "float",
            close_on_exit = false,
          }):toggle()
        end,
        desc = "GHDL Run & View",
        ft   = "vhdl",
      },
      {
        "<leader>vhc",
        function()
          local file = vim.fn.expand("%:p")
          local Terminal = require("toggleterm.terminal").Terminal
          Terminal:new({
            cmd           = string.format("ghdl -s %s", vim.fn.shellescape(file)),
            direction     = "float",
            close_on_exit = false,
          }):toggle()
        end,
        desc = "GHDL Syntax Check",
        ft   = "vhdl",
      },
    },
  },

  -- Snippets
  {
    "L3MON4D3/LuaSnip",
    optional = true,
    ft = "vhdl",
    config = function()
      local ls = require("luasnip")
      -- FIX #9: Removed unused `d` (dynamic_node) — all dynamic text uses
      -- `f` (function_node); `d` was imported but never referenced.
      local s, t, i, f = ls.snippet, ls.text_node, ls.insert_node, ls.function_node

      ls.add_snippets("vhdl", {
        s("entity", {
          t("entity "), i(1, "entity_name"), t(" is"),
          t({ "", "  port (" }),
          t({ "", "    " }), i(2, "signal_name"), t(" : "), i(3, "in"), t(" "), i(4, "std_logic"),
          t({ "", "  );" }),
          t({ "", "end entity " }), f(function(args) return args[1] end, { 1 }), t(";"),
        }),
        s("arch", {
          t("architecture "), i(1, "rtl"), t(" of "), i(2, "entity_name"), t(" is"),
          t({ "", "begin" }),
          t({ "", "  " }), i(0),
          t({ "", "end architecture " }), f(function(args) return args[1] end, { 1 }), t(";"),
        }),
        s("process", {
          t("process("), i(1, "clk"), t(")"),
          t({ "", "begin" }),
          t({ "", "  if rising_edge(" }), f(function(args) return args[1] end, { 1 }), t(") then"),
          t({ "", "    " }), i(0),
          t({ "", "  end if;" }),
          t({ "", "end process;" }),
        }),
        s("std", {
          t({ "library ieee;", "use ieee.std_logic_1164.all;", "use ieee.numeric_std.all;", "" }),
        }),
        s("tb", {
          t("entity "), i(1, "tb_name"), t(" is"),
          t({ "", "end entity " }), f(function(args) return args[1] end, { 1 }), t(";"),
          t({ "", "", "architecture sim of " }), f(function(args) return args[1] end, { 1 }), t(" is"),
          t({ "", "begin" }),
          t({ "", "  uut: entity work." }), i(2, "dut_name"),
          t({ "", "    port map (" }),
          t({ "", "      " }), i(0),
          t({ "", "    );" }),
          t({ "", "end architecture sim;" }),
        }),
      })
    end,
  },
}
