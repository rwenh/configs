-- lua/plugins/specs/lang/vhdl.lua - VHDL hardware description language
-- LSP (vhdl_ls) is configured in lsp.lua.
-- NOTE: <leader>vs was conflicting with Python venv selector — fixed to <leader>vhd* prefix.

return {
  -- Formatter
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = {
      formatters_by_ft = {
        vhdl = { "vsg" },  -- VHDL Style Guide (vsg) — more maintained than vhdl-style-guide
      },
      formatters = {
        vsg = {
          command = "vsg",
          args    = { "--stdin", "--output", "syntastic" },
          stdin   = true,
        },
      },
    },
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

  -- GHDL build/sim integration (fixed keymap prefix)
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
            cmd           = string.format("ghdl -e %s", entity),
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
            cmd           = string.format("ghdl -r %s --vcd=wave.vcd && gtkwave wave.vcd", entity),
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

  -- Snippets (proper separate snippet definitions)
  {
    "L3MON4D3/LuaSnip",
    optional = true,
    ft = "vhdl",
    config = function()
      local ls = require("luasnip")
      local s, t, i, d = ls.snippet, ls.text_node, ls.insert_node, ls.dynamic_node
      local f = ls.function_node

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
