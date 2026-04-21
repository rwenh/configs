-- lua/plugins/specs/lang/vhdl.lua - VHDL hardware description language
--
-- FIX (v2.2.3): vsg stdin workaround — file-based conform formatter.
--
-- OPT (v2.3.13):
--   • Build keymaps use core.util.term.float() — 4 × boilerplate removed.

return {
  -- ── Conform: vsg ─────────────────────────────────────────────────────
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters_by_ft = opts.formatters_by_ft or {}
      opts.formatters_by_ft.vhdl = { "vsg" }
      opts.formatters = opts.formatters or {}
      opts.formatters.vsg = {
        command   = "vsg",
        args      = { "--output", "-", "$FILENAME" },
        stdin     = false,
        condition = function()
          if vim.g.disable_vsg_format then return false end
          return vim.fn.executable("vsg") == 1
        end,
      }
    end,
  },

  -- ── Treesitter ────────────────────────────────────────────────────────
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "vhdl" })
      end
    end,
  },

  -- ── GHDL keymaps ─────────────────────────────────────────────────────
  {
    "akinsho/toggleterm.nvim",
    optional = true,
    keys = {
      {
        "<leader>vha",
        function()
          require("core.util.term").float(
            "ghdl -a " .. vim.fn.shellescape(vim.fn.expand("%:p"))
          )
        end,
        desc = "GHDL Analyze",
        ft   = "vhdl",
      },
      {
        "<leader>vhe",
        function()
          local entity = vim.fn.input("Entity name: ")
          if entity == "" then return end
          require("core.util.term").float("ghdl -e " .. vim.fn.shellescape(entity))
        end,
        desc = "GHDL Elaborate",
        ft   = "vhdl",
      },
      {
        "<leader>vhr",
        function()
          local entity = vim.fn.input("Entity name: ")
          if entity == "" then return end
          require("core.util.term").float(string.format(
            "ghdl -r %s --vcd=wave.vcd && gtkwave wave.vcd",
            vim.fn.shellescape(entity)
          ))
        end,
        desc = "GHDL Run & View",
        ft   = "vhdl",
      },
      {
        "<leader>vhc",
        function()
          require("core.util.term").float(
            "ghdl -s " .. vim.fn.shellescape(vim.fn.expand("%:p"))
          )
        end,
        desc = "GHDL Syntax Check",
        ft   = "vhdl",
      },
    },
  },

  -- ── LuaSnip snippets ──────────────────────────────────────────────────
  {
    "L3MON4D3/LuaSnip",
    optional = true,
    ft = "vhdl",
    config = function()
      local ok, ls = pcall(require, "luasnip")
      if not ok then return end

      local s, t, i, f = ls.snippet, ls.text_node, ls.insert_node, ls.function_node

      pcall(function()
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
      end)
    end,
  },
}
