-- lua/plugins/specs/lang/vhdl.lua — VHDL hardware description language
-- vhdl_ls must be installed via: cargo install vhdl_ls
--

local shared = require("plugins.specs.lang.shared")

return {
  -- ── Conform: vsg custom config ─────────────────────────────────────────────
  -- This spec only provides the conditional invocation and output args.

  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters     = opts.formatters or {}
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

  shared.treesitter({ "vhdl" }),

  -- ── GHDL keymaps ────────────────────────────────────────────────────────────
  {
    "akinsho/toggleterm.nvim",
    keys = {
      {
        "<leader>vha",
        function()
          local exec = require("core.util.exec")
          if not exec.require_bin("ghdl", "sudo zypper in ghdl") then return end
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
          local exec = require("core.util.exec")
          if not exec.require_bin("ghdl", "sudo zypper in ghdl") then return end
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
          local exec = require("core.util.exec")
          if not exec.require_bin("ghdl", "sudo zypper in ghdl") then return end
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
          local exec = require("core.util.exec")
          if not exec.require_bin("ghdl", "sudo zypper in ghdl") then return end
          require("core.util.term").float(
            "ghdl -s " .. vim.fn.shellescape(vim.fn.expand("%:p"))
          )
        end,
        desc = "GHDL Syntax Check",
        ft   = "vhdl",
      },
    },
  },

  -- ── LuaSnip snippets ────────────────────────────────────────────────────────

  {
    "L3MON4D3/LuaSnip",
    optional = true,
    ft       = "vhdl",
    config   = function()
      require("core.util.snippets").load("vhdl", function(s, t, i, _, ref)
        return {
          s("entity", {
            t("entity "), i(1, "entity_name"), t(" is"),
            t({ "", "  port (" }),
            t({ "", "    " }), i(2, "signal_name"),
            t(" : "), i(3, "in"), t(" "), i(4, "std_logic"),
            t({ "", "  );", "end entity " }), ref(1, "entity_name"), t(";"),
          }),
          s("arch", {
            t("architecture "), i(1, "rtl"),
            t(" of "), i(2, "entity_name"), t(" is"),
            t({ "", "begin", "  " }), i(0),
            t({ "", "end architecture " }), ref(1, "rtl"), t(";"),
          }),
          s("process", {
            t("process("), i(1, "clk"), t(")"),
            t({ "", "begin", "  if rising_edge(" }),
            ref(1, "clk"), t(") then"),
            t({ "", "    " }), i(0),
            t({ "", "  end if;", "end process;" }),
          }),
          s("std", {
            t({
              "library ieee;",
              "use ieee.std_logic_1164.all;",
              "use ieee.numeric_std.all;",
              "",
            }),
          }),
          s("tb", {
            t("entity "), i(1, "tb_name"), t(" is"),
            t({ "", "end entity " }), ref(1, "tb_name"), t(";"),
            t({ "", "", "architecture sim of " }), ref(1, "tb_name"), t(" is"),
            t({ "", "begin", "  uut: entity work." }), i(2, "dut_name"),
            t({ "", "    port map (", "      " }), i(0),
            t({ "", "    );", "end architecture sim;" }),
          }),
        }
      end)
    end,
  },
}
