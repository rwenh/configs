-- lua/plugins/specs/lang/vhdl.lua - VHDL hardware description language
-- Note: LSP (vhdl_ls) is configured in lsp.lua, no need to duplicate here

return {
  -- VHDL syntax and utilities
  {
    "saadparwaiz1/cmp_luasnip",
    optional = true,
    dependencies = "L3MON4D3/LuaSnip",
  },

  -- VHDL formatter
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = {
      formatters_by_ft = {
        vhdl = { "vhdl-style-guide" },
      },
    },
  },

  -- GHDL integration (simulator)
  {
    "akinsho/toggleterm.nvim",
    optional = true,
    keys = {
      {
        "<leader>va",
        function()
          local file = vim.fn.expand("%:p")
          local Terminal = require("toggleterm.terminal").Terminal
          local ghdl_analyze = Terminal:new({
            cmd = string.format("ghdl -a %s", file),
            direction = "float",
            close_on_exit = false,
          })
          ghdl_analyze:toggle()
        end,
        desc = "GHDL Analyze",
        ft = "vhdl",
      },
      {
        "<leader>ve",
        function()
          local entity = vim.fn.input("Entity name: ")
          if entity ~= "" then
            local Terminal = require("toggleterm.terminal").Terminal
            local ghdl_elab = Terminal:new({
              cmd = string.format("ghdl -e %s", entity),
              direction = "float",
              close_on_exit = false,
            })
            ghdl_elab:toggle()
          end
        end,
        desc = "GHDL Elaborate",
        ft = "vhdl",
      },
      {
        "<leader>vr",
        function()
          local entity = vim.fn.input("Entity name: ")
          if entity ~= "" then
            local Terminal = require("toggleterm.terminal").Terminal
            local ghdl_run = Terminal:new({
              cmd = string.format("ghdl -r %s --vcd=wave.vcd && gtkwave wave.vcd", entity),
              direction = "float",
              close_on_exit = false,
            })
            ghdl_run:toggle()
          end
        end,
        desc = "GHDL Run & View",
        ft = "vhdl",
      },
      {
        "<leader>vs",
        function()
          local file = vim.fn.expand("%:p")
          local Terminal = require("toggleterm.terminal").Terminal
          local ghdl_syntax = Terminal:new({
            cmd = string.format("ghdl -s %s", file),
            direction = "float",
            close_on_exit = false,
          })
          ghdl_syntax:toggle()
        end,
        desc = "GHDL Syntax Check",
        ft = "vhdl",
      },
    },
  },

  -- Snippets for common VHDL constructs
  {
    "L3MON4D3/LuaSnip",
    optional = true,
    ft = "vhdl",
    config = function()
      local ls = require("luasnip")
      local s = ls.snippet
      local t = ls.text_node
      local i = ls.insert_node

      ls.add_snippets("vhdl", {
        s("entity", {
          t("entity "), i(1, "entity_name"), t(" is"),
          t({"", "  port ("}),
          t({"", "    "}), i(2, "signal_name"), t(" : "), i(3, "in"), t(" "), i(4, "std_logic"),
          t({"", "  );"}),
          t({"", "end entity "}), i(1), t(";"),
        }),
        s("arch", {
          t("architecture "), i(1, "rtl"), t(" of "), i(2, "entity_name"), t(" is"),
          t({"", "begin"}),
          t({"", "  "}), i(0),
          t({"", "end architecture "}), i(1), t(";"),
        }),
        s("process", {
          t("process("), i(1, "clk"), t(")"),
          t({"", "begin"}),
          t({"", "  if rising_edge("}), i(1), t(") then"),
          t({"", "    "}), i(0),
          t({"", "  end if;"}),
          t({"", "end process;"}),
        }),
      })
    end,
  },

  -- File type settings
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      -- VHDL treesitter parser
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "vhdl" })
      end
    end,
  },
}
