-- lua/plugins/specs/lang/vhdl.lua — VHDL hardware description language
--
-- LSP:    vhdl_ls (rust_hdl) via lsp.lua optional servers
-- Format: vsg via conform (this file)
-- DAP:    none
-- Test:   none (GHDL simulation via keymaps below)
--
-- vhdl_ls must be installed via: cargo install vhdl_ls
-- OR: :MasonInstall rust_hdl
--

return {
  -- ── Conform: vsg ───────────────────────────────────────────────────────────
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters_by_ft      = opts.formatters_by_ft or {}
      opts.formatters_by_ft.vhdl = { "vsg" }
      opts.formatters             = opts.formatters or {}
      opts.formatters.vsg         = {
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

  -- ── Treesitter ──────────────────────────────────────────────────────────────
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "vhdl" })
      end
    end,
  },

  -- ── GHDL keymaps ────────────────────────────────────────────────────────────
  {
    "akinsho/toggleterm.nvim",
    optional = true,
    keys = {
      {
        "<leader>vha",
        function()
          if vim.fn.executable("ghdl") ~= 1 then
            vim.notify("[vhdl] ghdl not found", vim.log.levels.ERROR)
            return
          end
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
          if vim.fn.executable("ghdl") ~= 1 then
            vim.notify("[vhdl] ghdl not found", vim.log.levels.ERROR)
            return
          end
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
          if vim.fn.executable("ghdl") ~= 1 then
            vim.notify("[vhdl] ghdl not found", vim.log.levels.ERROR)
            return
          end
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
          if vim.fn.executable("ghdl") ~= 1 then
            vim.notify("[vhdl] ghdl not found", vim.log.levels.ERROR)
            return
          end
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
    ft = "vhdl",
    config = function()
      local ok, ls = pcall(require, "luasnip")
      if not ok then return end
      local s, t, i, f = ls.snippet, ls.text_node, ls.insert_node, ls.function_node

      local function ref(n, default)
        return f(function(args)
          return (args[n] and args[n][1] ~= "") and args[n][1] or (default or "")
        end, { n })
      end

      pcall(function()
        ls.add_snippets("vhdl", {
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
        })
      end)
    end,
  },
}
