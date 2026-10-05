-- lua/plugins/specs/lang/vhdl.lua — VHDL hardware description language
--

local shared = require("plugins.specs.lang.shared")

-- ── vhdl_ls config file detection ─────────────────────────────────────────────
vim.api.nvim_create_autocmd("FileType", {
  pattern  = "vhdl",
  once     = true,
  group    = vim.api.nvim_create_augroup("VhdlLsConfigCheck", { clear = true }),
  callback = function()
    local ok_path, path = pcall(require, "core.util.path")
    local root = (ok_path and path.find_root()) or vim.fn.getcwd()
    if root and vim.fn.filereadable(root .. "/vhdl_ls.toml") ~= 1 then
      vim.notify(
        "[vhdl] No vhdl_ls.toml found at project root.\n"
        .. "Library analysis will be limited. Create vhdl_ls.toml — see:\n"
        .. "https://github.com/VHDL-LS/rust_hdl#configuration",
        vim.log.levels.DEBUG
      )
    end
  end,
  desc = "Check for vhdl_ls.toml on first VHDL buffer open",
})

-- ── Snippets (queued; flushed once by completion.lua's LuaSnip config) ──
require("core.util.snippets").register("vhdl", function(s, t, i, _, ref)
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
      t({ "library ieee;", "use ieee.std_logic_1164.all;",
          "use ieee.numeric_std.all;", "" }),
    }),
  }
end)

return {
  -- ── Conform: vsg formatter ─────────────────────────────────────────────────
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters     = opts.formatters or {}
      -- vsg: `--output` is not a flag and `--stdin --fix` crashes (it tries to os.stat("stdin")).
      -- The working mode is in-place `--fix -f FILE`; conform runs it on a temp copy and reads it back.
      opts.formatters.vsg = {
        command = "vsg",
        args    = { "--fix", "-f", "$FILENAME" },
        stdin   = false,
        exit_codes = { 0, 1 },   -- 1 = "unfixable violations remain", not a crash
        condition = function()
          if vim.g.disable_vsg_format then return false end
          if vim.fn.executable("vsg") ~= 1 then
            vim.notify(
              "[vhdl] vsg not found — format-on-save disabled.\nInstall: pip install vsg",
              vim.log.levels.DEBUG
            )
            return false
          end
          return true
        end,
      }
    end,
  },

  shared.treesitter({ "vhdl" }),

  -- ── GHDL + testbench keymaps ───────────────────────────────────────────────
  {
    "akinsho/toggleterm.nvim",
    keys = {
      { "<leader>vha",
        function()
          local exec = require("core.util.exec")
          if not exec.require_bin("ghdl", "sudo zypper in ghdl") then return end
          require("core.util.term").float(
            "ghdl -a " .. vim.fn.shellescape(vim.fn.expand("%:p"))
          )
        end,
        desc = "GHDL Analyze current file", ft = "vhdl" },

      { "<leader>vhe",
        function()
          local exec = require("core.util.exec")
          if not exec.require_bin("ghdl", "sudo zypper in ghdl") then return end
          local entity = vim.fn.input("Entity name to elaborate: ")
          if entity == "" then return end
          require("core.util.term").float("ghdl -e " .. vim.fn.shellescape(entity))
        end,
        desc = "GHDL Elaborate (prompt entity)", ft = "vhdl" },

      { "<leader>vhr",
        function()
          local exec = require("core.util.exec")
          if not exec.require_bin("ghdl", "sudo zypper in ghdl") then return end

          local file   = vim.fn.expand("%:p")
          local entity = vim.fn.input("Entity name to simulate: ")
          if entity == "" then return end

          local vcd = vim.fn.getcwd() .. "/" .. entity .. "_wave.vcd"

          -- Chain: analyze current file → elaborate entity → run + VCD output
          local cmd = string.format(
            "ghdl -a %s && ghdl -e %s && ghdl -r %s --vcd=%s",
            vim.fn.shellescape(file),
            vim.fn.shellescape(entity),
            vim.fn.shellescape(entity),
            vim.fn.shellescape(vcd)
          )

          if vim.fn.executable("gtkwave") == 1 then
            cmd = cmd .. " && gtkwave " .. vim.fn.shellescape(vcd)
          else
            cmd = cmd
              .. string.format(" && echo 'VCD written to %s (gtkwave not found)'", vcd)
          end

          require("core.util.term").float(cmd)
        end,
        desc = "GHDL Analyze + Elaborate + Run + View (gtkwave)", ft = "vhdl" },

      { "<leader>vhc",
        function()
          local exec = require("core.util.exec")
          if not exec.require_bin("ghdl", "sudo zypper in ghdl") then return end
          require("core.util.term").float(
            "ghdl -s " .. vim.fn.shellescape(vim.fn.expand("%:p"))
          )
        end,
        desc = "GHDL Syntax Check", ft = "vhdl" },

      -- ── Testbench generator ───────────────────────────────────────────────
      {
        "<leader>vht",
        function()
          local file = vim.fn.expand("%:p")
          local ok_r, lines = pcall(vim.fn.readfile, file)
          if not ok_r then
            vim.notify("[vhdl] Cannot read current file", vim.log.levels.ERROR)
            return
          end

          local vhdl = require("core.util.vhdl")
          local entity_name, ports, generics = vhdl.parse_entity(lines)
          if not entity_name then
            vim.notify("[vhdl] No entity declaration found", vim.log.levels.WARN)
            return
          end

          local tb_file = vim.fn.fnamemodify(file, ":h") .. "/tb_" .. entity_name .. ".vhd"
          local ok_w = pcall(vim.fn.writefile, vhdl.testbench(entity_name, ports, generics), tb_file)
          if ok_w then
            vim.notify("[vhdl] Testbench created: " .. tb_file, vim.log.levels.INFO)
            vim.cmd("edit " .. vim.fn.fnameescape(tb_file))
          else
            vim.notify("[vhdl] Failed to write testbench file", vim.log.levels.ERROR)
          end
        end,
        desc = "VHDL Generate testbench skeleton", ft = "vhdl" },
    },
  },
}
