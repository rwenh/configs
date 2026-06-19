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

-- ── VHDL inline-comment stripper ─────────────────────────────────────────────
--
---@param  line string  one line of VHDL source
---@return string       the line with the comment and everything after it removed
local function strip_vhdl_comment(line)
  local idx = line:find("%-%-", 1, true)
  if idx then return line:sub(1, idx - 1) end
  return line
end

-- ── Port section parser ────────────────────────────────────────────────────────
---@param  lines string[]
---@return string|nil  entity name
---@return table       list of { name, dir, typ } tables
local function parse_entity_ports(lines)
  local entity_name = nil
  for _, line in ipairs(lines) do
    entity_name = line:match("^%s*entity%s+(%w+)%s+is")
    if entity_name then break end
  end
  if not entity_name then return nil, {} end

  local port_parts = {}
  local collecting = false
  local depth      = 0

  for _, line in ipairs(lines) do
    local safe_line = strip_vhdl_comment(line)

    if not collecting then
      if safe_line:find("port%s*%(") then
        collecting = true
        local after_port = safe_line:match("port%s*%((.*)$") or ""
        table.insert(port_parts, after_port)
        depth = 1
        for c in after_port:gmatch(".") do
          if c == "(" then depth = depth + 1
          elseif c == ")" then
            depth = depth - 1
            if depth == 0 then collecting = false; break end
          end
        end
      end
    else
      table.insert(port_parts, safe_line)
      for c in safe_line:gmatch(".") do
        if c == "(" then depth = depth + 1
        elseif c == ")" then
          depth = depth - 1
          if depth == 0 then collecting = false; break end
        end
      end
    end
  end

  local port_text = table.concat(port_parts, " ")
  port_text = port_text:gsub("%s*%)%s*;?%s*$", "")
  port_text = port_text:gsub("%s+", " ")

  local ports = {}
  for decl in (port_text .. ";"):gmatch("(.-)%s*;") do
    decl = vim.trim(decl)
    if decl ~= "" then
      local sig, dir, typ = decl:match("^(%w+)%s*:%s*(%w+)%s+(.+)$")
      if sig and dir and typ then
        typ = vim.trim(typ)
        sig = sig:gsub("^signal%s+", "")
        table.insert(ports, { name = sig, dir = dir:lower(), typ = typ })
      end
    end
  end

  return entity_name, ports
end

return {
  -- ── Conform: vsg formatter ─────────────────────────────────────────────────
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters     = opts.formatters or {}
      opts.formatters.vsg = {
        command = "vsg",
        args    = { "--output", "-", "--stdin" },
        stdin   = true,
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

          local entity_name, ports = parse_entity_ports(lines)

          if not entity_name then
            vim.notify("[vhdl] No entity declaration found", vim.log.levels.WARN)
            return
          end

          local tb_name = "tb_" .. entity_name
          local tb_file = vim.fn.fnamemodify(file, ":h") .. "/" .. tb_name .. ".vhd"

          local tb_lines = {
            "library ieee;",
            "use ieee.std_logic_1164.all;",
            "use ieee.numeric_std.all;",
            "",
            "entity " .. tb_name .. " is",
            "end entity " .. tb_name .. ";",
            "",
            "architecture sim of " .. tb_name .. " is",
          }

          for _, p in ipairs(ports) do
            table.insert(tb_lines, "  signal " .. p.name .. " : " .. p.typ .. ";")
          end

          table.insert(tb_lines, "begin")
          table.insert(tb_lines, "  uut: entity work." .. entity_name)
          table.insert(tb_lines, "    port map (")
          for i, p in ipairs(ports) do
            local comma = (i < #ports) and "," or ""
            table.insert(tb_lines, "      " .. p.name .. " => " .. p.name .. comma)
          end
          table.insert(tb_lines, "    );")
          table.insert(tb_lines, "")
          table.insert(tb_lines, "  stim: process")
          table.insert(tb_lines, "  begin")
          table.insert(tb_lines, "    -- TODO: add stimulus")
          table.insert(tb_lines, "    wait;")
          table.insert(tb_lines, "  end process stim;")
          table.insert(tb_lines, "end architecture sim;")

          local ok_w = pcall(vim.fn.writefile, tb_lines, tb_file)
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

  -- ── LuaSnip snippets ────────────────────────────────────────────────────────
  {
    "L3MON4D3/LuaSnip", optional = true, ft = "vhdl",
    config = function()
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
            t({ "library ieee;", "use ieee.std_logic_1164.all;",
                "use ieee.numeric_std.all;", "" }),
          }),
        }
      end)
    end,
  },
}
