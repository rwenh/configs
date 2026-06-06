-- lua/plugins/specs/lang/vhdl.lua — VHDL hardware description language
--

local shared = require("plugins.specs.lang.shared")

-- ── vhdl_ls config file detection ─────────────────────────────────────────
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

-- ── Port section parser ────────────────────────────────────────────────────
--
-- FIX: the original parser matched one port per line using a single pattern:
--
--   local sig, dir, typ = line:match("(%w+)%s*:%s*(%w+)%s+([%w_]+)")
--
-- This silently skipped any port whose declaration was split across lines,
-- e.g.:
--
--   data_bus : in
--     std_logic_vector(7 downto 0);
--
-- The new parser uses paren-depth tracking to extract the entire port section
-- as a single string, then splits on semicolons to get each declaration.
-- This correctly handles:
--   - Multi-line port declarations
--   - Types with generics/ranges: std_logic_vector(N-1 downto 0)
--   - Last port (no trailing semicolon before closing paren)
--
---@param  lines string[]  all lines of the source file
---@return string|nil      entity name
---@return table           list of { name, dir, typ } tables
local function parse_entity_ports(lines)
  -- ── Pass 1: extract entity name ──────────────────────────────────────────
  local entity_name = nil
  for _, line in ipairs(lines) do
    entity_name = line:match("^%s*entity%s+(%w+)%s+is")
    if entity_name then break end
  end
  if not entity_name then return nil, {} end

  -- ── Pass 2: collect port section using paren-depth tracking ──────────────
  -- We look for `port (` and accumulate text until the matching `)` closes.
  local port_parts = {}
  local collecting = false
  local depth      = 0

  for _, line in ipairs(lines) do
    if not collecting then
      if line:find("port%s*%(") then
        collecting = true
        -- Trim everything before 'port' so we start counting parens cleanly.
        local after_port = line:match("port%s*%((.*)$") or ""
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
      table.insert(port_parts, line)
      for c in line:gmatch(".") do
        if c == "(" then depth = depth + 1
        elseif c == ")" then
          depth = depth - 1
          if depth == 0 then collecting = false; break end
        end
      end
    end
  end

  -- ── Pass 3: normalise whitespace and split on semicolons ─────────────────
  local port_text = table.concat(port_parts, " ")
  -- Strip trailing ); which closes the port list itself.
  port_text = port_text:gsub("%s*%)%s*;?%s*$", "")
  -- Normalise runs of whitespace to single spaces.
  port_text = port_text:gsub("%s+", " ")

  local ports = {}
  -- Split on semicolons to get individual declarations.
  -- Append a trailing semicolon so the last entry (which may lack one) is caught.
  for decl in (port_text .. ";"):gmatch("(.-)%s*;") do
    decl = vim.trim(decl)
    if decl ~= "" then
      -- Match: identifier : direction type_expression
      local sig, dir, typ = decl:match("^(%w+)%s*:%s*(%w+)%s+(.+)$")
      if sig and dir and typ then
        typ = vim.trim(typ)
        -- Skip "signal" keyword prefix used by some VHDL styles.
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
        desc = "GHDL Analyze", ft = "vhdl" },

      { "<leader>vhe",
        function()
          local exec = require("core.util.exec")
          if not exec.require_bin("ghdl", "sudo zypper in ghdl") then return end
          local entity = vim.fn.input("Entity name: ")
          if entity == "" then return end
          require("core.util.term").float("ghdl -e " .. vim.fn.shellescape(entity))
        end,
        desc = "GHDL Elaborate", ft = "vhdl" },

      { "<leader>vhr",
        function()
          local exec = require("core.util.exec")
          if not exec.require_bin("ghdl", "sudo zypper in ghdl") then return end
          local entity = vim.fn.input("Entity name: ")
          if entity == "" then return end
          local vcd = vim.fn.getcwd() .. "/" .. entity .. "_wave.vcd"
          require("core.util.term").float(string.format(
            "ghdl -r %s --vcd=%s && gtkwave %s",
            vim.fn.shellescape(entity),
            vim.fn.shellescape(vcd),
            vim.fn.shellescape(vcd)
          ))
        end,
        desc = "GHDL Run & View (gtkwave)", ft = "vhdl" },

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

          -- Signal declarations (use the full type string from the parser)
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
