-- lua/plugins/specs/lang/vhdl.lua — VHDL hardware description language
--

local shared = require("plugins.specs.lang.shared")
local exec   = require("core.util.exec")
local path   = require("core.util.path")
local vhdl   = require("core.util.vhdl")

local function term() return require("core.util.term") end

local function notify(msg, level)
  vim.notify("[vhdl] " .. msg, level or vim.log.levels.INFO)
end

---@param start string?  directory to start from (default: the buffer's directory)
---@return string        project root, or the working directory when no marker is found
local function project_root(start)
  return path.find_root(start) or vim.fn.getcwd()
end

---@return string|nil  absolute path of the current buffer, or nil (with a warning) when unnamed
local function current_file()
  local file = vim.fn.expand("%:p")
  if file == "" then
    notify("this buffer has no file on disk", vim.log.levels.WARN)
    return nil
  end
  return file
end

---@return string[]
local function buffer_lines()
  return vim.api.nvim_buf_get_lines(0, 0, -1, false)
end

-- ── vhdl_ls.toml: one notice per project root per session ─────────────────────
local _noticed = {}

vim.api.nvim_create_autocmd("FileType", {
  pattern  = "vhdl",
  group    = vim.api.nvim_create_augroup("VhdlLsConfigCheck", { clear = true }),
  callback = function(e)
    local name = vim.api.nvim_buf_get_name(e.buf)
    local root = project_root(name ~= "" and vim.fn.fnamemodify(name, ":h") or nil)
    if _noticed[root] then return end
    _noticed[root] = true
    if vim.fn.filereadable(root .. "/vhdl_ls.toml") ~= 1 then
      notify(
        "no vhdl_ls.toml in " .. root .. ": vhdl_ls will analyse without library paths. "
          .. "See https://github.com/VHDL-LS/rust_hdl#configuration"
      )
    end
  end,
  desc = "Notice when a VHDL project has no vhdl_ls.toml (once per root)",
})

-- ── Snippets (queued; flushed once by completion.lua's LuaSnip config) ──────────
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

-- ── ghdl ──────────────────────────────────────────────────────────────────────
local function ghdl_ready()
  return exec.require_bin("ghdl", "sudo zypper in ghdl")
end

---@param cmd string  shell command, run from the project root
local function ghdl_in_root(cmd)
  term().float(term().cd_prefix(project_root()) .. cmd)
end

---@return boolean  false when the save failed and the command must not run
local function save_for_run()
  if not (vim.bo.modified and vim.g.runner_autosave ~= false) then return true end
  local ok, err = pcall(vim.cmd, "silent write")
  if not ok then notify("could not save before running: " .. tostring(err), vim.log.levels.WARN) end
  return ok
end

---@return string|nil  nil on cancel, or when the answer is not a VHDL identifier
local function ask_entity(prompt)
  local default = vhdl.parse_entity(buffer_lines()) or ""
  local name = vim.trim(vim.fn.input(prompt, default))
  if name == "" then return nil end
  if not vhdl.is_identifier(name) then
    notify(("'%s' is not a valid VHDL identifier"):format(name), vim.log.levels.WARN)
    return nil
  end
  return name
end

local function analyse()
  local file = current_file()
  if not file or not ghdl_ready() or not save_for_run() then return end
  ghdl_in_root("ghdl -a " .. vim.fn.shellescape(file))
end

local function elaborate()
  if not ghdl_ready() then return end
  local entity = ask_entity("Entity to elaborate: ")
  if entity then ghdl_in_root("ghdl -e " .. vim.fn.shellescape(entity)) end
end

local function simulate()
  local file = current_file()
  if not file or not ghdl_ready() or not save_for_run() then return end
  local entity = ask_entity("Entity to simulate: ")
  if not entity then return end
  local vcd = project_root() .. "/" .. entity .. "_wave.vcd"
  local cmd = vhdl.run_chain(file, entity, vcd)
  if vim.fn.executable("gtkwave") == 1 then
    cmd = cmd .. " && gtkwave " .. vim.fn.shellescape(vcd)
  else
    cmd = cmd .. " && echo " .. vim.fn.shellescape("VCD written to " .. vcd .. " (gtkwave is not installed)")
  end
  ghdl_in_root(cmd)
end

local function syntax_check()
  local file = current_file()
  if not file or not ghdl_ready() or not save_for_run() then return end
  ghdl_in_root("ghdl -s " .. vim.fn.shellescape(file))
end

-- ── Testbench skeleton ────────────────────────────────────────────────────────
local function write_testbench()
  local file = current_file()
  if not file then return end
  local name, ports, generics, err = vhdl.parse_entity(buffer_lines())
  if err then
    notify(err .. "; fix the declaration first", vim.log.levels.WARN)
    return
  end
  if not name then
    notify("no entity declaration found in this buffer", vim.log.levels.WARN)
    return
  end

  local out = vim.fn.fnamemodify(file, ":h") .. "/tb_" .. name .. ".vhd"
  local lines, unresolved = vhdl.testbench(name, ports, generics)

  local function write()
    -- writefile raises on failure (E482) and returns -1 on others; check both.
    local ok, rc = pcall(vim.fn.writefile, lines, out)
    if not ok then
      notify("could not write " .. out .. ": " .. tostring(rc), vim.log.levels.ERROR)
      return
    end
    if rc ~= 0 then
      notify("could not write " .. out .. " (writefile returned " .. tostring(rc) .. ")", vim.log.levels.ERROR)
      return
    end
    if #unresolved > 0 then
      notify(("generic(s) %s have no default and an unknown type; replace <value> before analysing")
        :format(table.concat(unresolved, ", ")), vim.log.levels.WARN)
    end
    vim.cmd("edit " .. vim.fn.fnameescape(out))
  end

  if vim.fn.filereadable(out) == 1 then
    vim.ui.input({ prompt = out .. " exists. Overwrite? [y/N]: " }, function(answer)
      if answer == "y" or answer == "Y" then
        write()
      else
        notify("testbench generation cancelled")
      end
    end)
  else
    write()
  end
end

-- ── Formatter: vsg ────────────────────────────────────────────────────────────
local function vsg_enabled()
  return not vim.g.disable_vsg_format and vim.fn.executable("vsg") == 1
end

return {
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters = opts.formatters or {}
      opts.formatters.vsg = {
        command    = "vsg",
        args       = { "--fix", "-f", "$FILENAME" },
        stdin      = false,
        exit_codes = { 0, 1 },
        condition  = vsg_enabled,
      }
    end,
  },

  shared.treesitter({ "vhdl" }),

  {
    "akinsho/toggleterm.nvim",
    keys = {
      { "<leader>vha", analyse,         desc = "GHDL Analyze current file",             ft = "vhdl" },
      { "<leader>vhe", elaborate,       desc = "GHDL Elaborate (prompt entity)",        ft = "vhdl" },
      { "<leader>vhr", simulate,        desc = "GHDL Analyze + Elaborate + Run + View", ft = "vhdl" },
      { "<leader>vhc", syntax_check,    desc = "GHDL Syntax Check",                     ft = "vhdl" },
      { "<leader>vht", write_testbench, desc = "VHDL Generate testbench skeleton",      ft = "vhdl" },
    },
  },
}
