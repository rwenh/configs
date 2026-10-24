-- lua/core/util/vhdl.lua — VHDL entity parsing, testbench generation, ghdl command construction
--

local M = {}

local MODES = { ["in"] = true, out = true, inout = true, buffer = true, linkage = true }
local CLASS_KEYWORDS = { signal = true, constant = true, variable = true }

-- Values for generics that have no default, keyed by the base name of the type.
local PLACEHOLDER = {
  std_logic = "'0'", std_ulogic = "'0'", bit = "'0'",
  boolean = "false",
  integer = "0", natural = "0", positive = "1",
  real = "0.0",
  time = "0 ns",
  string = '""',
  std_logic_vector = "(others => '0')", std_ulogic_vector = "(others => '0')",
  bit_vector = "(others => '0')", unsigned = "(others => '0')", signed = "(others => '0')",
}

---@param c string  one character, or ""
---@return boolean
local function is_ident(c)
  return c ~= "" and c:match("[%w_]") ~= nil
end

--- Remove a trailing `--` comment. String literals ("" escapes) and character literals such as '"'
--- or '-' are skipped, so a quote or dash inside them neither opens nor closes anything.
---@param line string
---@return string
function M.strip_comment(line)
  local n, i, in_str = #line, 1, false
  while i <= n do
    local ch = line:sub(i, i)
    if in_str then
      if ch == '"' then
        if line:sub(i + 1, i + 1) == '"' then i = i + 1 else in_str = false end
      end
    elseif ch == "'" and line:sub(i + 2, i + 2) == "'" and not is_ident(line:sub(i - 1, i - 1)) then
      i = i + 2   -- character literal such as '"'; an attribute like clk'event is not one
    elseif ch == '"' then
      in_str = true
    elseif ch == "-" and line:sub(i + 1, i + 1) == "-" then
      return line:sub(1, i - 1)
    end
    i = i + 1
  end
  return line
end

--- Inner text of the first `keyword ( ... )` group in `text`, using `low` (its lower-cased copy).
--- Returns nil when the keyword is absent, and nil plus a message when the parentheses never close.
---@return string|nil body
---@return string|nil err
local function group(text, low, keyword)
  local _, e = low:find("%f[%w_]" .. keyword .. "%s*%(")
  if not e then return nil end
  local depth = 1
  for i = e + 1, #text do
    local c = text:sub(i, i)
    if c == "(" then
      depth = depth + 1
    elseif c == ")" then
      depth = depth - 1
      if depth == 0 then return text:sub(e + 1, i - 1) end
    end
  end
  return nil, "unbalanced parentheses in the " .. keyword .. " clause"
end

---@param body string
---@return string[]  declarations, each split at ';' and whitespace-normalised
local function split_decls(body)
  local out = {}
  for decl in (body .. ";"):gmatch("(.-);") do
    decl = vim.trim((decl:gsub("%s+", " ")))
    if decl ~= "" then out[#out + 1] = decl end
  end
  return out
end

---@param names_part string  "clk, rst " or "signal en "
---@return string[]
local function names_of(names_part)
  local names = {}
  for n in names_part:gmatch("[%w_]+") do
    if not CLASS_KEYWORDS[n:lower()] then names[#names + 1] = n end
  end
  return names
end

--- "in std_logic_vector(7 downto 0) := x" -> "in", "std_logic_vector(7 downto 0)".
--- The mode is optional in VHDL and defaults to `in`.
---@param rest string
---@return string mode
---@return string typ
local function mode_and_type(rest)
  local word, after = rest:match("^(%a+)%s+(.+)$")
  local mode, typ = "in", rest
  if word and MODES[word:lower()] then
    mode, typ = word:lower(), after
  end
  typ = vim.trim((typ:gsub("%s*:=.*$", "")))
  return mode, typ
end

--- Parse the first entity declaration in `lines`.
---@param lines string[]
---@return string|nil name
---@return { name: string, dir: string, typ: string }[]|nil ports
---@return { name: string, typ: string, default: string|nil }[]|nil generics
---@return string|nil err  set when a clause is present but malformed; ports and generics are nil then
function M.parse_entity(lines)
  local clean = {}
  for _, l in ipairs(lines) do clean[#clean + 1] = M.strip_comment(l) end
  local text = table.concat(clean, "\n")
  local low = text:lower()

  local s, e = low:find("%f[%w_]entity%s+[%w_]+%s+is%f[^%w_]")
  if not s then return nil, {}, {} end
  local name = text:sub(s, e):match("^%a+%s+([%w_]+)")

  -- An entity header contains no `end` keyword, so the first one closes the declaration.
  local end_pos = low:find("%f[%w_]end%f[^%w_]", e + 1) or (#text + 1)
  local rtext, rlow = text:sub(e + 1, end_pos - 1), low:sub(e + 1, end_pos - 1)

  local generics, ports = {}, {}

  local gbody, gerr = group(rtext, rlow, "generic")
  if gerr then return name, nil, nil, gerr end
  if gbody then
    for _, decl in ipairs(split_decls(gbody)) do
      local names_part, rest = decl:match("^([%w_,%s]+):%s*(.+)$")
      if names_part then
        local typ, default = rest:match("^(.-)%s*:=%s*(.+)$")
        typ = vim.trim(typ or rest)
        default = default and vim.trim(default) or nil
        for _, n in ipairs(names_of(names_part)) do
          generics[#generics + 1] = { name = n, typ = typ, default = default }
        end
      end
    end
  end

  local pbody, perr = group(rtext, rlow, "port")
  if perr then return name, nil, nil, perr end
  if pbody then
    for _, decl in ipairs(split_decls(pbody)) do
      local names_part, rest = decl:match("^([%w_,%s]+):%s*(.+)$")
      if names_part then
        local mode, typ = mode_and_type(rest)
        for _, n in ipairs(names_of(names_part)) do
          ports[#ports + 1] = { name = n, dir = mode, typ = typ }
        end
      end
    end
  end

  return name, ports, generics, nil
end

--- Placeholder value for a generic with no default, or nil when the type is not one we know.
---@param typ string
---@return string|nil
function M.placeholder(typ)
  local base = (typ or ""):lower():match("^(%a[%w_]*)")
  return base and PLACEHOLDER[base] or nil
end

---@param s string
---@return boolean
function M.is_identifier(s)
  return type(s) == "string" and s:match("^%a[%w_]*$") ~= nil
end

--- Testbench skeleton for `entity`. Returns the lines and the names of generics that could not be
--- given a value. A file with unresolved generics will not analyse until the <value> markers are filled in.
---@param entity string
---@param ports table[]?
---@param generics table[]?
---@return string[] lines
---@return string[] unresolved
function M.testbench(entity, ports, generics)
  ports, generics = ports or {}, generics or {}
  local tb = "tb_" .. entity
  local L = {
    "-- Testbench skeleton for " .. entity .. ". Fill in the TODO markers before running it.",
    "library ieee;",
    "use ieee.std_logic_1164.all;",
    "use ieee.numeric_std.all;",
    "",
    "entity " .. tb .. " is",
    "end entity " .. tb .. ";",
    "",
    "architecture sim of " .. tb .. " is",
  }

  local unresolved = {}
  for _, g in ipairs(generics) do
    local value, note = g.default, ""
    if not value then
      value = M.placeholder(g.typ)
      note = " -- TODO: no default in the entity"
      if not value then
        value = "<value>"
        unresolved[#unresolved + 1] = g.name
      end
    end
    L[#L + 1] = ("  constant %s : %s := %s;%s"):format(g.name, g.typ, value, note)
  end
  for _, p in ipairs(ports) do
    L[#L + 1] = ("  signal %s : %s;"):format(p.name, p.typ)
  end
  L[#L + 1] = "begin"

  -- Generic and port maps are optional in VHDL when there is nothing to map. An empty
  -- `port map ()` is a syntax error, so each map is emitted only when it has entries.
  L[#L + 1] = "  uut : entity work." .. entity
  if #generics > 0 then
    L[#L + 1] = "    generic map ("
    for i, g in ipairs(generics) do
      L[#L + 1] = ("      %s => %s%s"):format(g.name, g.name, i < #generics and "," or "")
    end
    L[#L + 1] = "    )"
  end
  if #ports > 0 then
    L[#L + 1] = "    port map ("
    for i, p in ipairs(ports) do
      L[#L + 1] = ("      %s => %s%s"):format(p.name, p.name, i < #ports and "," or "")
    end
    L[#L + 1] = "    )"
  end
  L[#L] = L[#L] .. ";"

  vim.list_extend(L, {
    "",
    "  stim : process",
    "  begin",
    "    -- TODO: drive the inputs here",
    "    wait;",
    "  end process stim;",
    "end architecture sim;",
  })
  return L, unresolved
end

---@param file string
---@param entity string
---@param vcd string?
---@return string
function M.run_chain(file, entity, vcd)
  local q = vim.fn.shellescape
  local cmd = ("ghdl -a %s && ghdl -e %s && ghdl -r %s"):format(q(file), q(entity), q(entity))
  if vcd and vcd ~= "" then cmd = cmd .. " --vcd=" .. q(vcd) end
  return cmd
end

return M
