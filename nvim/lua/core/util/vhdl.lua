-- lua/core/util/vhdl.lua — VHDL entity parsing + testbench generation (pure, testable)
--

local M = {}

---@param line string
---@return string
function M.strip_comment(line)
  local in_string = false
  local i = 1
  while i <= #line do
    local ch = line:sub(i, i)
    if ch == '"' then
      in_string = not in_string
    elseif not in_string and ch == "-" and line:sub(i + 1, i + 1) == "-" then
      return line:sub(1, i - 1)
    end
    i = i + 1
  end
  return line
end

--- Inner text of the first `keyword ( ... )` group inside `text`, with balanced parens.
---@param text string  original-case text
---@param low  string  lower-cased copy (same length)
---@param keyword string
---@return string|nil
local function group(text, low, keyword)
  local s, e = low:find("%f[%w_]" .. keyword .. "%s*%(")
  if not s then return nil end
  local depth, i = 1, e + 1
  while i <= #text do
    local c = text:sub(i, i)
    if c == "(" then depth = depth + 1
    elseif c == ")" then
      depth = depth - 1
      if depth == 0 then return text:sub(e + 1, i - 1) end
    end
    i = i + 1
  end
  return text:sub(e + 1)   -- unbalanced: take the rest
end

local function split_decls(body)
  local out = {}
  for decl in (body .. ";"):gmatch("(.-);") do
    decl = vim.trim((decl:gsub("%s+", " ")))
    if decl ~= "" then out[#out + 1] = decl end
  end
  return out
end

local function names_of(names_part)
  local names = {}
  for n in names_part:gmatch("[%w_]+") do
    local l = n:lower()
    if l ~= "signal" and l ~= "constant" and l ~= "variable" then names[#names + 1] = n end
  end
  return names
end

---@param lines string[]
---@return string|nil entity_name
---@return { name: string, dir: string, typ: string }[] ports
---@return { name: string, typ: string, default: string? }[] generics
function M.parse_entity(lines)
  local clean = {}
  for _, l in ipairs(lines) do clean[#clean + 1] = M.strip_comment(l) end
  local text = table.concat(clean, "\n")
  local low  = text:lower()

  local s, e = low:find("%f[%w_]entity%s+[%w_]+%s+is%f[^%w_]")
  if not s then return nil, {}, {} end
  local name = text:sub(s, e):match("^%a+%s+([%w_]+)")

  -- Entity body ends at its first `end` (nothing inside an entity header contains one).
  local end_pos = low:find("%f[%w_]end%f[^%w_]", e + 1) or (#text + 1)
  local rtext, rlow = text:sub(e + 1, end_pos - 1), low:sub(e + 1, end_pos - 1)

  local generics, ports = {}, {}

  local gbody = group(rtext, rlow, "generic")
  if gbody then
    for _, decl in ipairs(split_decls(gbody)) do
      local names_part, rest = decl:match("^([%w_,%s]+):%s*(.+)$")
      if names_part then
        local typ, default = rest:match("^(.-)%s*:=%s*(.+)$")
        typ = vim.trim(typ or rest)
        for _, n in ipairs(names_of(names_part)) do
          generics[#generics + 1] = { name = n, typ = typ, default = default and vim.trim(default) or nil }
        end
      end
    end
  end

  local pbody = group(rtext, rlow, "port")
  if pbody then
    for _, decl in ipairs(split_decls(pbody)) do
      local names_part, dir, typ = decl:match("^([%w_,%s]+):%s*([%w_]+)%s+(.+)$")
      if names_part then
        typ = vim.trim((typ:gsub("%s*:=.*$", "")))
        for _, n in ipairs(names_of(names_part)) do
          ports[#ports + 1] = { name = n, dir = dir:lower(), typ = typ }
        end
      end
    end
  end

  return name, ports, generics
end

---@return string[] lines
function M.testbench(entity, ports, generics)
  local tb = "tb_" .. entity
  local L = {
    "library ieee;",
    "use ieee.std_logic_1164.all;",
    "use ieee.numeric_std.all;",
    "",
    "entity " .. tb .. " is",
    "end entity " .. tb .. ";",
    "",
    "architecture sim of " .. tb .. " is",
  }
  for _, g in ipairs(generics or {}) do
    L[#L + 1] = "  constant " .. g.name .. " : " .. g.typ .. (g.default and (" := " .. g.default) or "") .. ";"
  end
  for _, p in ipairs(ports) do
    L[#L + 1] = "  signal " .. p.name .. " : " .. p.typ .. ";"
  end
  L[#L + 1] = "begin"
  L[#L + 1] = "  uut: entity work." .. entity
  if generics and #generics > 0 then
    L[#L + 1] = "    generic map ("
    for i, g in ipairs(generics) do
      L[#L + 1] = "      " .. g.name .. " => " .. g.name .. ((i < #generics) and "," or "")
    end
    L[#L + 1] = "    )"
  end
  L[#L + 1] = "    port map ("
  for i, p in ipairs(ports) do
    L[#L + 1] = "      " .. p.name .. " => " .. p.name .. ((i < #ports) and "," or "")
  end
  L[#L + 1] = "    );"
  vim.list_extend(L, {
    "",
    "  stim: process",
    "  begin",
    "    -- TODO: add stimulus",
    "    wait;",
    "  end process stim;",
    "end architecture sim;",
  })
  return L
end

return M
