-- lua/core/util/jsonc.lua — string-aware JSONC (JSON with comments / trailing commas)
--

local M = {}

local BYTE = string.byte
local QUOTE, BACKSLASH, SLASH, STAR, NL, COMMA = BYTE('"'), BYTE("\\"), BYTE("/"), BYTE("*"), BYTE("\n"), BYTE(",")
local RBRACKET, RBRACE = BYTE("]"), BYTE("}")

--- Remove comments and trailing commas.
---@param text string
---@return string
function M.strip(text)
  if type(text) ~= "string" then return "" end
  if text:sub(1, 3) == "\239\187\191" then text = text:sub(4) end   -- UTF-8 BOM

  -- pass 1: comments
  local out, n, i, in_str = {}, #text, 1, false
  while i <= n do
    local c = BYTE(text, i)
    if in_str then
      out[#out + 1] = text:sub(i, i)
      if c == BACKSLASH then
        i = i + 1
        out[#out + 1] = text:sub(i, i)
      elseif c == QUOTE then
        in_str = false
      end
      i = i + 1
    elseif c == QUOTE then
      in_str = true
      out[#out + 1] = '"'
      i = i + 1
    elseif c == SLASH and BYTE(text, i + 1) == SLASH then
      local e = text:find("\n", i, true)          -- keep the newline (line numbers)
      i = e or (n + 1)
    elseif c == SLASH and BYTE(text, i + 1) == STAR then
      local e = text:find("*/", i + 2, true)
      i = e and (e + 2) or (n + 1)
    else
      out[#out + 1] = text:sub(i, i)
      i = i + 1
    end
  end
  local s = table.concat(out)

  -- pass 2: trailing commas (again outside strings only)
  out, n, i, in_str = {}, #s, 1, false
  while i <= n do
    local c = BYTE(s, i)
    if in_str then
      out[#out + 1] = s:sub(i, i)
      if c == BACKSLASH then
        i = i + 1
        out[#out + 1] = s:sub(i, i)
      elseif c == QUOTE then
        in_str = false
      end
      i = i + 1
    elseif c == QUOTE then
      in_str = true
      out[#out + 1] = '"'
      i = i + 1
    elseif c == COMMA then
      local j = i + 1
      while j <= n and s:sub(j, j):match("%s") do j = j + 1 end
      local nx = BYTE(s, j)
      if nx ~= RBRACKET and nx ~= RBRACE then out[#out + 1] = "," end
      i = i + 1
    else
      out[#out + 1] = s:sub(i, i)
      i = i + 1
    end
  end
  return table.concat(out)
end

--- Decode JSONC text.
---@param text string
---@return table|nil result
---@return string?   err
function M.decode(text)
  local ok, res = pcall(vim.json.decode, M.strip(text))
  if ok and type(res) == "table" then return res end
  return nil, ok and "top-level value is not an object/array" or tostring(res)
end

--- Read and decode a JSONC file.
---@param path string
---@return table|nil
---@return string? err
function M.read(path)
  local ok, lines = pcall(vim.fn.readfile, path)
  if not ok then return nil, "cannot read " .. path end
  return M.decode(table.concat(lines, "\n"))
end

return M
