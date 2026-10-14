-- lua/core/util/limits.lua — single source for size thresholds
--
-- Escape hatch: vim.g.large_file_bytes (number, bytes). Invalid values fall back to the default.

local M = {}

local DEFAULT_LARGE_FILE_BYTES = 500 * 1024

---@return integer
function M.large_file_bytes()
  local v = vim.g.large_file_bytes
  if type(v) == "number" and v > 0 then
    return math.floor(v)
  end
  return DEFAULT_LARGE_FILE_BYTES
end

--- Human label for notifications, e.g. "500 KB".
---@return string
function M.large_file_label()
  local bytes = M.large_file_bytes()
  if bytes >= 1024 * 1024 and bytes % (1024 * 1024) == 0 then
    return string.format("%d MB", bytes / (1024 * 1024))
  end
  return string.format("%d KB", math.floor(bytes / 1024 + 0.5))
end

return M
