-- lua/core/util/format.lua — single source of truth for formatting
--
-- Timeout resolution, range handling and the format-on-save policy used to be
-- copy-pasted into commands.lua, lsp.lua and the conform spec.

local M = {}

local DEFAULT_TIMEOUT_MS = 3000

--- Resolve the timeout: per-filetype → global → default.
---@param bufnr integer?
---@return integer
function M.timeout_ms(bufnr)
  local ft    = vim.bo[bufnr or 0].filetype
  local by_ft = type(vim.g.format_timeout_by_ft) == "table" and vim.g.format_timeout_by_ft or {}
  local per   = by_ft[ft]
  if type(per) == "number" and per > 0 then return per end
  local g = vim.g.format_timeout_ms
  if type(g) == "number" and g > 0 then return g end
  return DEFAULT_TIMEOUT_MS
end

--- conform `format_on_save` policy.
---@param bufnr integer
---@return table|nil
function M.on_save(bufnr)
  if vim.g.disable_autoformat then return nil end
  local ok, v = pcall(function() return vim.b[bufnr].disable_autoformat end)
  if ok and v then return nil end
  return { timeout_ms = M.timeout_ms(bufnr), lsp_format = "fallback" }
end

local function result_handler(ft, timeout_ms)
  return function(err)
    if not err then return end
    if tostring(err):lower():find("timeout") then
      vim.notify(
        string.format(
          "[format] timed out after %d ms for filetype '%s'.\n"
          .. "  vim.g.format_timeout_by_ft = { %s = %d }\n"
          .. "  or: vim.g.format_timeout_ms = %d",
          timeout_ms, ft, ft, timeout_ms * 2, timeout_ms * 2
        ),
        vim.log.levels.WARN
      )
    else
      vim.notify("[format] error: " .. tostring(err), vim.log.levels.WARN)
    end
  end
end

--- Format a buffer or a line range.
---@param opts { bufnr: integer?, line1: integer?, line2: integer? }?
function M.run(opts)
  opts = opts or {}
  local ok, conform = pcall(require, "conform")
  if not ok then vim.notify("[format] conform.nvim not available", vim.log.levels.ERROR); return end

  local bufnr = (opts.bufnr and opts.bufnr ~= 0) and opts.bufnr or vim.api.nvim_get_current_buf()
  local ft         = vim.bo[bufnr].filetype
  local timeout_ms = M.timeout_ms(bufnr)

  local copts = { bufnr = bufnr, timeout_ms = timeout_ms, lsp_format = "fallback", quiet = true }
  if opts.line1 and opts.line2 then
    local l1, l2 = math.min(opts.line1, opts.line2), math.max(opts.line1, opts.line2)
    local last   = vim.api.nvim_buf_get_lines(bufnr, l2 - 1, l2, false)[1] or ""
    -- conform wants a BYTE column (the old code used strchars, wrong for multibyte).
    copts.range = { start = { l1, 0 }, ["end"] = { l2, #last } }
  end

  local run_ok, run_err = pcall(conform.format, copts, result_handler(ft, timeout_ms))
  if not run_ok then vim.notify("[format] error: " .. tostring(run_err), vim.log.levels.WARN) end
end

return M
