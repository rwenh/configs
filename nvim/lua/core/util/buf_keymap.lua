-- lua/core/util/buf_keymap.lua — buffer-local keymap registration helper
--

local M = {}

--- Register a single buffer-local keymap.
---
---@param buf       integer       buffer handle
---@param mode      string|table  vim mode(s)
---@param lhs       string        left-hand side key sequence
---@param rhs       function|string
---@param desc      string        description shown in which-key / :map
function M.set(buf, mode, lhs, rhs, desc)
  vim.keymap.set(mode, lhs, rhs, {
    buffer  = buf,
    silent  = true,
    noremap = true,
    desc    = desc,
  })
end

---@param buf        integer    buffer handle
---@param maps       table      list of { mode, lhs, rhs, desc }
---@param flag_name  string?    optional deduplication key stored in vim.b[buf]
function M.batch(buf, maps, flag_name)
  if flag_name then
    local ok, already = pcall(function() return vim.b[buf][flag_name] end)
    if ok and already then return end
    pcall(function() vim.b[buf][flag_name] = true end)
  end

  for _, spec in ipairs(maps) do
    local mode, lhs, rhs, desc = spec[1], spec[2], spec[3], spec[4]
    M.set(buf, mode, lhs, rhs, desc)
  end
end

---@param ft         string     filetype to match (e.g. "python")
---@param maps       table      list of { mode, lhs, rhs, desc }
---@param flag_name  string?    deduplication key (recommended)
---@param group      string?    augroup name (defaults to "BufKeymap_<ft>_<flag>")
function M.on_ft(ft, maps, flag_name, group)
  local augroup_name = group
    or ("BufKeymap_" .. ft .. (flag_name and ("_" .. flag_name) or ""))

  -- Retroactively apply to already-open buffers of this filetype.
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_loaded(buf)
    and vim.bo[buf].filetype == ft then
      M.batch(buf, maps, flag_name)
    end
  end

  -- Future buffers.
  vim.api.nvim_create_autocmd("FileType", {
    pattern  = ft,
    group    = vim.api.nvim_create_augroup(augroup_name, { clear = true }),
    callback = function(e) M.batch(e.buf, maps, flag_name) end,
    desc     = "Register " .. ft .. " buffer-local keymaps",
  })
end

return M
