-- lua/core/util/buf_keymap.lua — buffer-local keymap registration helper
--

local M = {}

--- Register a single buffer-local keymap with optional conflict detection.
---@param buf   integer
---@param mode  string|table
---@param lhs   string
---@param rhs   function|string
---@param desc  string
function M.set(buf, mode, lhs, rhs, desc)
  -- Conflict detection: warn when the lhs is already mapped in the same mode.
  local modes = type(mode) == "table" and mode or { mode }
  for _, m in ipairs(modes) do
    local existing = vim.fn.maparg(lhs, m, false, true)
    if existing and existing.buffer == 1 and existing.lhs then
      -- Only warn when the RHS differs (re-registering the same mapping is fine).
      local existing_rhs = existing.callback and "<callback>" or (existing.rhs or "")
      local new_rhs      = type(rhs) == "function" and "<callback>" or tostring(rhs)
      if existing_rhs ~= new_rhs then
        vim.notify(
          string.format(
            "[buf_keymap] conflict: mode=%s lhs=%s buf=%d\n  existing: %s\n  new:      %s",
            m, lhs, buf, existing_rhs, new_rhs
          ),
          vim.log.levels.DEBUG
        )
      end
    end
  end

  vim.keymap.set(mode, lhs, rhs, {
    buffer  = buf,
    silent  = true,
    noremap = true,
    desc    = desc,
  })
end

---@param buf        integer
---@param maps       table    list of { mode, lhs, rhs, desc }
---@param flag_name  string?  deduplication key stored in vim.b[buf]
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

---@param ft         string
---@param maps       table
---@param flag_name  string?
---@param group      string?
function M.on_ft(ft, maps, flag_name, group)
  local augroup_name = group
    or ("BufKeymap_" .. ft .. (flag_name and ("_" .. flag_name) or ""))

  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_loaded(buf) and vim.bo[buf].filetype == ft then
      M.batch(buf, maps, flag_name)
    end
  end

  vim.api.nvim_create_autocmd("FileType", {
    pattern  = ft,
    group    = vim.api.nvim_create_augroup(augroup_name, { clear = true }),
    callback = function(e) M.batch(e.buf, maps, flag_name) end,
    desc     = "Register " .. ft .. " buffer-local keymaps",
  })
end

-- ── M.clear ───────────────────────────────────────────────────────────────────
--
-- Usage:
--   M.clear(buf, "python_iron_keymaps_registered")
--
---@param buf       integer
---@param flag_name string   the deduplication flag used when registering
---@param maps      table?   optional: the original maps table for targeted del
function M.clear(buf, flag_name, maps)
  if not vim.api.nvim_buf_is_valid(buf) then return end

  -- Reset the deduplication flag so the next batch() call re-registers.
  pcall(function() vim.b[buf][flag_name] = nil end)

  -- If the original maps table is provided, delete each lhs explicitly.
  if type(maps) == "table" then
    for _, spec in ipairs(maps) do
      local mode, lhs = spec[1], spec[2]
      local modes = type(mode) == "table" and mode or { mode }
      for _, m in ipairs(modes) do
        pcall(function()
          local existing = vim.fn.maparg(lhs, m, false, true)
          if existing and existing.buffer == 1 then
            vim.keymap.del(m, lhs, { buffer = buf })
          end
        end)
      end
    end
  end
end

return M
