-- lua/core/util/buf_keymap.lua — buffer-local keymap registration helper
--

local M = {}

---@param buf   integer
---@param mode  string|table
---@param lhs   string
---@param rhs   function|string
---@param desc  string
function M.set(buf, mode, lhs, rhs, desc)
  local modes = type(mode) == "table" and mode or { mode }
  for _, m in ipairs(modes) do
    local existing = vim.fn.maparg(lhs, m, false, true)
    if existing and existing.buffer == 1 and existing.lhs then
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
---@param maps       table
---@param flag_name  string?
function M.batch(buf, maps, flag_name)
  if flag_name then
    -- FIX (low) #4: check existing flag.
    local ok_read, already = pcall(function() return vim.b[buf][flag_name] end)
    if ok_read and already then return end

    -- Attempt to write the dedup flag.
    local ok_write = pcall(function() vim.b[buf][flag_name] = true end)

    if not ok_write then
      -- The buffer is invalid or the write failed. Emit a debug notice and
      -- abort — registering keymaps on an invalid buffer would also fail.
      vim.notify(
        string.format(
          "[buf_keymap] batch(): could not set dedup flag '%s' on buf=%d "
          .. "(buffer may be invalid). Skipping keymap registration.",
          flag_name, buf
        ),
        vim.log.levels.DEBUG
      )
      return
    end

    -- Verify the write actually persisted (defensive check).
    local ok_verify, written = pcall(function() return vim.b[buf][flag_name] end)
    if not ok_verify or not written then
      vim.notify(
        string.format(
          "[buf_keymap] batch(): dedup flag '%s' write did not persist on buf=%d. "
          .. "Skipping to avoid duplicate registrations.",
          flag_name, buf
        ),
        vim.log.levels.DEBUG
      )
      return
    end
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

---@param buf       integer
---@param flag_name string
---@param maps      table?
function M.clear(buf, flag_name, maps)
  if not vim.api.nvim_buf_is_valid(buf) then return end

  pcall(function() vim.b[buf][flag_name] = nil end)

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
