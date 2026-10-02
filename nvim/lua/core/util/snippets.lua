-- lua/core/util/snippets.lua — LuaSnip snippet factory helpers

local M = {}

-- ── M.ref ─────────────────────────────────────────────────────────────────────
--
---@param  n       integer   insert_node index to mirror
---@param  default string?   fallback when node n is empty (defaults to "")
---@return table|nil         valid LuaSnip node, or nil if LuaSnip unavailable
function M.ref(n, default)
  local ok, ls = pcall(require, "luasnip")
  if not ok then
    vim.notify(
      "[snippets] M.ref(): LuaSnip unavailable — returning nil.\n"
      .. "M.ref() must only be called from a factory passed to M.load().",
      vim.log.levels.DEBUG
    )
    return nil
  end
  return ls.function_node(function(args)
    local val = args[1] and args[1][1]
    return (val and val ~= "") and val or (default or "")
  end, { n })
end

-- ── M.load ────────────────────────────────────────────────────────────────────
--
-- Load snippets for *ft* using a factory function.
-- The factory receives five arguments: (s, t, i, f, ref)
--
---@param ft      string    Neovim filetype
---@param factory function  (s, t, i, f, ref) → snippet list
---@param id      string?   stable id; a repeat load with the same (ft, id) REPLACES, never duplicates
function M.load(ft, factory, id)
  local ok, ls = pcall(require, "luasnip")
  if not ok then
    vim.notify(
      string.format("[snippets] LuaSnip not available — %s snippets skipped", ft),
      vim.log.levels.DEBUG
    )
    return
  end

  if type(factory) ~= "function" then
    vim.notify(
      string.format("[snippets] factory for '%s' must be a function, got %s",
        ft, type(factory)),
      vim.log.levels.WARN
    )
    return
  end

  local ok_factory, snippets = pcall(
    factory,
    ls.snippet,
    ls.text_node,
    ls.insert_node,
    ls.function_node,
    M.ref
  )

  if not ok_factory then
    vim.notify(
      string.format("[snippets] factory error for '%s': %s", ft, tostring(snippets)),
      vim.log.levels.WARN
    )
    return
  end

  if type(snippets) ~= "table" then
    vim.notify(
      string.format("[snippets] factory for '%s' returned %s, expected table",
        ft, type(snippets)),
      vim.log.levels.WARN
    )
    return
  end

  local clean_snippets = vim.tbl_filter(function(s) return s ~= nil end, snippets)

  -- `key` makes add_snippets idempotent: reloads/hot-swaps replace, not duplicate.
  local key = "nvim-ide:" .. ft .. ":" .. (id or "default")
  local ok_add, err = pcall(ls.add_snippets, ft, clean_snippets, { key = key })
  if not ok_add then
    vim.notify(
      string.format("[snippets] add_snippets failed for '%s': %s", ft, tostring(err)),
      vim.log.levels.WARN
    )
  end
end

-- ── Deferred registration ─────────────────────────────────────────────────────
--
-- lazy.nvim keeps only ONE `config` per plugin (last spec wins), so per-language
-- `{ "L3MON4D3/LuaSnip", config = ... }` specs overwrite each other. Lang files
-- call M.register() at file scope instead; completion.lua owns the single
-- LuaSnip config, which calls M.flush().
local _pending = {}
local _flushed = false

---@param ft      string
---@param factory function  (s, t, i, f, ref) → snippet list
---@param id      string?
function M.register(ft, factory, id)
  if type(ft) ~= "string" or ft == "" or type(factory) ~= "function" then
    vim.notify("[snippets] register(ft: string, factory: function[, id])", vim.log.levels.WARN)
    return
  end
  id = id or "default"
  _pending[ft .. "\0" .. id] = { ft = ft, factory = factory, id = id }
  if _flushed then M.load(ft, factory, id) end   -- LuaSnip already up: load now
end

--- Load everything queued so far. Idempotent (keyed add_snippets).
function M.flush()
  _flushed = true
  for _, e in pairs(_pending) do M.load(e.ft, e.factory, e.id) end
end

---@return function?, function?, function?, function?   s, t, i, f
function M.destructure()
  local ok, ls = pcall(require, "luasnip")
  if not ok then
    vim.notify("[snippets] LuaSnip not available — destructure() returned nils",
      vim.log.levels.DEBUG)
    return nil, nil, nil, nil
  end
  return ls.snippet, ls.text_node, ls.insert_node, ls.function_node
end

---@param path string  absolute path to a snippets directory
function M.load_vscode(path)
  if vim.fn.isdirectory(path) ~= 1 then
    vim.notify(
      string.format("[snippets] vscode path not found: %s", path),
      vim.log.levels.WARN
    )
    return
  end
  pcall(function()
    require("luasnip.loaders.from_vscode").load({ paths = { path } })
  end)
end

-- ── M.list ────────────────────────────────────────────────────────────────────
---@param ft string
---@return table[]
function M.list(ft)
  local ok, ls = pcall(require, "luasnip")
  if not ok then return {} end

  local ok_snips, snip_table = pcall(function()
    return ls.get_snippets(ft) or {}
  end)
  if not ok_snips then return {} end
  return snip_table
end

-- ── M.remove ─────────────────────────────────────────────────────────────────
---@param ft      string
---@param trigger string
---@return integer  count of removed snippets (0 on failure or not-found)
function M.remove(ft, trigger)
  local ok, ls = pcall(require, "luasnip")
  if not ok then return 0 end

  if type(trigger) ~= "string" or trigger == "" then
    vim.notify("[snippets] remove(): trigger must be a non-empty string",
      vim.log.levels.WARN)
    return 0
  end

  local existing = M.list(ft)
  local kept     = {}
  local removed  = 0

  for _, snip in ipairs(existing) do
    if type(snip) == "table" and snip.trigger == trigger then
      removed = removed + 1
    else
      table.insert(kept, snip)
    end
  end

  if removed == 0 then
    vim.notify(
      string.format("[snippets] remove(): trigger '%s' not found in ft='%s'.", trigger, ft),
      vim.log.levels.DEBUG
    )
    return 0
  end

  local mutation_ok = pcall(function()
    local store = ls.get_snippets()
    store[ft]   = kept
  end)

  if not mutation_ok then
    vim.notify(
      string.format(
        "[snippets] remove(): internal store mutation failed for ft='%s', trigger='%s'.\n"
        .. "  Cause: LuaSnip's get_snippets() no longer returns a mutable reference,\n"
        .. "  likely due to an internal API change after a LuaSnip update.\n"
        .. "  Snippets for '%s' are UNCHANGED. Run ':Lazy update LuaSnip' and retry.",
        ft, trigger, ft
      ),
      vim.log.levels.WARN
    )
    return 0
  end

  -- Post-mutation verification: confirm the trigger is actually gone.
  local post = M.list(ft)
  for _, snip in ipairs(post) do
    if type(snip) == "table" and snip.trigger == trigger then
      vim.notify(
        string.format(
          "[snippets] remove(): trigger '%s' still present in ft='%s' after mutation.\n"
          .. "  The store write did not propagate. This is a LuaSnip internal invariant\n"
          .. "  violation. No snippets were removed.",
          trigger, ft
        ),
        vim.log.levels.WARN
      )
      return 0
    end
  end

  return removed
end

return M
