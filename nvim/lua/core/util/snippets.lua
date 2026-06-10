-- lua/core/util/snippets.lua — LuaSnip snippet factory helpers
--

local M = {}

---@param  n       integer   insert_node index to mirror
---@param  default string?   fallback when node n is empty (defaults to "")
---@return table             LuaSnip function_node, or {} on LuaSnip failure
function M.ref(n, default)
  local ok, ls = pcall(require, "luasnip")
  if not ok then
    return { type = "textNode", static_text = { default or "" } }
  end
  return ls.function_node(function(args)
    local val = args[1] and args[1][1]
    return (val and val ~= "") and val or (default or "")
  end, { n })
end

--- Load snippets for *ft* using a factory function.
--
-- The factory receives five arguments: (s, t, i, f, ref)
--
---@param ft      string    Neovim filetype
---@param factory function  (s, t, i, f, ref) → snippet list
function M.load(ft, factory)
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

  -- Conflict detection: warn when a trigger already exists for the ft.
  local existing = M.list(ft)
  local existing_set = {}
  for _, snip in ipairs(existing) do
    if type(snip.trigger) == "string" then
      existing_set[snip.trigger] = true
    end
  end

  local conflicts = {}
  for _, snip in ipairs(snippets) do
    if type(snip) == "table" and type(snip.trigger) == "string" then
      if existing_set[snip.trigger] then
        table.insert(conflicts, snip.trigger)
      end
    end
  end

  if #conflicts > 0 then
    vim.notify(
      string.format(
        "[snippets] %s: trigger(s) already registered, will override: %s",
        ft, table.concat(conflicts, ", ")
      ),
      vim.log.levels.DEBUG
    )
  end

  local ok_add, err = pcall(ls.add_snippets, ft, snippets)
  if not ok_add then
    vim.notify(
      string.format("[snippets] add_snippets failed for '%s': %s", ft, tostring(err)),
      vim.log.levels.WARN
    )
  end
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
--
-- Returns the list of LuaSnip snippet objects currently registered for *ft*.
--
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
--
-- Remove snippets matching *trigger* from *ft*.
-- Returns the number of snippets removed, or 0 if no match was found or if
-- the internal LuaSnip store mutation failed or could not be verified.
--
---@param ft      string   Neovim filetype
---@param trigger string   trigger string to remove (exact match)
---@return integer         count of removed snippets (0 on failure)
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

  if removed == 0 then return 0 end

  local mutation_ok = pcall(function()
    local store = ls.get_snippets()
    store[ft]   = kept
  end)

  if not mutation_ok then
    vim.notify(
      string.format(
        "[snippets] remove(): internal store mutation failed for ft='%s', trigger='%s'.\n"
        .. "This may occur after a LuaSnip update that changes the internal API.\n"
        .. "Snippets for '%s' are unchanged.",
        ft, trigger, ft
      ),
      vim.log.levels.WARN
    )
    return 0
  end

  -- Return 0 if the trigger is still present so callers are not misled.
  local post = M.list(ft)
  for _, snip in ipairs(post) do
    if type(snip) == "table" and snip.trigger == trigger then
      vim.notify(
        string.format(
          "[snippets] remove(): trigger '%s' still present in ft='%s' after mutation.\n"
          .. "The internal store write did not propagate — no snippets removed.",
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
