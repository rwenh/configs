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
--   s   = ls.snippet
--   t   = ls.text_node
--   i   = ls.insert_node
--   f   = ls.function_node
--   ref = M.ref  (the mirroring helper above)
--
---@param ft      string    Neovim filetype (e.g. "c", "fortran", "vhdl", "cobol")
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
  if not ok then return nil, nil, nil, nil end
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

return M
