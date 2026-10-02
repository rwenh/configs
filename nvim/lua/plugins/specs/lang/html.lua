-- lua/plugins/specs/lang/html.lua — HTML development
--

local shared = require("plugins.specs.lang.shared")

local function has_htmlhint_config()
  local ok_path, path_util = pcall(require, "core.util.path")
  local root = (ok_path and path_util.find_root()) or vim.fn.getcwd()
  if not root or root == "" then return false end

  local candidates = {
    root .. "/.htmlhintrc",
    root .. "/.htmlhint.json",
    root .. "/.htmlhint.js",
    root .. "/htmlhint.config.js",
  }
  for _, f in ipairs(candidates) do
    if vim.fn.filereadable(f) == 1 then return true end
  end

  local pkg = root .. "/package.json"
  if vim.fn.filereadable(pkg) == 1 then
    local ok, lines = pcall(vim.fn.readfile, pkg)
    if ok then
      local content = table.concat(lines, "\n")
      local ok_j, obj = pcall(vim.json.decode, content)
      if ok_j and type(obj) == "table" and obj["htmlhint"] then return true end
    end
  end

  return false
end

local _htmlhint_registered = false

local function try_register_htmlhint()
  if _htmlhint_registered then return end
  if vim.fn.executable("htmlhint") ~= 1 then return end
  if not has_htmlhint_config() then
    vim.notify(
      "[html] htmlhint found but no config detected (checked .htmlhintrc and package.json) — linter skipped.\n"
      .. "Create a .htmlhintrc or add an 'htmlhint' key in package.json.",
      vim.log.levels.DEBUG
    )
    return
  end
  local ok, lint = pcall(require, "lint")
  if not ok then return end
  lint.linters_by_ft = lint.linters_by_ft or {}
  lint.linters_by_ft.html = lint.linters_by_ft.html or {}
  local already = false
  for _, l in ipairs(lint.linters_by_ft.html) do
    if l == "htmlhint" then already = true; break end
  end
  if not already then table.insert(lint.linters_by_ft.html, "htmlhint") end
  _htmlhint_registered = true
end

vim.api.nvim_create_autocmd("FileType", {
  pattern  = { "html", "htmldjango" },
  group    = vim.api.nvim_create_augroup("HtmlHintConditional", { clear = true }),
  callback = try_register_htmlhint,
  desc = "Conditionally register htmlhint when config is present (re-checked on every relevant buffer, not just the first)",
})

vim.api.nvim_create_autocmd("DirChanged", {
  group    = vim.api.nvim_create_augroup("HtmlHintRecheck", { clear = true }),
  callback = function()
    if _htmlhint_registered then return end
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
      if vim.api.nvim_buf_is_loaded(buf) then
        local ft = vim.bo[buf].filetype
        if vim.tbl_contains({ "html", "htmldjango" }, ft) then
          try_register_htmlhint()
          return
        end
      end
    end
  end,
  desc = "Re-check htmlhint config when the working directory changes",
})

-- ── Snippets (queued; flushed once by completion.lua's LuaSnip config) ──
require("core.util.snippets").register("html", function(s, t, i, _, ref)
  return {
    s("html5", {
      t({ "<!DOCTYPE html>", '<html lang="' }), i(1, "en"), t({ '">', "<head>",
        '  <meta charset="UTF-8" />', '  <meta name="viewport" content="width=device-width, initial-scale=1.0" />',
        "  <title>" }), i(2, "Document"), t({ "</title>", "</head>", "<body>", "  " }),
      i(0), t({ "", "</body>", "</html>" }),
    }),
    s("tag", {
      t("<"), i(1, "div"), t(' class="'), i(2), t('">'),
      t({ "", "  " }), i(0), t({ "", "</" }), ref(1, "div"), t(">"),
    }),
    s("inp", {
      t('<input type="'), i(1, "text"), t('" name="'), i(2),
      t('" id="'), ref(2), t('" placeholder="'), i(3), t('" />'),
    }),
  }
end)

return {
  shared.treesitter({ "html" }),
}
