-- lua/plugins/specs/lang/shared.lua — shared constants for lang specs
--
-- Centralises filetype lists that were duplicated across web.lua, html.lua,
-- css.lua, javascript.lua, typescript.lua (Batch 8 DRY fix D2).
--
-- Usage:
--   local shared = require("plugins.specs.lang.shared")
--   ft = shared.WEB_FT      -- all web filetypes
--   ft = shared.JS_FT       -- JavaScript + JSX
--   ft = shared.TS_FT       -- TypeScript + TSX

local M = {}

-- Standard Neovim filetypes for JSX/TSX are "javascriptreact" / "typescriptreact".
-- "jsx" and "tsx" are NOT valid Neovim filetypes — they were dead entries in
-- the original config.

M.JS_FT = { "javascript", "javascriptreact" }
M.TS_FT = { "typescript", "typescriptreact" }
M.JS_TS_FT = vim.list_extend(
  vim.deepcopy(M.JS_FT),
  vim.deepcopy(M.TS_FT)
)

-- Full web filetype set: HTML, CSS/SCSS, JS, TS, templating, frameworks.
M.WEB_FT = {
  "html", "htmldjango", "jinja.html",
  "css", "scss", "less",
  "javascript", "javascriptreact",
  "typescript", "typescriptreact",
  "vue", "svelte",
}

-- Tailwind-specific filetypes (subset of WEB_FT + extras).
M.TAILWIND_FT = {
  "html", "css",
  "javascript", "javascriptreact",
  "typescript", "typescriptreact",
  "vue", "svelte",
}

return M
