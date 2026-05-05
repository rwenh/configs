-- lua/plugins/specs/lang/shared.lua — shared constants and spec helpers
--
-- Usage:
--   local shared = require("plugins.specs.lang.shared")
--
--   ft = shared.WEB_FT                    -- all web filetypes
--   ft = shared.JS_FT                     -- JavaScript + JSX
--   ft = shared.TS_FT                     -- TypeScript + TSX
--   shared.treesitter({ "go", "gomod" })  -- returns optional treesitter spec

local M = {}

-- ── Filetype lists ─────────────────────────────────────────────────────────

-- Standard Neovim filetypes for JSX/TSX are "javascriptreact" / "typescriptreact".
-- "jsx" and "tsx" are NOT valid Neovim filetypes.
M.JS_FT    = { "javascript", "javascriptreact" }
M.TS_FT    = { "typescript", "typescriptreact" }
M.JS_TS_FT = { "javascript", "javascriptreact", "typescript", "typescriptreact" }

M.WEB_FT = {
  "html", "htmldjango", "jinja.html",
  "css", "scss", "less",
  "javascript", "javascriptreact",
  "typescript", "typescriptreact",
  "vue", "svelte",
}

M.TAILWIND_FT = {
  "html", "css",
  "javascript", "javascriptreact",
  "typescript", "typescriptreact",
  "vue", "svelte",
}

-- ── Spec helpers ───────────────────────────────────────────────────────────

--- Return a lazy.nvim optional spec that extends treesitter's ensure_installed
--- with the given parsers. Use in lang spec return tables:
---
---   return {
---     { ... },  -- primary plugin spec
---     shared.treesitter({ "go", "gomod", "gowork", "gosum" }),
---   }
---
---@param parsers string[]
---@return table  lazy.nvim plugin spec
function M.treesitter(parsers)
  return {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, parsers)
      end
    end,
  }
end

return M

