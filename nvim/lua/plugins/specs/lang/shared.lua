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

---@param parsers string[]
---@return table  lazy.nvim plugin spec
function M.treesitter(parsers)
  return {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        local seen = {}
        for _, p in ipairs(opts.ensure_installed) do seen[p] = true end
        for _, p in ipairs(parsers) do
          if not seen[p] then
            table.insert(opts.ensure_installed, p)
            seen[p] = true
          end
        end
      end
    end,
  }
end

return M

