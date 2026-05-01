-- lua/plugins/specs/lang/web.lua — shared web tooling
--
-- IMPORTANT: this spec is imported before html.lua and css.lua in specs/init.lua.
-- autotag and emmet must be available when HTML/CSS configs run.
-- If this file fails to load, html.lua and css.lua lose autotag + emmet.
--

local shared = require("plugins.specs.lang.shared")

vim.g.user_emmet_leader_key = "<C-e>"

return {
  -- ── Auto-close HTML/JSX tags ───────────────────────────────────────────────
  {
    "windwp/nvim-ts-autotag",
    ft = shared.WEB_FT,
    opts = {
      opts = {
        enable_close          = true,
        enable_rename         = true,
        enable_close_on_slash = false,
      },
      per_filetype = {},
    },
    config = function(_, opts)
      pcall(function() require("nvim-ts-autotag").setup(opts) end)
    end,
  },

  -- ── Emmet ──────────────────────────────────────────────────────────────────

  {
    "mattn/emmet-vim",
    ft = shared.WEB_FT,
  },
}
