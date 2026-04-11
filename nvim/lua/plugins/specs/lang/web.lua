-- lua/plugins/specs/lang/web.lua - Web development
--
-- FIX (v2.2.3):
--   • nvim-ts-autotag v0.4+ changed its API: calling setup() with no opts
--     results in all features (close, rename, replace) being disabled by
--     default. Explicit opts table added to enable all features.
--     Ref: https://github.com/windwp/nvim-ts-autotag#setup
--
-- FIX (v2.3.1):
--   • Confirmed: tailwind-tools.nvim entry fully removed. css.lua is the
--     sole owner (server.override=false). Any remnant spec here would create
--     a duplicate lazy registration that silently drops css.lua's opts.
--   • colorizer is owned by advanced.lua (NvChad/nvim-colorizer.lua).
--     Neither plugin is referenced here.

return {
  {
    "windwp/nvim-ts-autotag",
    ft = {
      "html", "javascript", "javascriptreact",
      "typescriptreact", "jsx", "tsx", "vue", "svelte",
    },
    -- FIX: explicit opts — v0.4+ defaults all features to disabled without them
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

  {
    "mattn/emmet-vim",
    ft = {
      "html", "css", "javascript", "javascriptreact",
      "typescriptreact", "vue", "svelte",
    },
    init = function()
      vim.g.user_emmet_leader_key = "<C-e>"
    end,
  },
}
