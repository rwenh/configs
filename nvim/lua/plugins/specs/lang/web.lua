-- lua/plugins/specs/lang/web.lua — shared web tooling
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
      local ok, err = pcall(function() require("nvim-ts-autotag").setup(opts) end)
      if not ok then
        vim.notify(
          "[web] nvim-ts-autotag setup failed: " .. tostring(err)
          .. "\nRun :Lazy update nvim-ts-autotag",
          vim.log.levels.WARN
        )
      end
    end,
  },

  -- ── Emmet ──────────────────────────────────────────────────────────────────

  {
    "mattn/emmet-vim",
    ft = shared.WEB_FT,
  },
}
