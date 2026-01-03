-- lua/plugins/specs/lang/web.lua - Web development (HTML/CSS/JS/TS)

return {
  {
    "windwp/nvim-ts-autotag",
    ft = { "html", "xml", "javascript", "typescript", "jsx", "tsx" },
    opts = {},
  },

  {
    "mattn/emmet-vim",
    ft = { "html", "css", "javascript", "jsx" },
    init = function()
      vim.g.user_emmet_leader_key = "<C-e>"
    end,
  },
}