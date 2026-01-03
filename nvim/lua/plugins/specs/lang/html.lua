-- lua/plugins/specs/lang/html.lua - HTML development

return {
  {
    "windwp/nvim-ts-autotag",
    ft = { "html", "xml", "jsx", "tsx" },
    opts = {},
  },

  {
    "mattn/emmet-vim",
    ft = { "html", "css" },
    init = function()
      vim.g.user_emmet_leader_key = "<C-e>"
    end,
  },
}