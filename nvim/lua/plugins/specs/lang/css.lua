-- lua/plugins/specs/lang/css.lua - CSS development

return {
  {
    "windwp/nvim-ts-autotag",
    ft = "css",
    opts = {},
  },

  {
    "mattn/emmet-vim",
    ft = "css",
    init = function()
      vim.g.user_emmet_leader_key = "<C-e>"
    end,
  },
}