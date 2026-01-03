-- lua/plugins/specs/git.lua - Git integration

return {
  {
    "lewis6991/gitsigns.nvim",
    event = { "BufReadPre", "BufNewFile" },
    opts = {
      signs = {
        add = { text = "+" },
        change = { text = "~" },
        delete = { text = "_" },
        topdelete = { text = "‾" },
        changedelete = { text = "~" },
      },
      current_line_blame = false,
    },
  },

  {
    "tpope/vim-fugitive",
    cmd = { "Git", "Gdiffsplit" },
  },

  {
    "kdheepak/lazygit.nvim",
    cmd = "LazyGit",
    dependencies = "nvim-lua/plenary.nvim",
  },
}
