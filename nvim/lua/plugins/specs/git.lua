-- lua/plugins/specs/git.lua - Git integration

return {
  {
    "lewis6991/gitsigns.nvim",
    event = { "BufReadPost", "BufNewFile" },
    opts = {
      signs = {
        add          = { text = "+" },
        change       = { text = "~" },
        delete       = { text = "_" },
        topdelete    = { text = "‾" },
        changedelete = { text = "~" },
      },
      current_line_blame = false,
      on_attach = function(bufnr)
        -- RECALIBRATION: Require freshly on attach (not via package.loaded)
        local gs  = require("gitsigns")
        local map = vim.keymap.set

        map("n", "]h", gs.next_hunk, { buffer = bufnr, desc = "Next hunk" })
        map("n", "[h", gs.prev_hunk, { buffer = bufnr, desc = "Prev hunk" })

        map("n", "<leader>.p", gs.preview_hunk, { buffer = bufnr, desc = "Preview hunk" })
        map("n", "<leader>.r", gs.reset_hunk,   { buffer = bufnr, desc = "Reset hunk" })
        map("n", "<leader>.S", gs.stage_hunk,   { buffer = bufnr, desc = "Stage hunk" })
      end,
    },
  },

  {
    "tpope/vim-fugitive",
    cmd = { "Git", "Gdiffsplit", "Gread", "Gwrite", "GMove", "GDelete" },
  },

  {
    "kdheepak/lazygit.nvim",
    cmd          = "LazyGit",
    dependencies = "nvim-lua/plenary.nvim",
  },

  {
    "sindrets/diffview.nvim",
    cmd  = { "DiffviewOpen", "DiffviewFileHistory" },
    opts = {},
  },
}
