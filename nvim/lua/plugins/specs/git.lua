-- lua/plugins/specs/git.lua - Git integration

return {
  {
    "lewis6991/gitsigns.nvim",
    -- FIX #4: Changed BufReadPre → BufReadPost. Gitsigns needs buffer content
    -- to compute diff state; BufReadPre fires before content is loaded.
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
        -- FIX #1: Use require() instead of package.loaded.gitsigns.
        -- package.loaded is cleared on :Lazy reload, silently making gs = nil
        -- and breaking all hunk keymaps in already-attached buffers.
        local gs  = require("gitsigns")
        local map = vim.keymap.set

        -- Hunk navigation (buffer-local; global ]h/[h in keymaps.lua are
        -- superseded by these in git buffers — the buffer-local form is correct)
        map("n", "]h", gs.next_hunk, { buffer = bufnr, desc = "Next hunk" })
        map("n", "[h", gs.prev_hunk, { buffer = bufnr, desc = "Prev hunk" })

        -- FIX #2: Align keymap prefixes with KEYMAP_REFERENCE.md and keymaps.lua.
        -- Original used <leader>g* which contradicts the documented <leader>. namespace
        -- and would silently override the global <leader>.p/.r bindings in git buffers.
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