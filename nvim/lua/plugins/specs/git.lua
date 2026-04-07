-- lua/plugins/specs/git.lua
--
-- FIX (v2.2.2):
--   • octo.nvim added — GitHub PR review, issue tracking, and comment threads
--     directly in Neovim. LazyGit handles local git well but has no PR/review
--     support. octo.nvim fills that gap.

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
        local gs  = require("gitsigns")
        local map = vim.keymap.set

        map("n", "]h", gs.next_hunk, { buffer = bufnr, desc = "Next hunk" })
        map("n", "[h", gs.prev_hunk, { buffer = bufnr, desc = "Prev hunk" })
        map("n", "<leader>.p", gs.preview_hunk, { buffer = bufnr, desc = "Preview hunk" })
        map("n", "<leader>.r", gs.reset_hunk,   { buffer = bufnr, desc = "Reset hunk" })
        map("n", "<leader>.S", gs.stage_hunk,   { buffer = bufnr, desc = "Stage hunk" })

        map("v", "<leader>.r", function()
          gs.reset_hunk({ vim.fn.line("."), vim.fn.line("v") })
        end, { buffer = bufnr, desc = "Reset hunk (visual)" })
        map("v", "<leader>.S", function()
          gs.stage_hunk({ vim.fn.line("."), vim.fn.line("v") })
        end, { buffer = bufnr, desc = "Stage hunk (visual)" })
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

  {
    "NeogitOrg/neogit",
    cmd          = "Neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
      "sindrets/diffview.nvim",
    },
    keys = {
      { "<leader>.N", "<cmd>Neogit<cr>",        desc = "Neogit UI" },
      { "<leader>.C", "<cmd>Neogit commit<cr>", desc = "Git commit" },
    },
    opts = {
      disable_line_numbers = false,
      auto_refresh         = true,
      auto_show_console    = false,
      integrations         = { diffview = true, telescope = true },
      signs = {
        hunk    = { "", "" },
        item    = { ">", "v" },
        section = { ">", "v" },
      },
    },
    config = function(_, opts) require("neogit").setup(opts) end,
  },

  {
    "akinsho/git-conflict.nvim",
    event = "VeryLazy",
    keys = {
      { "<leader>gco", "<cmd>GitConflictChooseOurs<cr>",   desc = "Choose ours" },
      { "<leader>gct", "<cmd>GitConflictChooseTheirs<cr>", desc = "Choose theirs" },
      { "<leader>gcb", "<cmd>GitConflictChooseBoth<cr>",   desc = "Choose both" },
      { "<leader>gc0", "<cmd>GitConflictChooseNone<cr>",   desc = "Choose neither" },
    },
    opts = {
      default_mappings    = false,
      disable_diagnostics = false,
      highlights = {
        incoming = "DiffAdd",
        current  = "DiffText",
        ancestor = "DiffChange",
      },
    },
    config = function(_, opts) require("git-conflict").setup(opts) end,
  },

  {
    "junegunn/gv.vim",
    cmd          = "GV",
    dependencies = "tpope/vim-fugitive",
    keys         = { { "<leader>.v", "<cmd>GV<cr>", desc = "Git history" } },
  },

  -- GitHub PR / issue / review workflow
  {
    "pwntester/octo.nvim",
    cmd  = "Octo",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
      "nvim-tree/nvim-web-devicons",
    },
    keys = {
      { "<leader>.oi", "<cmd>Octo issue list<cr>",   desc = "GitHub issues" },
      { "<leader>.op", "<cmd>Octo pr list<cr>",      desc = "GitHub PRs" },
      { "<leader>.or", "<cmd>Octo review start<cr>", desc = "Start PR review" },
      { "<leader>.oc", "<cmd>Octo pr checkout<cr>",  desc = "Checkout PR" },
    },
    opts = {
      use_local_fs = false,
      default_remote = { "upstream", "origin" },
      ssh_aliases    = {},
      picker         = "telescope",
      picker_config  = {
        use_emojis = false,
        mappings   = {
          open_in_browser     = { lhs = "<C-b>", desc = "open issue in browser" },
          copy_url            = { lhs = "<C-y>", desc = "copy url to clipboard" },
          checkout_pr         = { lhs = "<C-o>", desc = "checkout pull request" },
          merge_pr            = { lhs = "<C-r>", desc = "merge pull request" },
        },
      },
      comment_icon        = "▎",
      outdated_icon       = "󰅒 ",
      resolved_icon       = " ",
      reaction_viewer_hint_icon = " ",
      user_icon           = " ",
      timeline_marker     = " ",
      timeline_indent     = "2",
      right_bubble_delimiter  = "",
      left_bubble_delimiter   = "",
      github_hostname         = "",
      snippet_context_lines   = 4,
      gh_env              = {},
      timeout             = 5000,
      ui = {
        use_signcolumn = true,
      },
      issues = {
        order_by = { field = "CREATED_AT", direction = "DESC" },
      },
      pull_requests = {
        order_by    = { field = "CREATED_AT", direction = "DESC" },
        always_select_remote_on_create = false,
      },
      file_panel = {
        size    = 10,
        use_icons = true,
      },
    },
    config = function(_, opts)
      pcall(function() require("octo").setup(opts) end)
    end,
  },
}
