-- lua/plugins/specs/git.lua
--
-- Architecture:
--   gitsigns   — inline hunk signs, buffer-local hunk navigation/staging
--   vim-fugitive — low-level Git commands (:Gdiffsplit, :Gread, etc.)
--   lazygit    — full-screen interactive Git TUI
--   diffview   — side-by-side diff + file history viewer
--   neogit     — Magit-style commit/rebase/branch UI (uses diffview + telescope)
--   git-conflict — 3-way conflict resolution with choose-ours/theirs/both
--   gv.vim     — commit graph browser (lightweight; uses fugitive)
--   octo.nvim  — GitHub PR / issue / review workflow directly in Neovim
--   blame.nvim — per-line git blame virtual text (key in hud.lua)
--
-- NOTE: blame.nvim is specced in hud.lua for historical reasons; its <leader>.B
-- key is documented here for discoverability.
--

return {

  -- ── gitsigns ───────────────────────────────────────────────────────────────
  {
    "lewis6991/gitsigns.nvim",
    event = { "BufReadPost", "BufNewFile" },
    opts  = {
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

        local function visual_lines()
          return { vim.fn.line("."), vim.fn.line("v") }
        end

        map("n", "]h", gs.next_hunk, { buffer = bufnr, desc = "Next hunk"    })
        map("n", "[h", gs.prev_hunk, { buffer = bufnr, desc = "Prev hunk"    })
        map("n", "<leader>.p", gs.preview_hunk, { buffer = bufnr, desc = "Preview hunk" })
        map("n", "<leader>.r", gs.reset_hunk,   { buffer = bufnr, desc = "Reset hunk"   })
        map("n", "<leader>.S", gs.stage_hunk,   { buffer = bufnr, desc = "Stage hunk"   })

        map("v", "<leader>.r", function()
          gs.reset_hunk(visual_lines())
        end, { buffer = bufnr, desc = "Reset hunk (visual)" })

        map("v", "<leader>.S", function()
          gs.stage_hunk(visual_lines())
        end, { buffer = bufnr, desc = "Stage hunk (visual)" })
      end,
    },
  },

  -- ── vim-fugitive ───────────────────────────────────────────────────────────
  {
    "tpope/vim-fugitive",
    cmd = { "Git", "Gdiffsplit", "Gread", "Gwrite", "GMove", "GDelete" },
  },

  -- ── LazyGit ────────────────────────────────────────────────────────────────
  {
    "kdheepak/lazygit.nvim",
    cmd          = "LazyGit",
    dependencies = "nvim-lua/plenary.nvim",
  },

  -- ── Diffview ───────────────────────────────────────────────────────────────

  {
    "sindrets/diffview.nvim",
    cmd  = { "DiffviewOpen", "DiffviewFileHistory" },
    opts = {},
  },

  -- ── Neogit ─────────────────────────────────────────────────────────────────
  {
    "NeogitOrg/neogit",
    cmd          = "Neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
      "sindrets/diffview.nvim",
    },
    keys = {
      { "<leader>.N", "<cmd>Neogit<cr>",        desc = "Neogit UI"    },
      { "<leader>.C", "<cmd>Neogit commit<cr>", desc = "Git commit"   },
    },
    opts = {
      disable_line_numbers = false,
      auto_refresh         = true,
      auto_show_console    = false,
      integrations = { diffview = true, telescope = true },
      signs = {
        hunk    = { "", "" },
        item    = { ">", "v" },
        section = { ">", "v" },
      },
    },
    config = function(_, opts)
      pcall(function() require("neogit").setup(opts) end)
    end,
  },

  -- ── git-conflict ───────────────────────────────────────────────────────────
  {
    "akinsho/git-conflict.nvim",
    event = "VeryLazy",
    keys  = {
      { "<leader>gco", "<cmd>GitConflictChooseOurs<cr>",   desc = "Choose ours"    },
      { "<leader>gct", "<cmd>GitConflictChooseTheirs<cr>", desc = "Choose theirs"  },
      { "<leader>gcb", "<cmd>GitConflictChooseBoth<cr>",   desc = "Choose both"    },
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
    config = function(_, opts)
      pcall(function() require("git-conflict").setup(opts) end)
    end,
  },

  -- ── gv.vim ─────────────────────────────────────────────────────────────────
  {
    "junegunn/gv.vim",
    cmd          = "GV",
    dependencies = "tpope/vim-fugitive",
    keys         = { { "<leader>.v", "<cmd>GV<cr>", desc = "Git history (graph)" } },
  },

  -- ── octo.nvim ──────────────────────────────────────────────────────────────

  {
    "pwntester/octo.nvim",
    cmd  = "Octo",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
      "nvim-tree/nvim-web-devicons",
    },
    keys = {
      { "<leader>.oi", "<cmd>Octo issue list<cr>",   desc = "GitHub issues"    },
      { "<leader>.op", "<cmd>Octo pr list<cr>",      desc = "GitHub PRs"       },
      { "<leader>.or", "<cmd>Octo review start<cr>", desc = "Start PR review"  },
      { "<leader>.oc", "<cmd>Octo pr checkout<cr>",  desc = "Checkout PR"      },
    },
    opts = {
      use_local_fs    = false,
      default_remote  = { "upstream", "origin" },
      picker         = "telescope",
      timeout        = vim.g.octo_timeout_ms or 10000,
      comment_icon   = "▎",
      file_panel     = { size = 10, use_icons = true },
      picker_config  = {
        use_emojis = false,
        mappings   = {
          open_in_browser = { lhs = "<C-b>", desc = "open in browser"     },
          copy_url        = { lhs = "<C-y>", desc = "copy url to clipboard" },
          checkout_pr     = { lhs = "<C-o>", desc = "checkout pull request" },
          merge_pr        = { lhs = "<C-r>", desc = "merge pull request"   },
        },
      },
    },
    config = function(_, opts)
      pcall(function() require("octo").setup(opts) end)
    end,
  },
}
