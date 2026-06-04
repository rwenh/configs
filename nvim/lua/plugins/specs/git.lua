-- lua/plugins/specs/git.lua — git tooling
--

return {

  -- ── gitsigns ───────────────────────────────────────────────────────────────
  {
    "lewis6991/gitsigns.nvim",
    event = { "BufReadPost", "BufNewFile" },
    opts  = {
      signs = { add = { text = "+" }, change = { text = "~" }, delete = { text = "_" },
                topdelete = { text = "‾" }, changedelete = { text = "~" } },
      current_line_blame = false,
      on_attach = function(bufnr)
        local gs  = require("gitsigns")
        local map = vim.keymap.set
        local function visual_lines() return { vim.fn.line("."), vim.fn.line("v") } end
        map("n", "]h", gs.next_hunk, { buffer = bufnr, desc = "Next hunk"    })
        map("n", "[h", gs.prev_hunk, { buffer = bufnr, desc = "Prev hunk"    })
        map("n", "<leader>.p", gs.preview_hunk, { buffer = bufnr, desc = "Preview hunk" })
        map("n", "<leader>.r", gs.reset_hunk,   { buffer = bufnr, desc = "Reset hunk"   })
        map("n", "<leader>.S", gs.stage_hunk,   { buffer = bufnr, desc = "Stage hunk"   })
        map("v", "<leader>.r", function() gs.reset_hunk(visual_lines()) end, { buffer = bufnr, desc = "Reset hunk (visual)" })
        map("v", "<leader>.S", function() gs.stage_hunk(visual_lines()) end, { buffer = bufnr, desc = "Stage hunk (visual)" })
      end,
    },
  },

  -- ── vim-fugitive ───────────────────────────────────────────────────────────
  {
    "tpope/vim-fugitive",
    cmd = { "Git", "Gdiffsplit", "Gread", "Gwrite", "GMove", "GDelete" },
  },

  -- ── LazyGit ────────────────────────────────────────────────────────────────
  { "kdheepak/lazygit.nvim", cmd = "LazyGit", dependencies = "nvim-lua/plenary.nvim" },

  -- ── Diffview ───────────────────────────────────────────────────────────────
  { "sindrets/diffview.nvim", cmd = { "DiffviewOpen", "DiffviewFileHistory" }, opts = {} },

  -- ── Neogit ─────────────────────────────────────────────────────────────────
  {
    "NeogitOrg/neogit",
    cmd          = "Neogit",
    dependencies = { "nvim-lua/plenary.nvim", "nvim-telescope/telescope.nvim", "sindrets/diffview.nvim" },
    keys = {
      { "<leader>.N", "<cmd>Neogit<cr>",        desc = "Neogit UI"  },
      { "<leader>.C", "<cmd>Neogit commit<cr>", desc = "Git commit" },
    },
    opts = {
      disable_line_numbers = false, auto_refresh = true, auto_show_console = false,
      integrations = { diffview = true, telescope = true },
      signs = { hunk = { "", "" }, item = { ">", "v" }, section = { ">", "v" } },
    },
    config = function(_, opts) pcall(function() require("neogit").setup(opts) end) end,
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
      default_mappings = false, disable_diagnostics = false,
      highlights = { incoming = "DiffAdd", current = "DiffText", ancestor = "DiffChange" },
    },
    config = function(_, opts) pcall(function() require("git-conflict").setup(opts) end) end,
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
    cmd          = "Octo",
    dependencies = { "nvim-lua/plenary.nvim", "nvim-telescope/telescope.nvim", "nvim-tree/nvim-web-devicons" },
    keys = {
      { "<leader>.oi", "<cmd>Octo issue list<cr>",   desc = "GitHub issues"   },
      { "<leader>.op", "<cmd>Octo pr list<cr>",      desc = "GitHub PRs"      },
      { "<leader>.or", "<cmd>Octo review start<cr>", desc = "Start PR review" },
      { "<leader>.oc", "<cmd>Octo pr checkout<cr>",  desc = "Checkout PR"     },
    },
    opts = {
      use_local_fs = false, default_remote = { "upstream", "origin" },
      picker = "telescope", timeout = vim.g.octo_timeout_ms or 10000,
      comment_icon = "▎", file_panel = { size = 10, use_icons = true },
    },
    config = function(_, opts) pcall(function() require("octo").setup(opts) end) end,
  },

  -- ── blame.nvim ─────────────────────────────────────────────────────────────
  {
    "FabijanZulj/blame.nvim",
    cmd  = "BlameToggle",
    keys = { { "<leader>.B", "<cmd>BlameToggle<cr>", desc = "Toggle git blame" } },
    opts = { date_format = "%Y-%m-%d", virtual_style = "right_align", focus_blame = true,
             merge_consecutive = false, max_summary_width = 30 },
  },

  -- ── Stash management ───────────────────────────────────────────────────────
  --
  -- Lightweight stash keymaps via vim-fugitive shell commands.
  -- Full stash browser available via LazyGit (<leader>.g).
  {
    "tpope/vim-fugitive",
    keys = {
      {
        "<leader>.zz",
        function()
          vim.ui.input({ prompt = "Stash message (optional): " }, function(msg)
            local cmd = (msg and msg ~= "") and ("Git stash push -m " .. vim.fn.shellescape(msg))
                        or "Git stash push"
            pcall(vim.cmd, cmd)
          end)
        end,
        desc = "Git stash push",
      },
      { "<leader>.zp", "<cmd>Git stash pop<cr>",           desc = "Git stash pop"          },
      { "<leader>.zl", "<cmd>Telescope git_stash<cr>",     desc = "Git stash list (Telescope)" },
      {
        "<leader>.zh",
        function()
          local file = vim.fn.expand("%:p")
          if file == "" then
            vim.notify("[git] no file in current buffer", vim.log.levels.WARN)
            return
          end
          pcall(vim.cmd, "DiffviewFileHistory --follow " .. vim.fn.fnameescape(file))
        end,
        desc = "Git file history --follow (rename-tracking)",
      },
    },
  },

  -- ── Worktree support ───────────────────────────────────────────────────────
  --
  --   <leader>.wl  list / switch worktrees
  --   <leader>.wc  create a new worktree
  {
    "ThePrimeagen/git-worktree.nvim",
    dependencies = { "nvim-telescope/telescope.nvim" },
    event        = "VeryLazy",
    config = function()
      local ok, wt = pcall(require, "git-worktree")
      if not ok then return end
      pcall(function() wt.setup() end)
      -- Register Telescope extension.
      local ok_t, telescope = pcall(require, "telescope")
      if ok_t then pcall(function() telescope.load_extension("git_worktree") end) end
    end,
    keys = {
      {
        "<leader>.wl",
        function()
          local ok, telescope = pcall(require, "telescope")
          if ok then
            pcall(function() telescope.extensions.git_worktree.git_worktrees() end)
          end
        end,
        desc = "Git worktree list / switch",
      },
      {
        "<leader>.wc",
        function()
          local ok, telescope = pcall(require, "telescope")
          if ok then
            pcall(function() telescope.extensions.git_worktree.create_git_worktree() end)
          end
        end,
        desc = "Git worktree create",
      },
    },
  },
}
